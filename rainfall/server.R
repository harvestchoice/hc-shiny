#####################################################################################
# Title: Testing CRU Time Series with leaflet
# Date: December 2014
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

library(shiny)
library(data.table)
library(reshape2)
library(raster)
library(rgdal)
library(leaflet)
library(dygraphs)

setwd("/home/projects/shiny/tmp")


# CRU and PDSI variables
d <- c("cld", "dtr", "frs", "pet", "pre", "tmn", "tmp", "tmx", "vap", "wet", "pdsi")
names(d) <- c("Cloud Cover (%)", "dtr", "frs", "pet", "Precipitation (mm)", "tmn", "Temperature (C)", "tmx", "vap", "wet", "Palmer Drough Severity Index (-25, 25)")

# Load CRU 3.22 time series (from 1901 onwards)
#tmp <- brick("../rainfall/data/cru_ts3.22.1901.2013.tmp.dat.nc")
#cld <- brick("../rainfall/data/cru_ts3.22.1901.2013.cld.dat.nc")
pre <- brick("../rainfall/data/cru_ts3.22.1901.2013.pre.dat.nc")
tm.cru <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
col.cru <- rev(c("#2F6FBF", "#69DB4D", "#F9EF58", "#DC5207", "#830000"))

# Load PDSI (from 1850 onwards)
pdsi <- brick("../rainfall/data/pdsisc.monthly.maps.1850-2012.nc")
tm.pdsi <- seq(as.Date("1850-01-01"), as.Date("2012-12-31"), "month")
col.pdsi <- c("#FF9900", "#FFFF66", "#FFFFFF", "#99FF99", "#009900")

# Load GAUL district boundaries (webified version)
load("../../cell5m/rdb/g2.web.rda")
g2.dt <- data.table(g2.web@data)[, .N, by=list(ADM0_CODE, ADM0_NAME)]
setkey(g2.dt, ADM0_NAME)

# Well-formatted country/province/district list to populate controls
load("../../cell5m/rdb/g2.list.rda")

# Zip rasters for download
writeRasterZip <- function(x, var, file, ...) {
  writeRaster(x, file, bylayer=F, overwrite=T, ...)
  f <- list.files(pattern=paste0(strsplit(file, ".", fixed=T)[[1]][1], ".*"))
  zip(paste0(file, ".zip"), f, flags="-9Xjm", zip="zip")
  return(TRUE)
}


shinyServer(function(input, output, session) {

  # Create the map
  map <- createLeafletMap(session, "map")

  # Create dynamic controls
  output$selectCRU <- renderUI({
    selectInput("selectCRU", "Choose a Variable", d[c(5,11)], selected="pre")
  })

  output$selectg0 <- renderUI({
    selectInput("selectg0", "Choose a Country", names(g2.list), selected="Ghana")
  })

  output$selectg2 <- renderUI({
    selectizeInput("selectg2", "Limit to District", choices=c("Entire Country", g2.list[[input$selectg0]]), selected="Entire Country")
  })

  output$chartMsg <- renderText({
    if (input$btn==0) return()
    out <- div(
      h3(names(d)[d==cru()], br(), tags$small(tags$mark(dist()), ", ", cntr(), " - Period: ", paste0(format(range(r.tm()), "%b %Y"), collapse=" - "))),
      p("The long-term mean is over the selected months only (or the entire year if no month is selected. The trend component is generated through classical seasonal decomposition by moving averages over the entire selected period."))
    return(as.character(out))
  })

  cru <- reactive({
    if (input$btn>0) isolate(input$selectCRU) else "pre"
  })

  cntr <- reactive({
    if (input$btn>0) isolate(input$selectg0) else "Ghana"
  })

  dist <- reactive({
    if (input$btn>0) input$selectg2 else "Entire Country"
  })

  r.tm <- reactive({
    if (input$btn==0) return(seq(as.Date("1960-01-01"), as.Date("2013-12-31"), "month"))
    seq(as.Date(paste0(input$rg[1], "-01-01")), as.Date(paste0(input$rg[2], "-12-31")), "month")
  })

  g <- reactive({
    # Selected country boundaries
    if (input$btn==0) return()
    g2.web[g2.web$ADM0_NAME==cntr(),]
  })

  stats <- reactive({
    if (input$btn==0) return()
    # Crop raster to selected country extent (only useful to generate rasters for download)
    r <- crop(get(cru()), g())
    # Define z dimension (time) to CRU layers
    tm <- if(cru()=="pdsi") tm.pdsi else tm.cru
    r <- setZ(r, tm, "month")
    r <- mean(r, na.rm=T)
    return(r)
  })

  stats.dt1 <- reactive({
    if (input$btn==0) return()
    isolate({
      # All district X month records have been pre-processed to .rds files for added speed (see `data.R`)
      gCode <- g2.dt[cntr()][, ADM0_CODE]
      dt <- readRDS(paste0("../rainfall/data/rds/", cru(), gCode, ".rds"))
      return(dt)
    })
  })

  stats.dt2 <- reactive({
    if (input$btn==0) return()
    sdist <- dist()
    dt <- stats.dt1()

    if (sdist=="Entire Country") {
      # Collapse to entire country
      dt <- dt[, list(
        ADM1_NAME=sdist,
        ADM2_NAME=sdist,
        ADM2_CODE=0, ID=0,
        value=mean(value, na.rm=T)), by=month]
    } else {
      # Limit to selected district
      dt <- dt[ADM2_NAME==sdist]
    }

    # Add trend
    dt <- dt[order(month)]
    dt.ts <- ts(dt$value, start=c(dt[, min(year(month))], 1), frequency=12)
    dt.ts <- decompose(zoo::na.StructTS(dt.ts))
    dt.ts <- data.table(trend=dt.ts$trend, seasonal=dt.ts$seasonal)
    dt <- cbind(dt, dt.ts)

    # Limit to selected period and month
    dt <- dt[month %between% range(r.tm())]
    if (input$selectMonth>0) dt <- dt[which(month(month)==input$selectMonth)]

    # Compute period stats
    dt[, mean := mean(value, na.rm=T)]
    dt[, sd := sd(value, na.rm=T)]
    dt[, mad := mad(value, na.rm=T)]
    return(dt)
  })


  output$dygraph <- renderDygraph({
    if (input$btn==0) return()
    dt <- stats.dt2()
    # Convert data.table to xts for use with TS chart
    dygraph(xts::as.xts(dt[, list(value, mean, trend)], order.by=dt$month)) %>%
      dySeries("value", label=cru()) %>%
      dySeries("mean", label="period mean") %>%
      dySeries("trend", label="trend", fillGraph=F, strokeWidth=2, strokePattern="dashed") %>%
      dyOptions(fillGraph=T, fillAlpha=0.4,
        colors=if(cru()=="pdsi") c("#FF9900", "#99FF99", "#009900") else c("#53B376", "#DD5A0B", "#2F6FBF")) %>%
      dyLegend(show="always", hideOnMouseOut=F, labelsSeparateLines=T, width=140) %>%
      dyRangeSelector(height=20)
  })


  output$saveData <- downloadHandler(function() {
    f <- paste0(cntr(), "-", cru())
    if(input$fileType %in% c("csv", "dta")) {
      # Complete file path
      f <- paste0(f, "-", dist(), "-", paste(range(r.tm()), collapse="-"), ".", input$fileType)
    } else {
      # Raster file path with `.zip`
      f <- paste0(f, ".", input$fileType, ".zip")
    }
    return(f)
  }, function(file) {
    r <- stats()
    dt <- stats.dt2()
    switch(input$fileType,
      grd = writeRasterZip(r, gsub(".zip", "", file, fixed=T), "raster"),
      tif = writeRasterZip(r, gsub(".zip", "", file, fixed=T), "GTiff", options="INTERLEAVE=BAND"),
      nc = writeRasterZip(r, gsub(".zip", "", file, fixed=T), "CDF"),
      csv = write.csv(dt, file, row.names=F, na=""),
      dta = foreign::write.dta(dt, file, version=9L)
    )
  })


  observe({
    if (input$btn>0) {
      map$clearGeoJSON()
      # Load country GeoJSON (pre-processed in `data.R`)
      gCode <- g2.dt[cntr()][, ADM0_CODE]
      m <<- readRDS(paste0("../rainfall/data/rds/", cru(), gCode, ".json.rds"))
      # Center map to selected country centroid
      coords <- apply(coordinates(g()), 2, mean, na.rm=T)
      map$setView(coords[2], coords[1]+5, 6)
      # Draw polygons one by one
      for (i in m$features) map$addGeoJSON(i, i$id)
    }
  })


  observe({
    if (dist()=="Entire Country") return()
    # Highlight selected polygon
    i <- which(sapply(m$features, function(x) x$properties$ADM2_NAME==dist()))
    m$features <- m$features[i]
    m$features[[1]]$style <- list(fillColor="gray", weight=.6, color="white", fillOpacity=0.7)
    map$addGeoJSON(m, 0)
  })


  # Map mouseover, show side panel
  output$details <- renderText({
    evt <- input$map_geojson_mouseover
    if (is.null(evt)) {
      out <- div(h3(cntr()), p("Mouse over districts to view details."))
    } else {
      out <- div(
        h3(cntr(), br(), tags$small(evt$properties$ADM2_NAME, ", ", evt$properties$ADM1_NAME)),
        hr(),
        h4("1960-2013 ", names(d)[d==cru()]),
        p("Mean: ", strong(evt$properties$mean), br(),
          "Min: ", strong(evt$properties$min), br(),
          "Max: ", strong(evt$properties$max), br(),
          "Sd. Dev.: ", strong(evt$properties$sd)),
        p(em("Click to select this district.")))
    }
    return(as.character(out))
  })

  # Map click, update district
  observe({
    evt <- input$map_geojson_click
    if (is.null(evt)) return()
    updateSelectInput(session, "selectg2", selected=evt$properties$ADM2_NAME)
  })


#   # Rank districts on barchart
#   output$barPlot <- renderPlot({
#     if(input$btn==0) return()
#     dt <- stats.dt1()
#     dt <- dt[, list(value=mean(value, na.rm=T)), by=list(ADM2_NAME)]
#     dt <- dt[order(value, decreasing=T)]
#     dt <- rbind(head(dt,4), data.table(ADM2_NAME="[ ... ]", value=0), tail(dt,4))
#     par(las=1, mar=c(3.1,5,0,0.1), fg="#444444" )
#     barplot(dt$value, names.arg=dt$ADM2_NAME, horiz=T,
#       col="#2F6FBF", border="white", xlab=NULL, ylab=NULL, main=NULL)
#   })
#
#
#   # Show country spplot
#   output$rasterPlot <- renderPlot({
#     if(input$btn==0) return()
#     spplot(stats(), col.regions=colorRampPalette(if(cru()=="pdsi") col.pdsi else col.cru)(20))
#     #plot(g(), add=T)
#   })

})
