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
library(xts)
library(dygraphs)

setwd("/home/projects/shiny/tmp")

# CRU variables
d <- c("cld", "dtr", "frs", "pet", "pre", "tmn", "tmp", "tmx", "vap", "wet")
names(d) <- c("Cloud Cover (%)", "dtr", "frs", "pet", "Precipitation (mm)", "tmn", "Temperature (C)", "tmx", "vap", "wet")

# CRU 3.22 time series
#tmp <- brick("../rainfall/data/cru_ts3.22.1901.2013.tmp.dat.nc")
#cld <- brick("../rainfall/data/cru_ts3.22.1901.2013.cld.dat.nc")
pre <- brick("../rainfall/data/cru_ts3.22.1901.2013.pre.dat.nc")
tm <- seq(as.Date("1901-01-15"), as.Date("2013-12-16"), "month")

# GAUL district boundaries (simplified)
load("../../cell5m/rdb/g2.web.rda")
g2.dt <- data.table(g2.web@data)[, .N, by=list(ADM0_CODE, ADM0_NAME)]
setkey(g2.dt, ADM0_NAME)

# Well-formatted country/province/district list to populate controls
load("../../cell5m/rdb/g2.list.rda")


shinyServer(function(input, output, session) {

  # Create the map
  map <- createLeafletMap(session, "map")

  # Create dynamic controls
  output$selectCRU <- renderUI({
    selectInput("selectCRU", "Choose a CRU Variable", d[c(5)], selected="pre")
  })

  output$selectg0 <- renderUI({
    selectInput("selectg0", "Choose a Country", names(g2.list), selected="Ghana")
  })

  output$selectg2 <- renderUI({
    selectizeInput("selectg2", "Choose a District", choices=g2.list[[input$selectg0]], selected="Adansi East")
  })

  output$waitMsg <- reactive({
    if (input$btn>0) {
      paste0("Monthly ", names(d)[d==cru()], " (", dist(), ", ", cntr(), ") - Period: ",
        paste0(format(range(r.tm()), "%Y-%m"), collapse=" - "))
    } else {
      paste0("Select a district, click 'Show Series' and wait a few seconds for the time-series graph and map to render.")
    }
  })

  output$dygraph <-  renderDygraph({
    if (input$btn==0) return()
    dt <- stats.dt3()
    # Convert data.table to xts for use with TS chart
    dygraph(as.xts(dt[, list(value, mean)], order.by=dt$month)) %>%
      dySeries("value", label=cru(), color="#41b6c4") %>%
      dySeries("mean", label="long-term mean", color="red") %>%
      dyOptions(fillGraph=T, fillAlpha=0.4) %>%
      dyLegend(show="always", hideOnMouseOut=F) %>%
      dyRangeSelector(height=20)
  })

  output$saveData <- downloadHandler(
    function() paste0(cntr(), "-", dist(), "-", cru(), "-", paste(range(r.tm()), collapse="-"), ".", input$fileType),
    function(file) {
      r <- stats()
      dt <- stats.dt3()[, .SD, .SDcols=-c(1:4)]
      switch(input$fileType,
        # Note that writeRaster() has an issue with download handler
        grd = writeRaster(r, file, "raster", bylayer=F, overwrite=T),
        tif = writeRaster(r, file, "GTiff", bylayer=F, options="INTERLEAVE=BAND", overwrite=T),
        nc = writeRaster(r, file, "CDF", bylayer=F, overwrite=T),
        img = writeRaster(r, file, "HFA", bylayer=F, overwrite=T),
        # These 2 formats work okay
        csv = write.csv(dt, file, row.names=F, na=""),
        dta = foreign::write.dta(dt, file, version=9L)
      )
    }
  )

  cru <- reactive({
    if (input$btn>0) isolate(input$selectCRU)
  })

  cntr <- reactive({
    if (input$btn>0) isolate(input$selectg0)
  })

  dist <- reactive({
    if (input$btn>0) isolate(input$selectg2)
  })

  g <- reactive({
    if (input$btn>0) isolate({
      m <<- jsonlite::fromJSON(paste0("../rainfall/data/json/g2web", g2.dt[cntr()][, ADM0_CODE]),
        simplifyVector=F)
      coords <- coordinates(g2.web[g2.web$ADM2_NAME==dist(),])
      map$setView(coords[2], coords[1], 6)
      return(g2.web[g2.web$ADM0_NAME==cntr(),])
    })
  })

  r.tm <- reactive({
    if (input$btn==0) return(tm)
    seq(as.Date(paste0(input$rg[1], "-01-01")), as.Date(paste0(input$rg[2], "-12-31")), "month")
  })

  stats <- reactive({
    if (input$btn>0) isolate({
      # Crop to selected country extent
      r <- crop(get(cru()), g())
      # Add z dimension (time)
      r <- setZ(r, tm, "month")
      return(r)
    })
  })

  stats.dt1 <- reactive({
    if (input$btn>0) isolate({
      # Summarize raster over districts and convert to data.table
      dt <- extract(stats(), g(), fun=mean, na.rm=T, df=T)
      dt <- cbind(g()@data[, c("ADM1_NAME", "ADM2_NAME", "ADM2_CODE")], dt)
      dt <- data.table(dt)
      dt <- melt(dt, id.vars=c("ADM1_NAME", "ADM2_NAME", "ADM2_CODE", "ID"), variable.name="month", variable.factor=F)
      dt[, month := as.Date(month, format="X%Y.%m.%d")]
      return(dt)
    })
  })

  stats.dt2 <- reactive({
    if (input$btn==0) return()
    dt <- stats.dt1()
    # Summarize all districts over entire period for mapping
    dt[, mean := mean(value, na.rm=T), by=list(ADM1_NAME, ADM2_NAME, ADM2_CODE)]
    return(dt)
  })

  stats.dt3 <- reactive({
    if (input$btn==0) return()
    dt <- stats.dt2()
    # Filter to district
    dt <- dt[ADM2_NAME==dist()]
    # Limit to period
    dt <- dt[month %between% range(r.tm())]
    if (input$selectMonth>0) dt <- dt[which(month(month)==input$selectMonth)]
    dt[, mean := mean(value, na.rm=T)]
    dt[, cv := cv(value, na.rm=T)]
    dt[, diff := value-mean]
    return(dt)
  })

  drawDistricts <- observe({
    if (input$btn>0) isolate({
      map$clearShapes()
      # Add symbology
      dt <- stats.dt2()
      dt <- dt[, .N, by=list(ADM1_NAME, ADM2_NAME, ADM2_CODE, mean)]
      setkey(dt, ADM2_CODE)
      dt <- dt[J(sapply(m$features, `[[`, "id"))]
      rg <- range(dt$mean, na.rm=T)
      cv <- classInt::classIntervals(dt$mean, n=5)$brks
      cl <- cut(dt$mean, unique(c(rg[1]-1, cv, rg[2]+1)), cutlabels=F, ordered_result=T)
      cl <- colorRampPalette(c("#a1dab4", "#41b6c4", "#2c7fb8"))(length(cv)+1)[cl]
      cl[dt[, which(ADM2_NAME==dist())]] <- "yellow"
      for (i in 1:length(m$features)) {
        m$features[[i]]$style <- list(fillColor=cl[i], weight=.6, color="white", fillOpacity=0.7)
      }
      # Draw polygons
      map$addGeoJSON(m)
    })
  })


  #   # When map is clicked, show a popup with layer info
  #   clickObs <- observe({
  #     map$clearPopups()
  #     evt <- input$map_geojson_click
  #     if (is.null(evt)) return()
  #     isolate({
  #       map$showPopup(evt$lat, evt$lng, paste(
  #         "Province: ", evt$properties$ADM1_NAME, "<br/>",
  #         "District: ", evt$properties$ADM2_NAME, "<br/>"))
  #     })
  #   })

})
