#####################################################################################
# Title: CRU and PDSI 1960-2013 Time Series across Districts
# Date: December 2014
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

library(shiny)
library(shinyBS)
library(data.table)
library(reshape2)
library(leaflet, lib.loc="/home/mbacou/R/x86_64-redhat-linux-gnu-library/3.1")
library(dygraphs)

setwd("/home/projects/shiny/tmp")

# CRU and PDSI variables
d <- c("cld", "dtr", "frs", "pet", "pre", "tmn", "tmp", "tmx", "vap", "wet", "pdsi")
names(d) <- c("Cloud Cover (%)", "dtr", "frs", "pet", "Precipitation (mm)", "tmn",
  "Temperature (C)", "tmx", "vap", "wet", "Palmer Drought Severity Index (-10, 10)")

# CRU 3.22 precipitation time series (from 1901 onwards)
path.pre <- "../rainfall/data/cru_ts3.22.1901.2013.tmp.dat.nc"
tm.pre <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
col.pre <- rev(c("#2F6FBF", "#69DB4D", "#F9EF58", "#DC5207", "#830000"))

# PDSI (from 1850 onwards)
path.pdsi <- "../rainfall/data/pdsisc.monthly.maps.1850-2012.nc"
tm.pdsi <- seq(as.Date("1850-01-01"), as.Date("2012-12-31"), "month")
col.pdsi <- c("#FF9900", "#FFFF66", "#FFFFFF", "#99FF99", "#009900")

# Load GAUL district boundaries (full version)
load("../../cell5m/rdb/g2.rda")
g2.dt <- data.table(g2@data)[, .N, by=list(ADM0_CODE, ADM0_NAME)]
setkey(g2.dt, ADM0_NAME)

# Load country/province/district list to populate controls
g2.list <- readRDS("../../cell5m/rdb/g2_2008v09.list.rds")


# Helper - Archive spatial formats for download
writeRasterZip <- function(x, file, filename, format, ...) {
  if (format=="ESRI Shapefile") {
    rgdal::writeOGR(x, "./", filename, format, overwrite_layer=T, check_exists=T)
  } else {
    writeRaster(x, filename, format, bylayer=F, overwrite=T, ...)
  }
  f <- list.files(pattern=paste0(strsplit(filename, ".", fixed=T)[[1]][1], ".*"))
  zip(paste0(filename, ".zip"), f, flags="-9Xjm", zip="zip")
  file.copy(paste0(filename, ".zip"), file)
  file.remove(paste0(filename, ".zip"))
}


# Helper - Generate time series stats
genStats <- function(dt, cntr, dist, tm, mth) {
  if (dist=="Entire Country") {
    # Collapse district records to entire country by month
    dt <- dt[, list(
        ADM0_CODE=g2.dt[ADM0_NAME==cntr, ADM0_CODE],
        ADM0_NAME=cntr,
        ADM1_CODE=0,
        ADM1_NAME="Entire Country",
        ADM2_CODE=0,
        ADM2_NAME="Entire Country",
        ID=0,
        value=mean(value, na.rm=T)), by=month]
  } else {
    # Limit to selected district
    dt <- dt[ADM2_NAME==dist]
  }
  
  # Add trend component for the 1960-2013 period
  dt <- dt[order(month)]
  dt.ts <- ts(dt$value, start=c(dt[, min(year(month))], 1), frequency=12)
  dt.ts <- decompose(zoo::na.StructTS(dt.ts))
  dt.ts <- data.table(trend=dt.ts$trend, seasonal=dt.ts$seasonal)
  dt <- cbind(dt, dt.ts)
  
  # Limit to selected period and month
  dt <- dt[month %between% range(tm)]
  if (mth>0) dt <- dt[which(month(month) %in% mth)]
  
  # Compute stats over selected period/month
  dt[, mean := mean(value, na.rm=T)]
  dt[, meanAnnual := mean(value, na.rm=T), by=year(month)]
  dt[, sd := sd(value, na.rm=T)]
  dt[, mad := mad(value, na.rm=T)]
  return(dt)
}



shinyServer(function(input, output, session) {
    
    # Create the map
    map <- createLeafletMap(session, "map")
    
    # Create reactive controls
    output$selectVar <- renderUI({
        selectInput("selectVar", "Choose a Variable", d[c(5,11)], selected="pre")
      })
    
    output$selectg0 <- renderUI({
        selectInput("selectg0", "Choose a Country", names(g2.list), selected="Kenya")
      })
    
    output$selectg2 <- renderUI({
        selectizeInput("selectg2", "Limit to District",
          choices=c("Entire Country", g2.list[[cntr()]]), selected="Entire Country")
      })
    
    var <- reactive({
        if (input$btn>0) isolate(input$selectVar) else "pre"
      })
    
    cntr <- reactive({
        if (input$btn>0) isolate(input$selectg0) else "Kenya"
      })
    
    dist <- reactive({
        if(length(input$selectg2)>0) input$selectg2 else "Entire Country"
      })
    
    tm <- reactive({
        # Selected time range
        if (input$btn==0) return(seq(as.Date("1960-01-01"), as.Date("2013-12-31"), "month"))
        seq(as.Date(paste0(input$rg[1], "-01-01")), as.Date(paste0(input$rg[2], "-12-31")), "month")
      })
    
    g <- reactive({
        # Selected country boundaries
        if (input$btn==0) return()
        g2[g2$ADM0_NAME==cntr(),]
      })
    
    
    dt1 <- reactive({
        if (input$btn==0) return()
        isolate({
            #closeAlert(session, "alertNoData")
            # Read district X month records from disk (see pre-process steps in `data.R`)
            dt <- try(readRDS(paste0("../rainfall/data/rds/", var(), g2.dt[cntr()][, ADM0_CODE], ".rds")))
            if (class(dt)=="try-error") {
              # File is missing for that country
              createAlert(session, "alertNoData",
                message="Try another combination.",
                title="Missing Data", type="warning", block=F, append=F)
            } else return(dt)
          })
      })
    
    dt2 <- reactive({
        # Isolate from button click
        dist <- dist()
        tm <- tm()
        mth <- input$selectMonth
        isolate({
            if (input$btn==0) return()
            genStats(dt1(), cntr(), dist, tm, mth)
          })
      })
    
    output$dygraph <- renderDygraph({
        if (is.null(dt2())) return()
        # Isolate from all but dt2() and input$selectSeries
        dt <- dt2()
        sr <- input$selectSeries
        isolate ({
            if (input$btn==0) return()
            # Convert data.table to xts
            out <- dygraph(xts::as.xts(dt[, list(value, mean, meanAnnual, trend)], order.by=dt$month)) %>%
              dyOptions(fillGraph=T, fillAlpha=0.4) %>%
              dyLegend(show="always", hideOnMouseOut=F, labelsSeparateLines=T, width=140) %>%
              dyRangeSelector(height=20)
            
            if("1" %in% sr) out <- out %>% dySeries("value", label=var(),
                colors=if(var()=="pdsi") "#FF9900" else "#53B376")
            if("2" %in% sr) out <- out %>% dySeries("meanAnnual", label="annual mean", 
                colors=if(var()=="pdsi") "#99FF99" else "#F4EB7E", fillGraph=F, strokeWidth=2)
            if("3" %in% sr) out <- out %>% dySeries("mean", label="period mean",
                colors=if(var()=="pdsi") "#009900" else "#2F6FBF")
            if("4" %in% sr) out <- out %>% dySeries("trend", label="trend",
                colors=if(var()=="pdsi") "#F8DE70" else "#DD5A0B", fillGraph=F, strokeWidth=3, strokePattern="dashed")
          })
        return(out)
      })
    
    output$chartMsg <- renderText({
        if (input$btn==0) return()
        out <- div(
          h3(names(d)[d==var()], br(), tags$small(tags$mark(dist()), ", ", cntr(), " - Period: ",
              paste0(format(range(tm()), "%b %Y"), collapse=" - "))),
          p("The long-term mean is over the selected months and period only (or over the
              entire year if no month is selected). The trend component is generated through
              classical seasonal decomposition by moving averages over the entire 1960-2013 period."))
        return(as.character(out))
      })
    
    # Draw GeoJSON
    observe({
        if (input$btn==0) return()
        # Read country GeoJSON from disk (pre-processed in `data.R` to .rds files)
        m <<- try(readRDS(paste0("../rainfall/data/rds/", var(), g2.dt[cntr()][, ADM0_CODE], ".json.rds")))
        if (class(m)=="try-error") {
          # File is missing for that country
          createAlert(session, "alertNoData",
            message="Try another combination.",
            title="Missing Data", type="warning", block=T)
        }
        
        # Center map to selected country centroid
        map$clearGeoJSON()
        coords <- apply(sp::coordinates(g()), 2, mean, na.rm=T)
        map$setView(coords[2], coords[1]+5, 6)
        map$addGeoJSON(m)
      })
    
    # Highlight selected polygon
    observe({
        if (dist()=="Entire Country") return()
        i <- which(sapply(m$features, function(x) x$properties$ADM2_NAME==dist()))
        m$features <- m$features[i]
        m$features[[1]]$style <- list(fillColor="gray", weight=.6, color="white", fillOpacity=0.7)
        map$addGeoJSON(m, 0)
      })
    
    # Show district details on mouseover
    output$details <- renderText({
        evt <- input$map_geojson_mouseover
        if (is.null(evt)) {
          out <- div(h3(cntr()), p("Mouse over districts to view details."))
        } else {
          out <- div(
            h3(cntr(), br(), tags$small(evt$properties$ADM2_NAME, ", ", evt$properties$ADM1_NAME)),
            hr(),
            h4("1960-2013 ", names(d)[d==var()]),
            p(
            "Mean: ", strong(evt$properties$mean), br(),
            "Min: ", strong(evt$properties$min), br(),
            "Max: ", strong(evt$properties$max), br(),
            "Sd. Dev.: ", strong(evt$properties$sd)),
            p(em("Click the map to select this district.")))
        }
        return(as.character(out))
      })
    
    # Update district on map click
    observe({
        evt <- input$map_geojson_click
        if (is.null(evt)) return()
        updateSelectInput(session, "selectg2", selected=evt$properties$ADM2_NAME)
      })
    
    
    output$saveData <- downloadHandler(function() {
        f <- paste0(cntr(), "-", var())
        if (input$fileType %in% c("csv", "dta")) {
          # Complete file path
          paste0(f, "-", dist(), "-", paste(range(tm()), collapse="-"), ".", input$fileType)
        } else {
          # File path with `.zip`
          paste0(f, ".", input$fileType, ".zip")
        }
      }, function(file) {
        
        if (input$fileType %in% c("tif", "nc")) {
          # Crop raster to selected country extent
          require(raster)
          r <- brick(get(paste0("path.", var())))
          r <- crop(r, g())
          # Define z dimension (time)
          r <- setZ(r, get(paste0("tm.", var())), "month")
        }
        
        f <- paste0(cntr(), "-", var(), ".", input$fileType)
        
        switch(input$fileType,
          tif = writeRasterZip(r, file, f, "GTiff",
            options="INTERLEAVE=BAND"),
          nc = writeRasterZip(r, file, f, "CDF"),
          csv = write.csv(dt2(), file, row.names=F, na=""),
          dta = foreign::write.dta(dt2(), file, version=9L),
          shp = {
            # Combine mean attributes with GAUL 2008 boundaries
            dt <- dt1()[, list(
                mean=mean(value, na.rm=T),
                min=min(value, na.rm=T),
                max=max(value, na.rm=T),
                sd=sd(value, na.rm=T),
                mad=mad(value, na.rm=T)), by=ADM2_CODE]
            g <- g2[g2$ADM0_NAME==cntr(),]
            tmp <- data.table(g@data)
            tmp[, rn := row.names(g)]
            setkey(tmp, ADM2_CODE)
            setkey(dt, ADM2_CODE)
            tmp <- dt[tmp]
            setkey(tmp, rn)
            g@data <- tmp[row.names(g)]
            writeRasterZip(g, file, f, "ESRI Shapefile")
          })
      })
    
    
    #   # Rank districts on barchart
    #   output$barPlot <- renderPlot({
    #     if(input$btn==0) return()
    #     dt <- dt1()
    #     dt <- dt[, list(value=mean(value, na.rm=T)), by=list(ADM2_NAME)]
    #     dt <- dt[order(value, decreasing=T)]
    #     dt <- rbind(head(dt,4), data.table(ADM2_NAME="[ ... ]", value=0), tail(dt,4))
    #     par(las=1, mar=c(3.1,5,0,0.1), fg="#444444" )
    #     barplot(dt$value, names.arg=dt$ADM2_NAME, horiz=T,
    #       col="#2F6FBF", border="white", xlab=NULL, ylab=NULL, main=NULL)
    #   })
    #
    #
    #   # Show country raster plot
    #   output$rasterPlot <- renderPlot({
    #     if(input$btn==0) return()
    #     spplot(stats(), col.regions=colorRampPalette(if(var()=="pdsi") col.pdsi else col.pre)(20))
    #     #plot(g(), add=T)
    #   })
    
  })
