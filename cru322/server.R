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

setwd("/home/projects/www/tmp")

# CRU variables
d <- c("cld", "dtr", "frs", "pet", "pre", "tmn", "tmp", "tmx", "vap", "wet")
names(d) <- c("Cloud Cover (%)", "dtr", "frs", "pet", "pre", "tmn", "Temperature (C)", "tmx", "vap", "wet")

# CRU 3.22 time series
tmp <- brick("../cru322/data/cru_ts3.22.1901.2013.tmp.dat.nc")
cld <- brick("../cru322/data/cru_ts3.22.1901.2013.cld.dat.nc")
tm <- seq(as.Date("1901-01-15"), as.Date("2013-12-16"), "month")

# GAUL district boundaries
load("../../cell5m/rdb/g2.rda")
g2.dt <- data.table(g2@data)
g2.dt <- g2.dt[ADM2_CODE>0, .N, by=list(ADM0_NAME, ADM1_NAME, ADM2_NAME)]

# Month array
mth <- 0:12
names(mth) <- c("All", month.name)

shinyServer(function(input, output, session) {

  # Create the map
  map <- createLeafletMap(session, "map")

  # Create input controls
  output$selectCRU <- renderUI({ selectInput("selectCRU", "Choose a CRU Variable",
    d[c(1,7)], selected="tmp") })

  output$selectg0 <- renderUI({ selectInput("selectg0", "Choose a Country",
    g2.dt[order(ADM0_NAME), unique(as.character(ADM0_NAME))], selected="Ghana") })

  output$selectg2 <- renderUI({ selectizeInput("selectg2", "Choose a District",
    choices=sg2(), selected="Adansi East") })

  output$selectMonth <- renderUI({ selectInput("selectMonth", "Limit to Month",
    mth, selected=0) })

  output$dygraph <-  renderDygraph({
    if (input$btn>0) {
      dt <- stats.dt2()
      dygraph(as.xts(dt[, list(value, mean)], order.by=dt$month)) %>%
        dySeries("value", label=cru(), color="#00BFFF") %>%
        dySeries("mean", label="long-term mean", color="red") %>%
        dyOptions(fillGraph=T, fillAlpha=0.4) %>%
        dyLegend(show="always", hideOnMouseOut=F) %>%
        dyRangeSelector(height=20)
    } else return()
  })

  output$waitMsg <- reactive({
    if (input$btn>0) {
      paste0("Monthly ", names(d)[d==cru()], " (", dist(), ", ", cntr(), ") - Period: ",
        paste0(format(range(r.tm()), "%Y-%m"), collapse=" - "))
    } else {
      paste0("Select a district, click 'Show Series' and wait a few seconds for the time-series graph and map to render.")
    }
  })

  output$saveData <- downloadHandler(
    function() paste0(cntr(), "-", dist(), "-", cru(), "-",
      paste(range(r.tm()), collapse="-"), ".", input$fileType),
    function(file) {
      r <- stats()
      dt <- stats.dt2()[, .SD, .SDcols=-c(1:3)]
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

  cntr <- reactive({
    if (input$btn==0) "Ghana" else input$selectg0
  })

  sg2 <- reactive({
    d <- g2.dt[ADM0_NAME==cntr(), .N, keyby=list(ADM1_NAME, ADM2_NAME)]
    r <- lapply(d[, unique(ADM1_NAME)], function(x) d[ADM1_NAME==x, as.character(ADM2_NAME)])
    names(r) <- d[, unique(ADM1_NAME)]
    return(r)
  })

  cru <- reactive({
    if (input$btn==0) "tmp" else isolate(input$selectCRU)
  })

  dist <- reactive({
    if (input$btn==0) "Adansi East" else isolate(input$selectg2)
  })

  g <- reactive({ g2[g2$ADM2_NAME==dist(),] })

  r.tm <- reactive({
    if (input$btn==0) return(tm)
    seq(as.Date(paste0(input$rg[1], "-01-01")), as.Date(paste0(input$rg[2], "-12-31")), "month")
  })

  stats <- reactive({
    # Crop to selected district extent
    r <- crop(get(cru()), g())
    # Add z dimension (time)
    r <- setZ(r, tm, "month")
    return(r)
  })

  stats.dt1 <- reactive({
    # Summarize over district and convert to data.table
    dt <- extract(stats(), g(), fun=mean, na.rm=T, df=T, small=T)
    dt <- cbind(g()@data[, c("ADM1_NAME", "ADM2_NAME")], dt)
    dt <- data.table(dt)
    dt <- melt(dt, id.vars=c("ADM1_NAME", "ADM2_NAME", "ID"), variable.name="month", variable.factor=F)
    dt[, month := as.Date(month, format="X%Y.%m.%d")]
    return(dt)
  })

  stats.dt2 <- reactive({
    # Filter by period and month
    dt <- stats.dt1()
    dt <- dt[month %between% range(r.tm())]
    if (input$selectMonth>0) dt <- dt[which(month(month)==input$selectMonth)]
    dt[, mean := mean(value, na.rm=T)]
    dt[, cv := cv(value, na.rm=T)]
    dt[, diff := value-mean]
    return(dt)
  })

  drawDistricts <- observe({
    if (input$btn==0) return()
    else {
      isolate({
        m <- g()
        coords <- coordinates(m)
        map$clearShapes()
        map$setView(coords[2], coords[1], 8)
        # Convert sp to map format (too slow)
        md <- maptools::sp2tmap(m)
        names(md) <- c("ID", "X", "Y")
        # Draw polygons
        map$addPolygon(I(md$Y), I(md$X), I(md$ID),
          lapply(md$ID, function(x) list(fillColor="yellow")),
          list(fill=T, fillOpacity=0.5, stroke=T, opacity=1, color="white"))
      })
    }
  })



})