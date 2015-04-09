#####################################################################################
# Title: CRU and PDSI 1960-2013 Time Series across Districts
# Date: December 2014
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################


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

  if (dist==cntr) {
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
  dt <- dt[as.integer(format(month, "%m")) %in% mth]

  # Compute stats over selected period/month
  dt[, mean := mean(value, na.rm=T)]
  dt[, total := sum(value, na.rm=T)]
  dt[, meanAnnual := mean(value, na.rm=T), by=year(month)]
  dt[, sd := sd(value, na.rm=T)]
  dt[, mad := mad(value, na.rm=T)]
  return(dt)
}



shinyServer(function(input, output, session) {

  # Create the map
  map <- createLeafletMap(session, "map")

  # Init
  values <- reactiveValues()

  # Primary observer
  observeEvent(input$btn, priority=1, {

    # Update district list
    updateSelectInput(session, "selectg2",
      choices=c(`Entire Country`=input$selectg0, g2.list[[input$selectg0]]),
      selected=input$selectg0)

    # Update time slider
    switch(input$var,
      pre=updateSliderInput(session, "rg", min=1960, max=2013, value=c(1990, 2013)),
      tmp=updateSliderInput(session, "rg", min=1960, max=2013, value=c(1990, 2013)),
      pdsi=updateSliderInput(session, "rg", min=1960, max=2012, value=c(1990, 2012)),
      aritp=updateSliderInput(session, "rg", min=1979, max=2014, value=c(1990, 2014)))

    # Read monthly district records from disk (see pre-process steps in `data.R`)
    dt <- try(readRDS(paste0("../rainfall/data/rds/", input$var, g2.dt[input$selectg0][, ADM0_CODE], ".rds")))

    # Read country GeoJSON from disk (pre-processed in `data.R` to .rds files)
    m <- try(readRDS(paste0("../rainfall/data/rds/", input$var, g2.dt[input$selectg0][, ADM0_CODE], ".json.rds")))

    if (class(dt)[1]=="try-error" | class(m)[1]=="try-error") {
      # File is missing for that country
      createAlert(session, "alertNoData",
        message="Try another combination.",
        title="Missing Data", type="warning")

    } else {

      # Get country boundaries
      g <- g2[g2$ADM0_NAME==input$selectg0,]

      # Re-center map to selected country centroid
      map$clearGeoJSON()
      coords <- apply(sp::coordinates(g), 2, mean, na.rm=T)
      map$setView(coords[2], coords[1]+5, 6)
      map$addGeoJSON(m)

      # Export
      values$m <- m
      values$g <- g
      values$dt1 <- dt
    }
  })


  # Secondary observer
  dt2 <- eventReactive({
    # React on 3 events
    input$selectg2
    input$rg
    input$selectMonth }, {

      if (input$btn==0) return()
      closeAlert(session, "alertNoData")
      values$tm <- seq(as.Date(paste0(input$rg[1], "-01-01")), as.Date(paste0(input$rg[2], "-12-31")), "month")
      mth <- as.integer(seq(input$selectMonth[1], input$selectMonth[2], 1))

      # Generate monthly statistics
      dt <- try(genStats(values$dt1, input$selectg0, input$selectg2, values$tm, mth))
      if (class(dt)[1]=="try-error") {
        # Data missing for that district
        createAlert(session, "alertNoData", "alertNoData",
          message="Try another district.",
          title="Missing Data", type="warning", block=T, append=F)
      }
      return(dt)
    })


  # Update title text
  output$selectedMsg <- renderText({
    # React to dt2() only
    if (is.null(dt2())) return()

    isolate({
      mth <- paste0(" (", paste0(substr(unique(month.name[input$selectMonth]), 1, 3), collapse="-"), ")")
      dist <- if(input$selectg0==input$selectg2) "Entire Country" else input$selectg2
      out <- h3(names(d)[d==input$var], br(),
        tags$small(tags$mark(dist,", ", input$selectg0), " Period: ",
          paste0(format(range(values$tm), "%b %Y"), collapse=" - "), mth))
      return(as.character(out))
    })
  })


  # Render monthly time-series
  output$dygraph <- renderDygraph({
    # React to dt2() only
    if (is.null(dt2())) return()

    isolate({
      dt <- dt2()
      dygraph(xts::as.xts(dt[, list(value, mean, trend)], order.by=dt$month), group="dy") %>%
        dySeries("value", label=input$var) %>%
        dySeries("mean", label="period mean") %>%
        dySeries("trend", label="trend", fillGraph=F, strokeWidth=3, strokePattern="dashed") %>%
        dyOptions(fillGraph=T, fillAlpha=0.4,
          colors=switch(input$var,
            pre=c("#84C796", "#CA6943", "#428BCA"),
            tmp=c("#1C90FF", "#FFE131 ", "#FF0000"),
            pdsi=c("#8DDE88", "#F8DE70", "#F8AE41"),
            eratp=c("#1D91C0", "#EDF8B1", "#081D58"))) %>%
        dyLegend(show="always", hideOnMouseOut=F, labelsSeparateLines=T, width=140)
    })
  })


  # Render annual time-series
  output$dygraphAnnual <- renderDygraph({
    # React to dt2() only
    if(is.null(dt2())) return()

    isolate({
      mth <- paste0(substr(unique(month.name[input$selectMonth]), 1, 3), collapse="-")

      if (input$var=="pre") {
        dt <- dt2()[, list(sumAnnual=sum(value, na.rm=T)), by=list(month=year(month))]
        txt <- paste0(mth, " total")
      } else {
        dt <- dt2()[, list(sumAnnual=mean(value, na.rm=T)), by=list(month=year(month))]
        txt <- paste0(mth, " mean")
      }

      dygraph(xts::as.xts(dt$sumAnnual, order.by=as.Date(as.character(dt$month), "%Y")), group="dy") %>%
        dySeries("V1", label=txt) %>%
        dyOptions(fillGraph=F, strokeWidth=2,
          colors=switch(input$var, pre="#84C796", tmp="#1C90FF", pdsi="#8DDE88", aritp="#1D91C0")) %>%
        dyLegend(show="always", hideOnMouseOut=F, labelsSeparateLines=T, width=180) %>%
        dyRangeSelector(height=20)
    })
  })


  # Highlight selected polygon
  observeEvent(input$selectg2, priority=-5, {
    if (input$btn==0) return()
    dist <- input$selectg2
    if (dist==input$selectg0 | dist=="Entire Country") return()
    m <- values$m
    i <- which(sapply(m$features, function(x) x$properties$ADM2_NAME==dist))
    m$features <- m$features[i]
    m$features[[1]]$style <- list(fillColor="gray", weight=.6, color="white", fillOpacity=0.7)
    map$addGeoJSON(m, 0)
  })


  # Update district on map click
  observeEvent(input$map_geojson_click,
    updateSelectInput(session, "selectg2",
      selected=input$map_geojson_click$properties$ADM2_NAME))


  # Show district details on mouseover
  output$details <- renderText({
    evt <- input$map_geojson_mouseover
    isolate({
      if (is.null(evt)) {
        out <- div(h3(input$selectg0), p("Mouse over districts to view details."))
      } else {
        out <- div(
          h3(input$selectg0, br(), tags$small(evt$properties$ADM2_NAME, ", ", evt$properties$ADM1_NAME)),
          hr(),
          h4(names(d)[d==input$var]),
          p(
            "Mean: ", strong(evt$properties$mean), br(),
            "85th perc.: ", strong(evt$properties$`85th`), br(),
            "Max: ", strong(evt$properties$max), br(),
            "Sd. Dev.: ", strong(evt$properties$sd)),
          p(em("Click the map to select this district.")))
      }
    })
    return(as.character(out))
  })


  # Download handler
  output$saveData <- downloadHandler(function() {
    f <- paste0(input$selectg0, "-", input$var)

    if (input$fileType %in% c("csv", "dta")) {
      # Complete file path
      paste0(f, "-", input$selectg2, "-", paste(range(values$tm), collapse="-"), ".", input$fileType)
    } else {
      # File path with `.zip`
      paste0(f, ".", input$fileType, ".zip")
    }
  }, function(file) {

    var <- input$var
    f <- paste0(input$selectg0, "-", var, ".", input$fileType)

    if (input$fileType %in% c("tif", "nc")) {
      # Crop raster to selected country extent
      require(raster)
      r <- brick(get(paste0("path.", var)))
      r <- crop(r, values$g)
      # Define z dimension (time)
      proj4string(r) <- CRS("+init=epsg:4326")
      r <- setZ(r, as.character(get(paste0("tm.", var))))
      names(r) <- as.character(get(paste0("tm.", var)))
    }

    switch(input$fileType,
      tif = writeRasterZip(r, file, f, "GTiff", options=c("TFW=YES", "INTERLEAVE=BAND")),
      nc = writeRasterZip(r, file, f, "CDF",
        varname=input$var, varunit="mm", zname="month", zunit="month",
        options=c("COMPRESS=DEFLATE", "WRITE_GDAL_TAGS=YES")),
      csv = write.csv(dt2(), file, row.names=F, na=""),
      dta = foreign::write.dta(dt2(), file, version=9L),
      shp = {
        # Combine mean attributes with GAUL 2008 boundaries
        dt <- values$dt1[, list(
          mean=mean(value, na.rm=T),
          min=min(value, na.rm=T),
          max=max(value, na.rm=T),
          sd=sd(value, na.rm=T),
          mad=mad(value, na.rm=T)), by=ADM2_CODE]
        g <- values$g
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
  #   output$distBarPlot <- renderPlot({
  #     if (is.null(values$dt1)) return()
  #     isolate ({
  #       dt <- values$dt1
  #       dt <- dt[, list(value=mean(value, na.rm=T)), by=list(ADM2_NAME)]
  #       dt <- dt[order(value, decreasing=T)]
  #       dt <- rbind(head(dt,4), data.table(ADM2_NAME="[ ... ]", value=0), tail(dt,4))
  #       dt[, ADM2_NAME := factor(ADM2_NAME, levels=ADM2_NAME, ordered=T)]
  #       par(las=1, mar=c(0, 10, 4.1, 0), fg="#444444")
  #       asTheEconomist(
  #         barchart(ADM2_NAME~value, dt,
  #           main=names(d)[d==input$var],
  #           col="#2F6FBF", border="#ffffff", alpha=.8, horizontal=T))
  #     })
  #   })

  #
  #   # Show country raster plot
  #   output$rasterPlot <- renderPlot({
  #     if(input$btn==0) return()
  #     spplot(stats(), col.regions=colorRampPalette(if(var()=="pdsi") col.pdsi else col.pre)(20))
  #     #plot(g(), add=T)
  #   })

})
