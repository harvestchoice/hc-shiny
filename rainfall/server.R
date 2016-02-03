#####################################################################################
# Title: CRU and PDSI 1960-2013 Time Series across Districts
# Date: December 2014
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################


#####################################################################################
# Helper - Print map
#####################################################################################
printMap <- function(x, var, cntr) {
  require(tmap)

  # Need to reproject
  x <- spTransform(x, CRS("+init=epsg:3857"))

  p <- tm_shape(g0) + tm_polygons(col="white", borders="grey90") +
    tm_text("ADM0_NAME", size=0.9, col="grey70") +
    tm_shape(x, is.master=T) +
    tm_fill(col="mean", n=6, style="kmeans", alpha=0.8, legend.hist=T,
      title=names(d)[d==var], palette=colorRampPalette(vars[[var]]$pal)(9), colorNA="grey90") +
    tm_credits("IFPRI/HarvestChoice, 2015. www.harvestchoice.org") +
    tm_layout(title=cntr, title.size=1.2, bg.color="#5daddf",
      inner.margin=c(0,0.3,0,0), legend.position=c(0.02, 0.02))

  return(p)
}


#####################################################################################
# Helper - Archive spatial formats for download
#####################################################################################
writeRasterZip <- function(x, file, filename, format, ...) {

  if (format=="ESRI Shapefile") {
    writeOGR(x, "./", filename, format, overwrite_layer=T, check_exists=T)
  } else {
    writeRaster(x, filename, format, bylayer=F, overwrite=T, ...)
  }

  f <- list.files(pattern=paste0(strsplit(filename, ".", fixed=T)[[1]][1], ".*"))
  zip(paste0(filename, ".zip"), f, flags="-9Xjm", zip="zip")
  file.copy(paste0(filename, ".zip"), file)
  file.remove(paste0(filename, ".zip"))
}


#####################################################################################
# Helper - Generate time series stats
#####################################################################################
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

  # Add trend component over 1960-2013 period
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


#####################################################################################
# Helper - Make popup
#####################################################################################
genPopup <- function(x, var) {
  # Show district details
  txt <- div(
    h3(x$ADM0_NAME, br(), tags$small(x$ADM2_NAME, ", ", x$ADM1_NAME)),
    h5(var),
    p(
      "Mean: ", strong(prettyNum(x$mean, digits=0)), br(),
      "85th perc.: ", strong(prettyNum(x$perct, digits=0)), br(),
      "Max: ", strong(prettyNum(x$max, digits=0)), br(),
      "Sd. Dev.: ", strong(prettyNum(x$sd, digits=0))),
    helpText("Click the map to view time-series for this district."))
  return(as.character(txt))
}


#####################################################################################
# Main
#####################################################################################

shinyServer(function(input, output, session) {

  # Init reactive values
  values <- reactiveValues()

  # Create the map
  output$map <- renderLeaflet(
    map <- leaflet() %>%
      setView(41, 1, 6) %>%
      addTiles(urlTemplate="http://{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution="Mapbox")
  )

  # Primary observer (react to main button)
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

    # Add statistics
    dt <- data[ADM0_NAME==input$selectg0 & var==input$var]
    dt.g2 <- dt[, list(
      mean=mean(value, na.rm=T),
      min=min(value, na.rm=T),
      perct=quantile(value, 0.85, na.rm=T),
      max=max(value, na.rm=T),
      sd=sd(value, na.rm=T)),
      by=list(ADM0_CODE, ADM0_NAME, ADM1_CODE, ADM1_NAME, ADM2_CODE, ADM2_NAME)]

    # Get country boundaries
    g <- g2[g2$ADM0_NAME==input$selectg0,]

    # Merge attributes
    setkey(dt.g2, ADM2_CODE)
    g@data <- data.frame(dt.g2[J(g@data$ADM2_CODE)])
    coords <- apply(sp::coordinates(g), 2, mean, na.rm=T)
    pal <- colorBin(vars[[input$var]]$pal, dt$mean)

    # Update map
    leafletProxy("map", data=g) %>%
      # Recenter map if country has changed
      setView(coords[1]+5, coords[2], 6) %>%
      clearShapes() %>%

      # Add polygons
      addPolygons(group="Country", layerId=~ADM2_CODE,
        stroke=F, fillOpacity=0.5, fillColor=~pal(mean)) %>%

      # Add legend
      addLegend("bottomleft", opacity=1, pal=pal, values=~mean, layerId="lgd",
        title=names(d)[d==input$var])

    # Show popup
    output$details <- renderText(as.character(helpText("Mouse over districts to view details.")))

    # Export
    values$g <- g
    values$dt1 <- dt
    values$g0 <- input$selectg0
    values$var <- input$var
  })


  # Secondary observer
  dt2 <- eventReactive({
    # React on 3 events
    input$selectg2
    input$rg
    input$selectMonth }, {

      if (input$btn==0) return()
      values$tm <- seq(as.Date(paste0(input$rg[1], "-01-01")), as.Date(paste0(input$rg[2], "-12-31")), "month")
      mth <- as.integer(seq(input$selectMonth[1], input$selectMonth[2], 1))

      # Generate monthly statistics
      dt <- try(genStats(values$dt1, input$selectg0, input$selectg2, values$tm, mth))
      if (class(dt)[1]=="try-error") {
        # Data missing for that district
        createAlert(session, "alertNoData", "alertNoData",
          content="Try another district.",
          title="Missing Data", style="warning", append=F)
      }
      return(dt)
    })


  # Secondary observer
  observeEvent(dt2(), {
    dt <- dt2()

    # Update title text
    output$selectedMsg <- renderText({
      mth <- paste0(" (", paste0(substr(unique(month.name[input$selectMonth]), 1, 3), collapse="-"), ")")
      dist <- if(input$selectg0==input$selectg2) "Entire Country" else input$selectg2
      out <- h3(names(d)[d==values$var], br(),
        tags$small(tags$mark(dist, ", ", values$g0), "  Period: ",
          paste0(format(range(values$tm), "%b %Y"), collapse=" - "), mth))
      as.character(out)
    })

    # Render monthly time-series
    output$dygraph <- renderDygraph({
      dygraph(xts::as.xts(dt[, list(month, value, mean, trend)]), group="dy") %>%
        dySeries("value", label=values$var) %>%
        dySeries("mean", label="period mean") %>%
        dySeries("trend", label="trend", fillGraph=F, strokeWidth=3, strokePattern="dashed") %>%
        dyOptions(fillGraph=T, fillAlpha=0.4,
          colors=switch(values$var,
            pre=c("#84C796", "#CA6943", "#428BCA"),
            tmp=c("#1C90FF", "#FFE131 ", "#FF0000"),
            pdsi=c("#8DDE88", "#F8DE70", "#F8AE41"),
            eratp=c("#1D91C0", "#EDF8B1", "#081D58"))) %>%
        dyLegend(show="always", hideOnMouseOut=F, labelsSeparateLines=T, width=140)
    })


    # Render annual time-series
    output$dygraphAnnual <- renderDygraph({
      mth <- paste0(substr(unique(month.name[input$selectMonth]), 1, 3), collapse="-")

      if (values$var=="pre") {
        dt <- dt2()[, list(sumAnnual=sum(value, na.rm=T)), by=list(month=year(month))]
        txt <- paste0(mth, " total")
      } else {
        dt <- dt2()[, list(sumAnnual=mean(value, na.rm=T)), by=list(month=year(month))]
        txt <- paste0(mth, " mean")
      }

      dygraph(xts::as.xts(dt$sumAnnual, order.by=as.Date(as.character(dt$month), "%Y")), group="dy") %>%
        dySeries("V1", label=txt) %>%
        dyOptions(fillGraph=F, strokeWidth=2,
          colors=switch(values$var, pre="#84C796", tmp="#1C90FF", pdsi="#8DDE88", aritp="#1D91C0")) %>%
        dyLegend(show="always", hideOnMouseOut=F, labelsSeparateLines=T, width=180) %>%
        dyRangeSelector(height=20)
    })
  })



  # Highlight selected polygon
  observeEvent(input$selectg2, priority=-5, {
    if (input$btn==0) return()
    dist <- input$selectg2
    if (dist==values$g0 | dist=="Entire Country") return()
    g <- values$g
    g <- g[g$ADM2_NAME==dist,]

    # Add to map
    leafletProxy("map", data=g) %>%
      clearGroup("Selected") %>%
      addPolygons(group="Selected", smoothFactor=3,
        color="yellow", opacity=0.7, fillColor="grey50")
  })


  # Update district on map click
  observeEvent(input$map_shape_click, {
    setkey(data, ADM2_CODE)
    evt <- data[J(input$map_shape_click$id)][, ADM2_NAME]
    updateSelectInput(session, "selectg2", selected=evt)
  })


  # Show district details on mouseover
  observeEvent(input$map_shape_mouseover,
    output$details <- renderText({
      evt <- values$g@data[values$g$ADM2_CODE==input$map_shape_mouseover$id, ]
      genPopup(evt, names(d)[d==values$var])
    }))


  # Download handler
  output$saveData <- downloadHandler(function() {
    f <- paste0(values$g0, "-", values$var)
    if (input$fileType %in% c("csv", "dta", "pdf")) {
      # Complete file path
      paste0(f, "-", input$selectg2, "-", paste(range(values$tm), collapse="-"), ".", input$fileType)
    } else {
      # File path with `.zip`
      paste0(f, ".", input$fileType, ".zip")
    }
  }, function(file) {

    f <- paste0(values$g0, "-", values$var, ".", input$fileType)

    if (input$fileType %in% c("tif", "nc")) {
      # Crop raster to selected country extent
      require(raster)
      r <- brick(get(paste0("path.", values$var)))
      r <- crop(r, values$g)
      # Define z dimension (time)
      proj4string(r) <- CRS("+init=epsg:4326")
      r <- setZ(r, as.character(get(paste0("tm.", values$var))))
      names(r) <- as.character(get(paste0("tm.", values$var)))
    }

    switch(input$fileType,
      tif = writeRasterZip(r, file, f, "GTiff", options=c("TFW=YES", "INTERLEAVE=BAND")),
      nc = writeRasterZip(r, file, f, "CDF",
        varname=values$var, varunit="mm", zname="month", zunit="month",
        options=c("COMPRESS=DEFLATE", "WRITE_GDAL_TAGS=YES")),
      csv = write.csv(dt2(), file, row.names=F, na=""),
      dta = foreign::write.dta(dt2(), file, version=12L),
      shp = writeRasterZip(values$g, file, f, "ESRI Shapefile"),
      pdf = { if (!file.exists(f)) {
        # Re-generate PDF
        pdf(file=f, paper="letter")
        print(printMap(values$g, values$var,
          paste0(values$g0, "\n", paste0(format(range(values$tm), "%b %Y"), collapse=" - "))))
        dev.off() }
        file.copy(f, file)
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
