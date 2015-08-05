

# Helper - Archive spatial formats for download
writeRasterZip <- function(x, file, filename, format, ...) {
  writeOGR(x, "./", filename, format, overwrite_layer=T, check_exists=T)
  f <- list.files(pattern=paste0(strsplit(filename, ".", fixed=T)[[1]][1], ".*"))
  zip(paste0(filename, ".zip"), f, flags="-9Xjm", zip="zip")
  file.copy(paste0(filename, ".zip"), file)
  file.remove(paste0(filename, ".zip"))
}


shinyServer(function(input, output, session) {

  m08 <- m[m$ISO3=="GHA" & m$Y08==T,]
  pald <- colorNumeric(rev(brewer.pal(11, "RdYlGn")), m08@data[, "hc_poor2"], na.color="grey90")

  values <- reactiveValues(
    dt=m.dt[ISO3=="GHA" & m$Y08==T, .SD, .SDcols=-c(3,4,5,7,23:25)]
  )

  map <- leaflet(data=m08) %>%
    setView(1, 2, 6) %>%
    addTiles(urlTemplate="http://{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution="Mapbox") %>%

    # Add polygons
    addPolygons(stroke=F, fillOpacity=0.6, fillColor=~pald(hc_poor2),
      popup=~paste(prttyNm, prettyNum(hc_poor2), sep="<br />")) %>%

    # Add legend
    addLegend("bottomright", opacity=1, pal=pald, values=~hc_poor2,
      title="Headcount Ratio\nbelow PPP $2/year",
      labFormat=labelFormat(digits=0, transform=perct))

  output$map <- renderLeaflet(map)

  # Year select
  observeEvent(input$selectISO3, {
    updateSelectInput(session, inputId="selectYear",
      choices = if (input$selectISO3=="SSA") def else {
        m.dt[ISO3==input$selectISO3][order(-year), unique(year)] })
  })

  # Update map
  observeEvent(input$btn, {

    # Select lists
    if (input$selectISO3=="SSA" & input$selectYear=="circa 2005") g <- m[m$Y05==T,]
    else if (input$selectISO3=="SSA" & input$selectYear=="circa 2008") g <- m[m$Y08==T, ]
    else g <- m[m$ISO3==input$selectISO3 & m$year==as.numeric(input$selectYear),]
    names(g)[which(names(g)==input$var)] <- "value"
    coords <- apply(bbox(g), FUN=mean, MARGIN=1)

    # Make palette
    pal <- colorNumeric(rev(brewer.pal(11, "RdYlGn")), g$value, na.color="grey90")

    # Add layer
    leafletProxy("map", data=g) %>%
      # Recenter map if country has changed
      setView(coords[[1]], coords[[2]], 6) %>%
      clearShapes() %>%
      clearControls() %>%

      # Add polygons
      addPolygons(stroke=F, fillOpacity=0.5, fillColor=~pal(value), smoothFactor=2,
        popup=~paste(prttyNm, prettyNum(value), sep="<br />")) %>%

      # Add legend
      addLegend("bottomright", opacity=1, pal=pal, values=~value,
        title=input$var, labFormat=labelFormat(digits=0, transform=perct))

    values$g <- g
    values$dt <- m.dt[ISO3==input$selectISO3 & year==input$selectYear,
      .SD, .SDcols=-c(3,4,5,7,23:25)]

  })

  output$dtDetails <- renderRHandsontable({
    rhandsontable(values$dt[, .SD, .SDcols=-c(10:11,15:16)],
      rowHeaders=F, readOnly=T, stretchH="all",
      columnSorting=T, fixedColumnsLeft=3) %>%
      hot_cols(type="numeric", format="0.0# %", renderer=convertNA()) %>%
      hot_col("year", type="numeric", format="0") %>%
      hot_col("num_poor1", type="numeric", format="0,#", renderer=convertNA()) %>%
      hot_col("num_poor2", type="numeric", format="0,#", renderer=convertNA()) %>%
      hot_col("pcexp_ppp_m", type="numeric", format="PPP$ 0,0.0#", renderer=convertNA()) %>%
      hot_col("totpop", type="numeric", format="0,#", renderer=convertNA())

  })


  # Download handler
  output$saveData <- downloadHandler(function() {
    f <- paste0(input$selectISO3, "-", input$selectYear)
    if (input$fileType %in% c("csv", "dta")) paste0(f, ".", input$fileType)
    else paste0(f, ".zip")
  }, function(file) {
    f <- paste0(input$selectISO3, "-", input$var, ".", input$fileType)
    switch(input$fileType,
      csv = write.csv(values$dt, file, row.names=F, na=""),
      dta = foreign::write.dta(values$dt, file, version=9L),
      shp = writeRasterZip(values$g, file, f, "ESRI Shapefile")
    )
  })

})
