

# Helper - Print map
printMap <- function(x) {
  require(tmap)

  tm_shape(x, is.master=T) +
    tm_fill(col="value", n=8, legend.hist=T, title="value",
      palette=rev(RColorBrewer::brewer.pal(8, "RdYlGn")), colorNA="grey90") +
    tm_shape(g0) + tm_borders(col="grey90") +
    tm_credits("IFPRI/HarvestChoice, 2015. www.harvestchoice.org", position="right")
}

# Helper - Archive spatial formats for download
writeRasterZip <- function(x, file, filename, format, ...) {
  writeOGR(x, "./", filename, format, overwrite_layer=T, check_exists=T)
  f <- list.files(pattern=paste0(strsplit(filename, ".", fixed=T)[[1]][1], ".*"))
  zip(paste0(filename, ".zip"), f, flags="-9Xjm", zip="zip")
  file.copy(paste0(filename, ".zip"), file)
  file.remove(paste0(filename, ".zip"))
}


shinyServer(function(input, output, session) {

  # Init values
  values <- reactiveValues(
    g=m[m$ISO3=="KEN" & m$Y08==T,]
  )

  # Update year select
  observeEvent(input$selectISO3, {
    updateSelectInput(session, inputId="selectYear",
      choices = if (input$selectISO3=="SSA") { def
      } else sort(decreasing=T,
        unique(m@data[m$ISO3==input$selectISO3,][order(-m$year), "year"]))
    )
  })

  output$map <- renderLeaflet({

    # Palette
    pal_def <- colorNumeric(pal, c(0.17,0.98), na.color="grey90")

    # Basemap
    leaflet(data=values$g) %>%
      setView(41, 1, 6) %>%  # Kenya
      addTiles(attribution="Mapbox",
        urlTemplate="http://{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png" ) %>%

      # Add polygons
      addPolygons(stroke=F, fillOpacity=0.6, fillColor=~pal_def(hc_poor2),
        popup=~paste(prttyNm, prettyNum(hc_poor2), sep="<br />")) %>%

      # Add legend
      addLegend("bottomright", opacity=1, pal=pal_def, values=~hc_poor2,
        title="hc_poor2", labFormat=labelFormat(digits=0, transform=perct))
  })

  # Update map
  observeEvent(input$btn, {

    # Subset map
    g <- if (input$selectISO3=="SSA" & input$selectYear=="circa 2005")  { m[m$Y05==T,]
    } else if (input$selectISO3=="SSA" & input$selectYear=="circa 2008") { m[m$Y08==T, ]
    } else m[m$ISO3==input$selectISO3 & m$year==as.numeric(input$selectYear),]

    names(g)[which(names(g)==input$var)] <- "value"
    coords <- apply(bbox(g), FUN=mean, MARGIN=1)

    # Make palette
    pal_react <- colorNumeric(pal, g@data$value, na.color="grey90")

    # Add layer
    leafletProxy("map", data=g) %>%
      # Recenter map if country has changed
      setView(coords[[1]], coords[[2]], 6) %>%
      clearShapes() %>%
      clearControls() %>%

      # Add polygons
      addPolygons(stroke=F, fillOpacity=0.5, fillColor=~pal_react(value),
        smoothFactor=2,
        popup=~paste(prttyNm, prettyNum(value), sep="<br />")) %>%

      # Add legend
      addLegend("bottomright", opacity=1, pal=pal_react, values=~value,
        title=input$var, labFormat=labelFormat(digits=0, transform=perct))

    names(g)[which(names(g)=="value")] <- input$var
    values$g <- g
  })

  # Table
  output$dtDetails <- renderRHandsontable({
    g <- values$g@data

    isolate({
      rhandsontable(g[, -c(3,4,5,7,23:25, 19:20)],
        rowHeaders=F, readOnly=T, stretchH="all",
        columnSorting=T, fixedColumnsLeft=3,
        highlightCol=T, highlightRow=T) %>%
        hot_cols(type="numeric", format="0.0# %", renderer=convertNA()) %>%
        hot_col("year", type="numeric", format="0") %>%
        hot_col("num_poor1", type="numeric", format="0,#", renderer=convertNA()) %>%
        hot_col("num_poor2", type="numeric", format="0,#", renderer=convertNA()) %>%
        hot_col("pcexp_ppp_m", type="numeric", format="PPP$ 0,0.0#", renderer=convertNA()) %>%
        hot_col("totpop", type="numeric", format="0,#", renderer=convertNA())
    })
  })


  # Download handler
  output$saveData <- downloadHandler(function() {
    f <- paste0(input$selectISO3, "-", input$selectYear)
    if (input$fileType %in% c("csv", "dta")) paste0(f, ".", input$fileType)
    else paste0(f, ".zip")

  }, function(file) {
    g <- values$g
    f <- paste0(input$selectISO3, "-", input$var, ".", input$fileType)

    switch(input$fileType,
      csv = write.csv(g@data, file, row.names=F, na=""),
      dta = foreign::write.dta(g@data, file, version=12L),
      shp = writeRasterZip(g, file, f, "ESRI Shapefile"),
      png = {
        png(file, width=6, height=6, units="in", res=300)
        printMap(g)
        dev.off()
      }
    )
  })

})
