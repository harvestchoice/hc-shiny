
#####################################################################################
# Helper - Print map
#####################################################################################
printMap <- function(x, var, cntr) {
  require(tmap)

  # Need to reproject
  x <- spTransform(x, CRS("+init=epsg:3857"))

  p <- tm_shape(g0) + tm_polygons(col="white", borders="grey90") +
    tm_text("ADM0_NAME", size=0.9, fontcolor="grey70") +
    tm_shape(x, is.master=T) +
    tm_fill(col="value", n=7, legend.hist=T, title=var, palette=pal, colorNA="grey90") +
    tm_text("prttyNm", size="AREA", fontcolor="white") +
    tm_credits("IFPRI/HarvestChoice, 2015. www.harvestchoice.org") +
    tm_layout(title=cntr, title.size=1.2, bg.color="#5daddf",
      inner.margin=c(0,0.3,0,0), legend.position=c(0.02, 0.02))

  return(p)
}


#####################################################################################
# Helper - Archive spatial formats for download
#####################################################################################
writeRasterZip <- function(x, file, filename, format, ...) {
  writeOGR(x, "./", filename, format, overwrite_layer=T, check_exists=T)
  f <- list.files(pattern=paste0(strsplit(filename, ".", fixed=T)[[1]][1], ".*"))
  zip(paste0(filename, ".zip"), f, flags="-9Xjm", zip="zip")
  file.copy(paste0(filename, ".zip"), file)
  file.remove(paste0(filename, ".zip"))
}



#####################################################################################
# Main
#####################################################################################
shinyServer(function(input, output, session) {

  # Init values
  values <- reactiveValues(g=0L)

  # Update year select
  observeEvent(input$selectISO3, {
    updateSelectInput(session, inputId="selectYear",
      choices = if (input$selectISO3=="SSA") { def
      } else unique(m@data[m$ISO3==input$selectISO3,][sort(-m$year), "year"])
    )
  })


  # Basemap
  output$map <- renderLeaflet({
    leaflet(data=values$g) %>%
      setView(41, 1, 6) %>%  # Kenya
      addTiles(attribution="Mapbox",
        urlTemplate="http://{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png" )
  })


  # Update map
  observeEvent(input$btn, {

    values$var <- input$var

    # Subset map
    g <- if (input$selectISO3=="SSA" & input$selectYear=="circa 2005")  { m[m$Y05==T,]
    } else if (input$selectISO3=="SSA" & input$selectYear=="circa 2008") { m[m$Y08==T, ]
    } else m[m$ISO3==input$selectISO3 & m$year==as.numeric(input$selectYear),]

    # keep only few columns
    g <- g[, c("ISO3", "year", "prttyNm", names(g)[names(g) %like% values$var])]
    coords <- apply(bbox(g), FUN=mean, MARGIN=1)

    # Save selected var
    var <- paste(input$opts, values$var, sep="_")

    # Make palette
    names(g)[names(g)==var] <- "value"
    pal <- colorNumeric(pal, g@data$value, na.color="grey90")

    # Add layer
    leafletProxy("map", data=g) %>%
      # Recenter map if country has changed
      setView(coords[[1]], coords[[2]], 6) %>%
      clearShapes() %>%

      # Add polygons
      addPolygons(stroke=F, fillOpacity=0.5, fillColor=~pal(value),
        smoothFactor=2.2,
        popup=~paste(prttyNm, prettyNum(value), sep="<br />")) %>%

      # Add legend
      addLegend("bottomright", layerId="lgd", opacity=1, pal=pal, values=~value,
        title=paste(values$var, input$opts, sep=" - "),
        labFormat=labelFormat(digits=2))

    names(g)[names(g)=="value"] <- var
    values$g <- g
  })


  # Table
  observeEvent(values$g, {
    g <- values$g

    if (class(g)=="integer") {
      # show help text
      output$hText <- renderText({ as.character(
        helpText("Click", tags$b("Map Indicator"), "to view summary table."))
      })

    } else {
      # Show table
      output$dtDetails <- renderRHandsontable({
        t <- rhandsontable(g@data,
          rowHeaders=F, readOnly=T, stretchH="all", height=min(400, 60+nrow(g@data)*14),
          columnSorting=T, fixedColumnsLeft=3,
          highlightCol=T, highlightRow=T)

        # Format numbers
        if (values$var %in% c("num_poor1", "num_poor2", "totpop")) {
          t <- t %>% hot_cols(type="numeric", format="0,#", renderer=convertNA())
        } else if (values$var %like% "exp") {
          t <- t %>% hot_cols(type="numeric", format="PPP$ 0,0.0#", renderer=convertNA())
        } else {
          t <- t %>% hot_cols(type="numeric", format="0,0.0# %", renderer=convertNA())
        }

        # Return
        t %>% hot_col("year", type="numeric", format="0")
      })
    }
  })


  # Re-symbolize on opts
  observeEvent(input$opts, {
    g <- values$g
    if (class(g)=="integer") return()

    var <- paste(input$opts, values$var, sep="_")
    names(g)[names(g)==var] <- "value"

    # Make palette
    pal <- colorNumeric(pal, g@data$value, na.color="grey90")

    # Add layer
    leafletProxy("map", data=g) %>%
      clearShapes() %>%

      # Redraw polygons
      addPolygons(stroke=F, fillOpacity=0.5, fillColor=~pal(value),
        smoothFactor=2.2,
        popup=~paste(prttyNm, prettyNum(value), sep="<br />")) %>%

      # Update legend
      addLegend("bottomright", layerId="lgd", opacity=1, pal=pal, values=~value,
        title=paste(values$var, input$opts, sep=" - "),
        labFormat=labelFormat(digits=2))

  })


  # Download handler
  output$saveData <- downloadHandler(function() {
    f <- paste0(input$selectISO3, "-", input$selectYear, "-", input$var)
    if (input$fileType %in% c("csv", "dta", "pdf")) paste0(f, ".", input$fileType)
    else paste0(f, ".zip")

  }, function(file) {
    g <- values$g
    f <- paste0(input$selectISO3, "-", input$selectYear, "-", input$var, ".", input$fileType)

    switch(input$fileType,
      csv = write.csv(g@data, file, row.names=F, na=""),
      dta = foreign::write.dta(g@data, file, version=12L),
      shp = writeRasterZip(g, file, f, "ESRI Shapefile"),
      pdf = {
        if (!file.exists(f)) {
          # Re-generate PDF
          pdf(file=f, paper="letter")
          var <- paste(input$opts, values$var, sep="_")
          names(g)[names(g)==var] <- "value"
          print(printMap(g,
            paste(names(vars)[vars==input$var], input$opts, sep=" - "),
            paste(names(iso)[iso==input$selectISO3], input$selectYear, sep=", ")))
          dev.off()
        }
        file.copy(f, file)
      }
    )
  })

})
