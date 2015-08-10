
#####################################################################################
# Helper - Make leaflet color palette
#####################################################################################
# Only because leaflet doesn't interpret "-RdYlGn" for now
leafletPal <- function(x) {
  if(substr(x, 1, 1)=="-") {
    x <- RColorBrewer::brewer.pal(9, gsub("-", "", x, fixed=T))
    x <- rev(x)
  } else {
    x <- RColorBrewer::brewer.pal(9, x)
  }
  return(x)
}

# Sensible legend wrapping
titleWrap <- function(x) gsub("\n", "<br />", stringr::str_wrap(x, 15), fixed=T)


#####################################################################################
# Helper - p1 Ruttanogram
#####################################################################################

# Theme
ap <- axis_props(
  axis=list(stroke="transparent"),
  title=list(fill="#444", font="Pt Sans", fontSize=14),
  labels=list(fill="#444", font="Pt Sans", fontSize=12),
  ticks=list(stroke="transparent"),
  grid=list(stroke="#e3e3e3", strokeWidth=1))

# Tooltips
tt <- function(x) {
  if(is.null(x)) return()
  paste(sapply(x, format), collapse="<br />")
}

p1 <- function() {
  p <- data.frame(pcn[, N:=.N, by=ISO3][N>1][sample(ISO3, 10)]) %>%
    ggvis(~gini, ~hc_poor2*100) %>%
    add_axis("x", title="Poverty HCR (%), PPP$ 2/day", title_offset=40, properties=ap) %>%
    add_axis("y", title="Gini Coefficient", title_offset=40, properties=ap) %>%
    scale_numeric("x", reverse=T) %>% scale_numeric("y", reverse=T) %>%
    layer_text(text:=~as.integer(year), dx:=8, dy:=8, fontSize=9) %>%
    layer_points(size=2, shape=~factor(country), fill:="#444", stroke:="#444") %>%
    group_by(ISO3) %>% layer_paths(stroke:="#444") %>%
    add_legend(scales="shape", title="Country") %>%
    set_options(height=420, width="auto") %>%
    add_tooltip(tt, on="hover")
  return(p)
}


#####################################################################################
# Helper - p2 Bubble chart
#####################################################################################

p2 <- function(x, var, svar) {

  names(x)[names(x)==svar] <- "value"
  tmp <- data.table(x)[, list(
    value=mean(value, na.rm=T),
    num_poor=sum(total_num_poor2, na.rm=T)), by=list(soc_d30_qtl, aez16)]

  p <- data.frame(tmp) %>%
    ggvis(~value, ~factor(aez16)) %>%
    add_axis("x", properties=ap, title="") %>%
    add_axis("y", title="", properties=ap) %>%
    group_by(soc_d30_qtl) %>%
    layer_points(stroke=~soc_d30_qtl, fill=~soc_d30_qtl, size=~num_poor) %>%
    scale_numeric("x", reverse=varList[["var"]]$rev) %>%
    add_legend(scales=c("stroke", "fill"), title="Soil Fertility",
      properties=legend_props(gradient=c("#4D4D4D", "#AEAEAE", "#E6E6E6"))) %>%
    add_legend(scales="size", title="Poverty Headcount $2/day",
      properties=legend_props(legend=list(y=90))) %>%
    set_options(height=280, width="auto", duration=0) %>%
    add_tooltip(tt, on="hover")
  return(p)
}

#####################################################################################
# Helper - p4 Data Inventory
#####################################################################################

p4 <- function() {
  dl <- data.frame(x05=2005, x08=2008, data.table(m@data)[year>0, .N, keyby=list(ISO3)])
  p <- data.frame(data.table(m@data)[year>0, .N, keyby=list(ISO3, year)]) %>%
    ggvis(~year, ~factor(ISO3)) %>%
    add_axis("y", title="Country ISO3 Code", title_offset=60, properties=ap) %>%
    add_axis("x", title="Year", title_offset=40, format="4d", properties=ap) %>%
    layer_points(size=~N, fill:="#444", stroke:="#444") %>%
    group_by(factor(ISO3)) %>% layer_paths(stroke:="#444") %>%
    add_legend(scales="size", title="Data points") %>%
    add_tooltip(tt, on="hover") %>%
    layer_paths(~x05, ~factor(ISO3), stroke:="#D83228", data=dl) %>%
    layer_paths(~x08, ~factor(ISO3), stroke:="#D83228", data=dl) %>%
    set_options(height=500, width="auto")
  return(p)
}

#####################################################################################
# Helper - Print map
#####################################################################################
printMap <- function(x, pal, var, cntr) {
  require(tmap)

  # Need to reproject
  x <- spTransform(x, CRS("+init=epsg:3857"))

  p <- tm_shape(g0) + tm_polygons(col="white", borders="grey90") +
    tm_text("ADM0_NAME", size=0.9, fontcolor="grey70") +
    tm_shape(x, is.master=T) +
    tm_fill(col="value", n=9, legend.hist=T, title=var, palette=pal, colorNA="grey90") +
    tm_text("adminUnit", size="AREA", fontcolor="white") +
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
  values <- reactiveValues(g=0L, svar=character(0), var=character(0))

  # Update year select
  observeEvent(input$selectISO3,
    updateSelectInput(session, inputId="selectYear",
      choices=if(input$selectISO3=="SSA") def else years[input$selectISO3]$year))

  # Basemap
  output$map <- renderLeaflet({
    leaflet(data=values$g) %>%
      setView(41, 1, 6) %>%  # Kenya
      addTiles(attribution="Mapbox",
        urlTemplate="http://{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png" )
  })

  # p1 - Ruttanogram
  p1() %>% bind_shiny("p1")

  # p4 - Inventory
  p4() %>% bind_shiny("p4")

  # Refresh p1
  observeEvent(input$p1Update, p1() %>% bind_shiny("p1"))

  # Helptext
  output$hText <- renderText({
    input$btn
    as.character(helpText(
      "Choose an indicator on the left and wait a few seconds for the map and graphs to render."))
  })

  # Bubble chart header
  output$svar <- renderText({
    if (class(values$g)=="integer") return()
    as.character(
      div(h3("Across Agro-Ecological Domains",
        tags$small(br(), names(iso)[iso==values$iso3], ",",
          varList[[values$var]]$name, "-", input$opts)),
        p("Showing the number of under $2/day Poor and mean",
          names(vars)[vars==values$var],
          "across agro-ecological zones and zones of low, medium and high soil
          fertility (as measured through mean organic carbon content at depth of 30 cm).")))
  })


  # obsTable - Main observer (1)
  observeEvent(input$btn, priority=3, label="obsTable", {
    # Export
    values$var <- input$var
    values$svar <- paste(input$opts, values$var, sep="_")
    values$iso3 <- input$selectISO3

    # Switch tab
    updateTabsetPanel(session, "ts", selected="Details")

    # Update helptext
    output$hText <- renderText({
      as.character(div(
        h3("Province/District Summary"),
        helpText("Selecting a row will highlight the corresponding area on the map.")))
    })

    # Subset rows
    g <- if (values$iso3=="SSA" & input$selectYear=="circa 2005")  { m[m$Y05==T,]
    } else if (values$iso3=="SSA" & input$selectYear=="circa 2008") { m[m$Y08==T, ]
    } else m[m$ISO3==values$iso3 & m$year==as.numeric(input$selectYear),]

    # Subset columns
    cols <- c("ISO3", "year", "adminUnit", "aez16", "soc_d30_qtl", "total_num_poor2",
      names(g)[names(g) %like% values$var])
    g <- g[, unique(cols)]

    # Table
    output$dtDetails <- renderRHandsontable({
      rhandsontable(g@data[, -c(4:6)], selectCallback=T,
        colHeaders=gsub("_", " ", names(g)[-c(4:6)], fixed=T),
        rowHeaders=F, readOnly=T, stretchH="all",
        height=min(420, 60+nrow(g@data)*16),
        columnSorting=T, fixedColumnsLeft=3,
        highlightCol=T, highlightRow=T) %>%
        hot_cols(type="numeric", format=varList[[values$var]]$format, renderer=convertNA()) %>%
        hot_col("year", type="numeric", format="0")
    })

    # Export
    values$g <- g

  })


  # obsMap - Main observer (2)
  observeEvent(input$btn, priority=-2, label="obsMap", {
    # Import
    g <- values$g

    # Update chart p2
    p2(g@data, values$var, values$svar) %>% bind_shiny("p2")

    # Make palette
    names(g)[names(g)==values$svar] <- "value"
    pal <- try(colorNumeric(leafletPal(varList[[values$var]]$pal), g@data$value, na.color="grey90"))
    ext <- bbox(g)

    # Add layer
    leafletProxy("map", data=g) %>%
      # Recenter map
      fitBounds(ext[1,1], ext[2,1], ext[1,2], ext[2,2]) %>%
      clearShapes() %>%

      # Add polygons
      addPolygons(stroke=F, fillOpacity=0.8, smoothFactor=1.4,
        fillColor=if(class(pal)=="try-error") "#FAF5AC" else ~pal(value),
        popup=~paste(adminUnit, prettyNum(value), sep="<br />")) %>%

      # Add legend
      addLegend("bottomright", layerId="lgd", opacity=1, pal=pal, values=~value,
        title=titleWrap(paste(varList[[values$var]]$name, input$opts, sep=" ")),
        labFormat=labelFormat(digits=if(varList[[values$var]]$format=="0,0.0# %") 2 else 0))

    })


  # obsOpts - secondary observer
  observeEvent(input$opts, label="obsOpts", {
    # Export
    values$svar <- paste(input$opts, values$var, sep="_")

    # Import
    g <- values$g
    if (class(g)=="integer") return()

    # Update chart p2
    p2(g@data, values$var, values$svar) %>% bind_shiny("p2")

    # TODO Update chart p3


    # Make palette
    names(g)[names(g)==values$svar] <- "value"
    pal <- try(colorNumeric(leafletPal(varList[[values$var]]$pal), g@data$value, na.color="grey90"))

    # Add layer
    leafletProxy("map", data=g) %>%
      clearShapes() %>%

      # Redraw polygons
      addPolygons(stroke=F, fillOpacity=0.8, smoothFactor=1.4,
        fillColor=if(class(pal)=="try-error") "#FAF5AC" else ~pal(value),
        popup=~paste(adminUnit, prettyNum(value), sep="<br />")) %>%

      # Update legend
      addLegend("bottomright", layerId="lgd", opacity=1, pal=pal, values=~value,
        title=titleWrap(paste(varList[[values$var]]$name, input$opts, sep=" ")),
        labFormat=labelFormat(digits=2))

  })


  # Highlight selected polygon on row click
  observeEvent(input$dtDetails_select$select$r, {
    # Import (note: doesn't work if rows are re-sorted)
    g <- values$g[input$dtDetails_select$select$r+1,]
    ext <- bbox(g)

    # Add to map
    leafletProxy("map", data=g) %>%
      fitBounds(ext[1,1]-1, ext[2,1]-1, ext[1,2]+1, ext[2,2]+1) %>%
      clearGroup("Selected") %>%
      addPolygons(group="Selected", smoothFactor=3,
        color="yellow", opacity=0.7, fillColor="grey50")
  })


  # Download handler
  output$saveData <- downloadHandler(function() {
    f <- paste(values$iso3, input$selectYear, input$var, Sys.Date(), sep="-")
    if (input$fileType %in% c("csv", "dta", "pdf")) paste0(f, ".", input$fileType)
    else paste0(f, ".zip")

  }, function(file) {
    # Import
    g <- values$g
    # Can't use normal tempfile() here because of spatial formats, construct
    f <- paste0(values$iso3, "-", input$selectYear, "-", values$var, ".", input$fileType)

    switch(input$fileType,
      csv = write.csv(g@data[, -c(4:6)], file, row.names=F, na=""),
      dta = foreign::write.dta(g@data[, -c(4:6)], file, version=12L),
      shp = writeRasterZip(g, file, f, "ESRI Shapefile"),
      pdf = { if (!file.exists(f)) {
        # Re-generate PDF map
        pdf(file=f, paper="letter")
        var <- paste(input$opts, values$var, sep="_")
        names(g)[names(g)==var] <- "value"
        print(printMap(g,
          varList[[values$var]]$pal,
          paste(varList[[values$var]]$name, input$opts, sep=" - "),
          paste(names(iso)[iso==values$iso3], input$selectYear, sep=", ")))
        dev.off() }
        file.copy(f, file)
      }
    )
  })

})
