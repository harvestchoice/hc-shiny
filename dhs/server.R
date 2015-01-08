#####################################################################################
# Title: Visualize DHS Regional Estimates
# Date: Januray 2015
# Project: HarvestChoice for A4NH
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

library(shiny)
library(shinyBS)
library(data.table)
library(reshape2)
library(leaflet)
library(RColorBrewer)

setwd("/home/projects/shiny/tmp")
load("../dhs/data/dhsMap.2014.10.16.RData")


# Helper - Archive spatial formats for download
writeZip <- function(x, file, filename, format, ...) {
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


# Helper - Merge attributes and symbolize
genMap <- function(var, iso, year, cl, cv) {

}



shinyServer(function(input, output, session) {

  # Leaflet map
  map <- createLeafletMap(session, "map")

  # Reactive controls
  output$selectVar <- renderUI({
    selectInput("selectVar", "Choose an Indicator", dhs.lbl, selected="i101_hv271_wealth_index")
  })

  output$selectISO <- renderUI({
    selectInput("selectISO", "Choose a Country", iso, selected="GH")
  })

  output$cl <- renderUI({
    selectInput("cl", "Choose a Color Palette", row.names(brewer.pal.info), selected="RdBu")
  })

  output$selectYear <- renderUI({
    svy <- svyYear[iso==input$selectISO]
    sliderInput("selectYear", "Select Survey Year", format="###0",
      svy$yearStart, svy$yearEnd, svy$yearStart)
  })

  # Reactive values
  var <- reactive({
    if (input$btn>0) isolate(input$selectVar) else "i101_hv271_wealth_index"
  })

  iso2 <- reactive({
    if (input$btn>0) isolate(input$selectISO) else "GH"
  })

  year <- reactive({
    if(length(input$selectYear)>0) input$selectYear else 2008
  })

  svyCode <- reactive({
    if(input$btn>0) paste0(iso2(), year()) else "GH2008"
  })

  g <- reactive({
    # Selected country boundaries
    if (input$btn==0) return()
    gis[gis$svyCode==svyCode(),]
  })


  # Reactive data tables
  dt1 <- reactive({
    if (input$btn==0) return()
  })

  dt2 <- reactive({
  })



  # Draw GeoJSON
  observe({
    if (input$btn==0) return()
    # Symbolize and read country GeoJSON from disk
    m <<- try(readRDS(genMap(svyCode())))
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


  # Show admin details on click
  output$details <- renderText({
    evt <- input$map_geojson_mouseclick
    if (is.null(evt)) {
      out <- div(h3(cntr()), p("Click a region to view details."))
    } else {
      out <- div(
        h3(cntr(), br(), tags$small(evt$properties$DHSREGEN, ", ", evt$properties$ISO)),
        hr(),
        h4(names(d)[d==var()]),
        p("Mean: ", strong(evt$properties$iso), br(),
          "Min: ", strong(evt$properties$svyYear), br(),
          "Max: ", strong(evt$properties$DHSREGEN), br(),
          "Sd. Dev.: ", strong(evt$properties$svyUnit)))
    }
    return(as.character(out))
  })


  # Plot #1
  output$plot1 <- renderPlot({
    if(input$btn==0) return()
  })


  # Plot #2
  output$plot2 <- renderPlot({
    if(input$btn==0) return()
  })

  # Brewer color palettes
  output$plotBrewer <- renderPlot(height=500, {
    if(input$btnShowBrewer==0) return()
    par(mar=c(0,3,0,0))
    display.brewer.all()
  })


  # Survey details
  output$svydt <- renderTable({

  })


  # Download
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

    f <- paste0(cntr(), "-", var(), ".", input$fileType)

    switch(input$fileType,
      csv = write.csv(dt2(), file, row.names=F, na=""),
      dta = foreign::write.dta(dt2(), file, version=9L),
      shp = {
        # Combine attributes with admin boundaries
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
        writeZip(g, file, f, "ESRI Shapefile")
      })
  })

})
