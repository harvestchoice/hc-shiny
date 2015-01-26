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
library(leaflet, lib.loc="/usr/lib64/R/library")
library(RColorBrewer)

if (.Platform$OS.type=="windows") {
  setwd("~/Projects/hc-shiny/tmp")
} else {
  setwd("/home/projects/shiny/tmp")
}


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
genMap <- function(svy, res, var, col, brks) {
  dt <- dhs[svyCode==svy & hv025==res, .SD, .SDcols=c("hv024", var)]
  setnames(dt, var, "var")
  setkey(dt, hv024)
  m <- gis.web[gis.web$svyCode==svy, ]
  dt <- dt[J(m$regCode)]
  rg <- range(dt$var, na.rm=T)
  
  # Try equal interval breaks
  cv <- try(classInt::classIntervals(dt$var, n=min(brks, dt[, length(unique(var))], na.rm=T))$brks)
  
  if (class(cv)=="try-error") {
    return(class(cv))
    
  } else {
    # Symbolize
    dt[, cl := cut(var, unique(c(rg[1]-1, cv, rg[2]+1)), cutlabels=F, ordered_result=T)]
    dt[, cl := brewer.pal(length(cv)+1, col)[cl]]
    m@data <- cbind(m@data, dt)
    return(m)
  }
}



shinyServer(function(input, output, session) {
    
    # Init map
    map <- leaflet() %>%
      addTiles("http://{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution=HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>')) %>%
      setView(8, 8, 6)
    
    # Reactive controls
    output$map <- renderLeaflet(map)
    
    output$selectCat <- renderUI({
        selectInput("selectCat", "Choose a Category", dhs.lbl[, unique(varCat)],
          selected="wealth index")
      })
    
    output$selectVar <- renderUI({
        var <- dhs.lbl[varCat==input$selectCat, varCode]
        names(var) <- dhs.lbl[varCat==input$selectCat, varLabel]
        selectInput("selectVar", "Choose an Indicator", var, selected=var[1])
      })
    
    output$selectISO <- renderUI({
        selectInput("selectISO", "Choose a Country", iso, selected="GH")
      })
    
    output$col <- renderUI({
        selectInput("col", "Color palette", row.names(brewer.pal.info),
          selected="RdBu")
      })
    
    output$svydt <- renderTable(digits=1, include.rownames=F, dt1())
    
    # Reactive values
    init <- reactive(input$btn+input$btnUpdate) 
    
    # Reactive data tables
    dt1 <- eventReactive(input$btn, {
        var <- names(dhs)[names(dhs) %like% input$selectVar]
        dt <- dhs[country_code==input$selectISO, .SD, .SDcols=c("year", "hv025", "hv024", var)]
        if (dim(dt)[2]>4) setnames(dt, 4:5, c("Female", "Male")) else setnames(dt, 4, "mean")
        dt <- melt(dt, id.vars=c("year", "hv025", "hv024"))
        dt <- dcast.data.table(dt, hv024~year+hv025+variable)
        setnames(dt, gsub("_", " ", names(dt), fixed=T))
        setnames(dt, 1, c("Region"))
        return(dt)
      })
    
    dt2 <- reactive({
      })
    
    observeEvent(input$btn, {
        # Update year
        y <- svyYear[[input$selectISO]]
        updateRadioButtons(session, "selectYear", choices=y, selected=tail(y,1), inline=T)
        # Update gender
        if (dhs.lbl[varCode==input$selectVar, gender]==T) {
          updateRadioButtons(session, "selectGender", choices=c(male="_m", female="_f"), selected="_f")
        } else {
          updateRadioButtons(session, "selectGender", choices=c(`n/a`=""), selected="")
        }
      }, priority=3)
    
    
    # Update map    
    observeEvent({input$btn+input$btnUpdate}, {       
        svyCode <- paste0(input$selectISO, input$selectYear)
        
        if (!svyCode %in% unique(gis.web@data$svyCode)) {
          # File is missing for that country
          createAlert(session, "alertNoData",
            message="Try another combination.",
            title="Missing Map Data", type="warning", block=T, append=F)
          
        } else {
          g <- genMap(svyCode, input$selectRes, paste0(input$selectVar, input$selectGender),
            input$col, input$brks)
          coords <- apply(sp::coordinates(g), 2, mean, na.rm=T)
          m <- map %>%
            setView(coords[1], coords[2], 6) %>%
            addPolygons(data=g, layerId=g@data$regCode, fillColor=g@data$cl,
              weight=.6, color="white", fillOpacity=0.7,
              popup=paste0(
                "<small>Region</small><strong><br/>", g@data$regName, "</strong><br/>",
                "<small>Value</small><strong><br/>", round(g@data$var, 2), "</strong>"))
          output$map <- renderLeaflet(m)
        }
      }, priority=2)
    
    
    # Show selected
    output$details <- renderText({
        my_iso <- input$selectISO
        my_iso <- names(iso)[iso==my_iso]
        my_var <- input$selectVar 
        my_var <- dhs.lbl[varCode==my_var, varLabel]
        out <- h3(my_iso, br(), tags$small(my_var))
        return(as.character(out))
      })
    
    # Show admin details on click
    output$tips <- renderText({
        evt <- input$map_geojson_mouseclick
        if (!is.null(evt)) {
          out <- div(
            p(evt$properties$regName, " (code: ", evt$properties$regCode, ")"),
            hr(),
            p("Survey: ", strong(svyCode()), br(),
              "Year: ", strong(evt$properties$year), br(),
              "Value: ", strong(evt$properties$value)))
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
    
    
    # Download
    output$saveData <- downloadHandler(function() {
        t <- input$fileType
        f <- paste0(input$selectISO, "-", input$selectVar)
        if (t %in% c("csv", "dta")) {
          # Complete file path
          paste0(f, ".", t)
        } else {
          # File path with `.zip`
          paste0(year(), "-", f, ".", t, ".zip")
        }
      }, function(file) {
        t <- input$fileType
        f <- paste0(input$selectISO, "-", input$selectVar, ".", t)
        
        switch(t,
          csv = write.csv(dt1(), file, row.names=F, na=""),
          dta = foreign::write.dta(dt1(), file, version=9L),
          shp = {
            # TODO Use original boundaries for download
            g <- genMap(svyCode, input$selectRes, paste0(input$selectVar, input$selectGender),
              input$col, input$brks)
            writeZip(g, file, f, "ESRI Shapefile")
          })
      })
    
  })
