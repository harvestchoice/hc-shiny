#####################################################################################
# Title: Testing Interactive Viz with leaflet
# Date: December 2014
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

library(data.table)
library(shiny)
library(shinyBS)
library(hcapi3)
library(leaflet)
library(classInt)

setwd("/home/projects/www/tmp")

# Query hcapi3 and symbolize
getCircles <- function(var="PN05_TOT", iso3="GHA", ...) {
  tmp <- getLayer(var, iso3, ...)
  setkey(tmp, X, Y)
  setnames(tmp, length(names(tmp)), "my_var")
  tmp <- tmp[!is.na(my_var) | ADM1_NAME_ALT!="buffer gridcell"]
  
  # Get default symbology from `vi`
  cc <-  as.character(unlist(strsplit(vi[var][, classColors], "|", fixed=T)))
  cv <- try(classIntervals(tmp$my_var, style="kmeans")$brks)
  
  if (class(cv)=="try-error") {
    # Not enough data for kmeans, alert, and create empty data.table
    createAlert(session, "alertNoData", 
        title="No Data!",
        message="Choose another combination.",
        type="warning", block=T)
    tmp <- data.table(X=NA, Y=NA, my_var=NA, my_col=NA) 
    
  } else {
    # kmeans algo worked, good to classify
    rg <- range(tmp$my_var, na.rm=T)
    tmp[, my_col := cut(my_var, unique(c(rg[1]-1, cv, rg[2]+1)), cutlabels=F, ordered_result=T)]
    tmp[, my_col := colorRampPalette(cc)(length(cv)+1)[my_col]]
    tmp[is.na(my_col), my_col := "#ffffff"]
  }
  return(tmp)
}



shinyServer(function(input, output, session) {
      
      # Init some variables
      my_iso3 <- "GHA"
      my_var <- "PN05_TOT"
      my_dom <- "ADM1_NAME_ALT"      
      
      # Create the map
      map <- createLeafletMap(session, "map")      
      
      # Create controls
      output$selectCat <- renderUI({ selectInput("selectCat", "Choose a Category", 
                vi[order(cat1), unique(cat1)][-1],
                selected="Demographics") })
      
      output$selectVar <- renderUI({ selectInput("selectVar", "Choose a Layer",
                varlst(),  
                selected=my_var) })
      
      output$selectISO3 <- renderUI({ selectInput("selectISO3", "Choose a Country", 
                iso,
                selected=my_iso3) })
      
      output$selectFilter <- renderUI({ sliderInput("selectFilter", "Filter layer to Min/Max", 
                stats()[1, Value], stats()[6, Value],
                c(stats()[1, Value], stats()[6, Value]), round=T) }) 
      
      output$varTitle <- reactive({
            ifelse(length(input$selectVar)>0, vi[input$selectVar][, varTitle], "")
          }) 
      
      output$saveData <- downloadHandler(
          function() paste("data-", Sys.Date(), ".zip", sep=""),
          function(file) file.copy(genFile(var(), iso3(), format=input$fileType), file)
      )
      
      output$tableSum <- renderTable(digits=0, include.rownames=F,
          format.args=list(big.mark=",", decimal.mark="."),
          stats())
      
      output$plotHist <- renderPlot(width=220, height=220, {
            par(mar=c(2,2,0,0), bty="n", family="Helvetica-Narrow", cex.axis=.8)
            hist(dtFilter()$my_var, col=4, border="white", main=NULL, ylab=NULL, xlab=NULL)
          })      
      
      # Not sure that's the right way to init variables
      cat <- reactive({
            ifelse(length(input$selectCat)>0, input$selectCat, "Demographics")
          })      
      
      var <- reactive({
            ifelse(length(input$selectVar)>0, input$selectVar, my_var)
          })     
      
      iso3 <- reactive({
            ifelse(length(input$selectISO3)>0, input$selectISO3, my_iso3)
          })      
      
      varlst <- reactive({
            tmp <- vi[genRaster==T & type=="continuous" & cat1==cat(), varCode]
            names(tmp) <- vi[tmp][, varLabel] 
            return(tmp)
          })
      
      # Query and symbolize layer
      dt <- reactive({
            getCircles(var(), iso3())
          })
      
      # Filter layer
      dtFilter <- reactive({
            tmp <- input$selectFilter
            dt()[my_var >= tmp[1] & my_var <= tmp[2]]
          })   
      
      # Compute 5 stats
      stats <- reactive({
            tmp <- summary(dt()$my_var)
            tmp <- data.table(Statistic=names(tmp), Value=tmp)
            return(tmp)            
          })
      
      # Draw raster
      drawObs <- observe({
            
            # Clear existing circles before drawing
            map$clearShapes()
            tmp <- dtFilter()
            
            if ( !identical(my_iso3, iso3()) ) {
              # Recenter map only if country has changed
              map$setView(mean(tmp$Y, na.rm=T), mean(tmp$X+2, na.rm=T), 6)
              my_iso3 <<- iso3()
              
              # Draw circles
              map$addCircle(
                  tmp$Y, tmp$X, 5000, tmp$CELL5M,
                  options=list(stroke=F, fillOpacity=0.55, fill=T),
                  eachOptions=list(fillColor=tmp$my_col)
              )
              
            } else {
              # Draw circles (placeholder for optimized code)
              map$addCircle(
                  tmp$Y, tmp$X, 5000, tmp$CELL5M,
                  options=list(stroke=F, fillOpacity=0.55, fill=T),
                  eachOptions=list(fillColor=tmp$my_col))
            }
          })
      
      # When map is clicked, show a popup with layer info
      clickObs <- observe({
            map$clearPopups()
            event <- input$map_shape_click
            if (is.null(event)) return()
            isolate({
                  tmp <- dtFilter()[CELL5M==event$id]
                  map$showPopup(event$lat, event$lng, paste(
                          "CELL5M: ", event$id, "<br/>",
                          "Lat: ", event$lat, "<br/>",
                          "Long: ", event$lng, "<br/>",
                          "Province: ", tmp$ADM1_NAME_ALT, "<br/>",
                          "District: ", tmp$ADM2_NAME_ALT, "<br/>",
                          "Value: ", tmp$my_var, " ", vi[var()][, unit]))
                })
          })
      
      
      ################################################################################
      # Domain Summary
      ################################################################################
      
      output$selectDomain <- renderUI({ selectInput("selectDomain",
                "Choose a layer to summarize by", domlst())
          })
      
      output$tableDomain <- renderTable(digits=0, include.rownames=F,
          format.args=list(big.mark=",", decimal.mark="."), {
            # Bound to btnDomain
            if (input$btnDomain==0) return()
            isolate(setnames(
                    dtDomain()[, .SD, .SDcols=-c("X", "Y", "my_col")],
                    2, vi[var()][, varLabel]))
          })   
      
      domlst <- function() {
        # Just select random 10 variables to test
        tmp <- c(my_dom, vi$varCode[sample(1:600, 10)])
        names(tmp) <- vi[tmp][, varLabel] 
        return(tmp)
      }
      
      domby <- reactive({
            ifelse(length(input$selectDomain)>0, input$selectDomain, my_dom)
          })
      
      # Summarize layer
      dtDomain <- reactive({
            getCircles(c("X", "Y", var()), iso3(), domby())
          })
      
      drawObsDomain <- observe({
            # Bound to btnMapDomain
            if (input$btnMapDomain==0) return()
            
            isolate({
                  # Clear existing circles before drawing
                  map$clearShapes()
                  # Summarize and symbolize layer
                  tmp <- getCircles(var(), iso3(), by=domby(), collapse=F)
                  # Draw circles
                  map$addCircle(
                      tmp$Y, tmp$X, 5000, tmp$CELL5M,
                      options=list(stroke=F, fillOpacity=0.55, fill=T),
                      eachOptions=list(fillColor=tmp$my_col))
                })
          })
    })
