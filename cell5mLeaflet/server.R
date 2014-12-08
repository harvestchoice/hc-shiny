#####################################################################################
# Title: Testing HTML Canvas with ggvis
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

shinyServer(function(input, output, session) {
      
      # Locals
      my_iso3 <- "GHA"
      
      # Create the map
      map <- createLeafletMap(session, "map")      
      
      # Create controls
      output$selectCat <- renderUI({ selectInput("selectCat", "Choose a Category", 
                vi[order(cat1), unique(cat1)][-1],
                selected="Demographics") })
      
      output$selectVar <- renderUI({ selectInput("selectVar", "Choose a Layer",
                varlst(),  
                selected="PN05_TOT") })
      
      output$selectISO3 <- renderUI({ selectInput("selectISO3", "Choose a Country", 
                iso,
                selected="GHA") })
      
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
      
      
      
      cat <- reactive({
            ifelse(length(input$selectCat)>0, input$selectCat, "Demographics")
          })      
      
      var <- reactive({
            ifelse(length(input$selectVar)>0, input$selectVar, "PN05_TOT")
          })     
      
      iso3 <- reactive({
            ifelse(length(input$selectISO3)>0, input$selectISO3, "GHA")
          })      
      
      varlst <- reactive({
            tmp <- vi[genRaster==T & type=="continuous" & cat1==cat(), varCode]
            names(tmp) <- vi[tmp][, varLabel] 
            return(tmp)
          })
      
      # query hcapi3
      dt <- reactive({
            tmp <- getLayer(var(), iso3())
            setkey(tmp, X, Y)
            setnames(tmp, 8, "my_var")
            tmp <- tmp[!is.na(my_var) | ADM1_NAME_ALT!="buffer gridcell"]
            
            # Get default symbology from `vi`
            cc <-  as.character(unlist(strsplit(vi[var()][, classColors], "|", fixed=T)))
            cv <- try(classIntervals(tmp$my_var, style="kmeans")$brks)
            
            if (class(cv)=="try-error") {
              # Not enough data, alert, and create empty dt
              createAlert(session, "alertNoData", 
                  title="No Data!",
                  message="Choose another combination.",
                  type="warning", block=T)
              tmp <- data.table(X=NA, Y=NA, my_var=NA, my_col=NA) 
              
            } else {
              # kmeans algo worked, good to classify
              rg <- range(tmp$my_var, na.rm=T)
              tmp[, my_col := cut(my_var, unique(c(rg[1]-1, cv, rg[2]+1)), cutlabels=F, ordered_result=T)]
              tmp[, my_col := colorRampPalette(cc)(length(cv)+2)[my_col]]
              tmp[is.na(my_col), my_col := "#ffffffff"]
            }
            return(tmp)
          })
      
      dtFilter <- reactive({
            dt()[my_var >= input$selectFilter[1] & my_var <= input$selectFilter[2]]
          })      
      
      stats <- reactive({
            tmp <- summary(dt()$my_var)
            tmp <- data.table(Statistic=names(tmp), Value=tmp)
            return(tmp)            
          })
      
      # Statistics
      output$tableSum <- renderTable(digits=0, include.rownames=F,
          format.args=list(big.mark=",", decimal.mark="."),
          stats())
      
      # Histogram
      output$plotHist <- renderPlot(width=220, height=220, {
            par(mar=c(2,2,0,0), bty="n", family="Helvetica-Narrow", cex.axis=.8)
            hist(dtFilter()$my_var, col=4, border="white", main=NULL, ylab=NULL, xlab=NULL)
          })
      
      # session$onFlushed is necessary to work around a bug in the Shiny/Leaflet
      session$onFlushed(once=T, function() {           
            
            paintObs <- observe({
                  
                  # Clear existing circles before drawing
                  map$clearShapes()
                  # Get data
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
            
            # This is necessary in order to prevent the observer from
            # attempting to write to the websocket after the session is gone.
            session$onSessionEnded(paintObs$suspend)
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

      output$selectDomain <- renderUI({ selectInput("selectDomain", "Choose a layer to summarize by",
                domlst())
          })
      
      output$tableDomain <- renderTable(digits=0, include.rownames=F,
          format.args=list(big.mark=",", decimal.mark="."), {
            input$btnDomain
            isolate(getLayer(var(), iso3(), by=domby()))
          })       
      
      domlst <- function() {
        tmp <- c("ADM1_NAME_ALT", vi$varCode[sample(1:600, 10)])
        names(tmp) <- vi[tmp][, varLabel] 
        return(tmp)
      }
      
      domby <- reactive({
            ifelse(length(input$selectDomain)>0, input$selectDomain, "ADM1_NAME_ALT")
          })        
      
      output$txtDomain <- reactive({
            # Bound to btnMapDomain
            if (input$btnMapDomain==0) return()
            
            isolate({
                  # Clear existing circles before drawing
                  map$clearShapes()
                  
                  # Summarize layer
                  tmp <- getLayer(var(), iso3(), by=domby(), collapse=F)
                  setkey(tmp, X, Y)
                  setnames(tmp, 5, "my_var")
                  tmp <- tmp[!is.na(my_var)]
                  
                  # Get default symbology from `vi`
                  cc <-  as.character(unlist(strsplit(vi[var()][, classColors], "|", fixed=T)))
                  cv <- classIntervals(tmp$my_var, length(cc))$brks
                  rg <- range(tmp$my_var, na.rm=T)
                  tmp[, my_col := cut(my_var, unique(c(rg[1]-1, cv, rg[2]+1)), cutlabels=F, ordered_result=T)]
                  tmp[, my_col := colorRampPalette(cc)(length(cv)+2)[my_col]]
                  tmp[is.na(my_col), my_col := "#ffffff"]        
                  
                  # Draw circles
                  map$addCircle(
                      tmp$Y, tmp$X, 5000, tmp$CELL5M,
                      options=list(stroke=F, fillOpacity=0.55, fill=T),
                      eachOptions=list(fillColor=tmp$my_col))
                  
                  # Check
                  return("Done!")
                })
          })
      
      session$onSessionEnded(clickObs$suspend)
    })
