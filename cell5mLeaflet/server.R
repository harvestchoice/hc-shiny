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

shinyServer(function(input, output, session) {
      
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
      
      output$selectMin <- renderUI({ sliderInput("selectMin", "Minimum", 
                stats()[1, Value], stats()[6, Value], stats()[1, Value], round=T) })
      
      output$selectMax <- renderUI({ sliderInput("selectMax", "Maximum", 
                stats()[1, Value], stats()[6, Value], stats()[6, Value], round=T) })  
      
      output$varTitle <- reactive({
            ifelse(length(input$selectVar)>0, vi[input$selectVar][, varTitle], "")
          }) 
      
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
            cc <-  as.character(unlist(strsplit(vi[var()][, classColors], "|", fixed=T)))
            rg <- range(tmp$my_var, na.rm=T)
            cv <- try(classIntervals(tmp$my_var, style="kmeans")$brks)
            if (class(cv)=="try-error") {
              # Not enough data, alert, and create empty dt
              createAlert(session, "alertNoData", 
                  title="No Data!", message="Choose another combination.", type="warning", block=T)
              tmp <- data.table(X=NA, Y=NA, my_var=NA, my_col=NA) 
            } else {
              # kmeans algo worked, classify
              tmp[, my_col := cut(my_var, unique(c(rg[1]-1, cv, rg[2]+1)), cutlabels=F, ordered_result=T)]
              tmp[, my_col := colorRampPalette(cc)(length(cv)+2)[my_col]]
              tmp[is.na(my_col), my_col := "#ffffff"]
            }
            return(tmp)
          })
      
      stats <- reactive({
            tmp <- summary(dt()$my_var)
            tmp <- data.table(Statistic=names(tmp), Value=tmp)
            return(tmp)            
          })
      
      dtFilter <- reactive({
            dt()[my_var >= input$selectMin & my_var <= input$selectMax]
          })
      
      # Statistics
      output$tableSum <- renderTable(stats(), digits=0, include.rownames=F)   
      
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
                  map$setView(mean(dt()$Y, na.rm=T), mean(dt()$X+2, na.rm=T), 6)
                  
                  # Bug in Shiny causes this to error out when user closes browser            
                  map$addCircle(
                      dtFilter()$Y, dtFilter()$X, 5000, dtFilter()$CELL5M,
                      options=list(stroke=F, fillOpacity=0.55, fill=T),
                      eachOptions=list(fillColor=dtFilter()$my_col)
                  )
                })
            
            # TIL this is necessary in order to prevent the observer from
            # attempting to write to the websocket after the session is gone.
            session$onSessionEnded(paintObs$suspend)
          })
      
      # When map is clicked, show a popup with layer info
      clickObs <- observe({
            map$clearPopups()
            event <- input$map_shape_click
            if (is.null(event)) return()
            isolate({
                  map$showPopup(event$lat, event$lng, paste(
                          "Lat: ", event$lat, "<br/>",
                          "Long: ", event$lng, "<br/>",
                          "Value: ", dtFilter()[CELL5M==event$id, my_var]),
                      options=list(className=""))
                })
          })
      
      session$onSessionEnded(clickObs$suspend)
      
    })
