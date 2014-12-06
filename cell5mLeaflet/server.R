#####################################################################################
# Title: Testing HTML Canvas with ggvis
# Date: December 2014
# Project: HarvestChoice for the Bill and Melinda Gates Foundation
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

library(data.table)
library(shiny)
library(hcapi3)
library(leaflet)
library(RColorBrewer)
library(classInt)
#library(ggvis)

setwd("/home/projects/www/cell5mLeaflet")


# Define server logic for slider examples
shinyServer(function(input, output, session) {
      
      output$selectCat <- renderUI({ selectInput("selectCat", "Choose a Category", 
                vi[order(cat1), unique(cat1)],
                selected="Demographics") })
      
      output$selectVar <- renderUI({ selectInput("selectVar", "Choose a Layer",
                varlst(),  
                selected="PN05_TOT") })
      
      output$selectISO3 <- renderUI({ selectInput("selectISO3", "Choose a Country", 
                iso,
                selected="GHA") })
      
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
      
      dt <- reactive({
            tmp <- hcapi3::getLayer(var(), iso3())
            tmp <- tmp[ADM1_NAME_ALT!="buffer gridcell"]
            setkey(tmp, X, Y)
            setnames(tmp, 8, "my_var")
            cv <- classIntervals(tmp$my_var, style="kmeans")$brks
            tmp[, my_col := cut(my_var, unique(cv), cutlabels=F)]
            tmp[, my_col := brewer.pal(8, "OrRd")[my_col]]
            tmp[is.na(my_col), my_col := "#ffffff"]
            return(tmp)
          })
      
      # Statistics
      output$tableSum <- renderTable(include.rownames=F, {
            t(summary(dt()[["my_var"]]))  
          })
      
#      # Redraw histogram
#      dtf %>%
#          ggvis(~my_var) %>%
#          layer_histograms(fill.hover:="blue", width=0.1) %>%
#          set_options(width=320, height=180) %>%
#          bind_shiny("hist", "hist_ui")       
      
      
      # Create the map
      map <- createLeafletMap(session, "map")
      
      # session$onFlushed is necessary to work around a bug in the Shiny/Leaflet
      session$onFlushed(once=T, function() {
            paintObs <- observe({                 
                  
                  # Clear existing circles before drawing
                  map$clearShapes()
                  map$setView(mean(dt()$Y, na.rm=T), mean(dt()$X+2, na.rm=T), 6)
                  
                  # Draw in batches of 1000; makes the app feel a bit more responsive
                  chunksize <- 1000
                  for (from in seq.int(1, nrow(dt()), chunksize)) {
                    to <- min(nrow(dt()), from + chunksize)
                    chunk <- dt()[from:to]
                    # Bug in Shiny causes this to error out when user closes browser
                    # before we get here
                    try(map$addCircle(
                            chunk$Y, chunk$X, 5000,
                            options=list(stroke=F, fillOpacity=0.65, fill=T),
                            eachOptions=list(color=dt()$my_col)
                        )
                    )
                  }
                })
            
            # TIL this is necessary in order to prevent the observer from
            # attempting to write to the websocket after the session is gone.
            session$onSessionEnded(paintObs$suspend)
          })       
      
    })
