#####################################################################################
# Title: Testing HTML Canvas with ggvis
# Date: December 2014
# Project: HarvestChoice for the Bill and Melinda Gates Foundation
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

library(data.table)
library(shiny)
library(ggvis)
library(leaflet)
library(RColorBrewer)
library(classInt)
library(hcapi3,  lib.loc="/usr/lib/opencpu/library")


# Define server logic for slider examples
shinyServer(function(input, output, session) {
      
      cat <- reactive({
            ifelse(length(input$selectCat)>0, input$selectCat, "Demographics")
          })      
      
      var <- reactive({
            ifelse(length(input$selectVar)>0, input$selectVar, "PN05_TOT")
          })
      
      iso3 <- reactive({
            ifelse(length(input$selectISO3)>0, input$selectISO3, "ETH")    
          })      
      
      varlst <- reactive({
            tmp <- vi[genRaster==T & cat1==cat(), varCode]
            names(tmp) <- vi[tmp][, varLabel] 
            return(tmp)
          })
      
      output$selectCat <- renderUI({ selectInput("selectCat", "Choose a Category", 
                vi[order(cat1), unique(cat1)],
                selected="Demographics") })      
      
      output$selectVar <- renderUI({ selectInput("selectVar", "Choose a Layer",
                varlst(),  
                selected="PN05_TOT") })
      
      output$selectISO3 <- renderUI({ selectInput("selectISO3", "Choose a Country", 
                iso,
                selected="ETH") })
      
      dt <- reactive({
            tmp <- getLayer(var(), iso3())
            tmp <- tmp[ADM1_NAME_ALT!="buffer gridcell"]
            setkey(tmp, X, Y)
            setnames(tmp, 8, "my_var")
#            cv <- classIntervals(tmp$my_var, style="kmeans")$brks
#            tmp[, my_col := cut(my_var, unique(cv), cutlabels=F)]
#            tmp[, my_col := brewer.pal(8, "OrRd")[my_col]]
            return(as.data.frame(tmp))
          })
      
      output$tableSum <- renderTable(include.rownames=F, {
            t(summary(dt()[, "my_var"]))  
          })
      
      # Draw histogram
      dt %>% ggvis(~my_var) %>%
          layer_histograms(fill.hover:="blue", width=0.1) %>%
          set_options(width=320, height=180) %>%
          bind_shiny("hist", "hist_ui")       
      
#      dt.agg <- reactive({
#            by <- ifelse(iso3()=="SSA", "ADM0_NAME", "ADM1_NAME_ALT")
#            tmp <- getLayer(var(), iso3(), by=by)
#            setkey(tmp, ADM1_NAME_ALT)
#            setnames(tmp, c("Province", vi[var()][, varLabel]))
#            return(as.data.frame(tmp))
#          })      
#      
#      output$tableVar <- renderTable(include.rownames=F, {
#            dt.agg()  
#          })
#     
#      # Draw dot plot with ggvis
#      dt %>% ggvis(~X, ~Y, fill=~my_var) %>%
#          layer_points(size=5/60) %>%
#          hide_legend("fill") %>%
#          set_options(width=400, height=400) %>%
#          add_tooltip(function(df) paste(df$my_var, vi[var()][, unit])) %>%
#          bind_shiny("ggvis", "ggvis_ui")
      
      
      # Create the map
      map <- createLeafletMap(session, "map")
      
      # session$onFlushed is necessary to work around a bug in the Shiny/Leaflet
      # integration; without it, the addCircle commands arrive in the browser
      # before the map is created.
      paintObs <- observe({
            
            # Clear existing circles before drawing
            map$clearShapes()
            map$setView(mean(dt()$Y, na.rm=T), mean(dt()$X+2, na.rm=T), 6)
            
            # Draw in batches of 1000; makes the app feel a bit more responsive
            chunksize <- 1000
            for (from in seq.int(1, nrow(dt()), chunksize)) {
              to <- min(nrow(dt()), from + chunksize)
              chunk <- dt()[from:to,]
              # Bug in Shiny causes this to error out when user closes browser
              # before we get here
              try(map$addCircle(
                      chunk$Y, chunk$X, 7000, chunk$my_var,
                      options=list(stroke=F, fillOpacity=0.65),
                      eachOptions=list(color="red")
                  )
              )
            }
          })
      
      shiny:::flushReact()
      
    })




