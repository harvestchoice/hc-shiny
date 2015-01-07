#####################################################################################
# Title: Testing HTML Canvas with ggvis
# Date: December 2014
# Project: HarvestChoice for the Bill and Melinda Gates Foundation
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

library(data.table)
library(shiny)
library(hcapi3)
library(ggvis)

setwd("/home/projects/shiny/cell5mGgvis")


# Define server logic for slider examples
shinyServer(function(input, output, session) {

      # Create controls
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

      # query hcapi3
      dt <- reactive({
            tmp <- hcapi3::getLayer(var(), iso3())
            tmp <- tmp[ADM1_NAME_ALT!="buffer gridcell"]
            setkey(tmp, X, Y)
            setnames(tmp, 8, "my_var")
            return(data.frame(tmp))
          })

      # Statistics
      output$tableSum <- renderTable(digits=0, include.rownames=F, align=c("l", "l", "r"), {
            tmp <- summary(dt()$my_var)
            tmp <- data.table(Statistic=names(tmp), Value=tmp)
            return(tmp)
          })

      # Redraw dot plot
#      dt %>% ggvis(~X, ~Y, fill=~my_var) %>%
#          layer_points(size=.8) %>%
#          set_options(width=220, height=180) %>%
#          bind_shiny("ggvis", "ggvis_ui")

      # Redraw histogram
      dt %>% ggvis(~my_var) %>%
          layer_histograms(fill.hover:="blue", width=0.1) %>%
          set_options(width=220, height=180) %>%
          bind_shiny("hist", "hist_ui")


    })
