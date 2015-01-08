#####################################################################################
# Title: Visualize DHS Regional Estimates
# Date: Janurary 2015
# Project: HarvestChoice for A4NH
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

library(shiny)
library(shinyBS)
library(leaflet)


shinyUI(fluidPage(
  title="DHS - Subnational Nutrition and Health Statistics",
  theme="bootstrap.css",

  # Leaflet map
  absolutePanel(top=0, bottom="auto", left=0, right=0, width="auto", height="auto",
    style="z-index: -99",
    leafletMap("map", width="100%", height=460,
      initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
      options=list(center=c(7.79167, -1.20833 ), zoom=6, zoomControl="topright"))
  ),

  sidebarLayout(position="right",

    sidebarPanel(width=3, style="margin-top:20px;",
      h3("Nutrition and Health Statistics"),
      uiOutput("selectISO"),
      uiOutput("selectVar"),
      div(style="margin-left:20px;",
        radioButtons("selectRes", "Residence", c("rural", "urban"), selected="rural")
      ),
      actionButton("btn", "Show Map", icon("globe")),
      hr(),
      uiOutput("selectYear"),
      uiOutput("cl"),
      p(actionLink("btnShowBrewer", "Show color palettes"),
        `data-toggle`="modal", `data-target`="#brew"),
      sliderInput("cv", "Number of legend breaks", 2, 8, 4),
      hr(),
      selectInput("fileType", "Choose Export Format",
        choices=c(`ESRI Shapefile`="shp", CSV="csv", STATA="dta"),
        selected="csv"),
      downloadButton("saveData", "Save Layer"),
      hr(),
      includeHTML("../dhs/www/txtCredits.html"),
      p(br())
    ),

    mainPanel(width=9, style="margin-top: 460px;",
      conditionalPanel(condition="input.btn==0", includeHTML("../dhs/www/txtIntro.html")),
      conditionalPanel(condition="input.btn>0", bsAlert("alertNoData")),
      fluidRow(
        column(3, tableOutput("svydt")),
        column(3, plotOutput("plot1")),
        column(3, plotOutput("plot2"))
      )
    )
  ),

  # Modal color palette (a bit code heavy)
  div(class="modal fade", id="brew", tabindex="-1", role="dialog",
    `aria-labelledby`="Brewer Color Palettes", `aria-hidden`="true",
    div(class="modal-dialog", style="height:auto;",
      div(class="modal-content",
        div(class="modal-header",
          tags$button(class="close", `data-dismiss`="modal", `aria-label`="Close",
            span(`aria-hidden`="true", HTML("&times;"))),
          h4("Brewer Color Palettes")),
        div(class="modal-body", style="height:540px", plotOutput("plotBrewer"))
      )
    )
  )
)
)
