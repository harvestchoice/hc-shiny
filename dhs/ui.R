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
    
    column(5, includeHTML("../dhs/www/txtIntro.html")),
    column(7, p(br())),
    column(3, offset=1,
      uiOutput("selectCat"),
      uiOutput("selectVar")
    ),
    column(2,
      uiOutput("selectISO"),
      actionButton("btn", "Show Indicator", icon("bar-chart"), class="btn-primary"),
      p(br())
    ),

    
    fluidRow(style="position: relative;",
      leafletMap("map", width="100%", height=380,
        initialTileLayer="//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        initialTileLayerAttribution=HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
        options=list(center=c(1, 41), zoom=6)),
      
      # Anchor side panel to map
      absolutePanel(bottom=10, right=20, width=380, height="auto",
        class="panel panel-primary",
        div(class="panel-heading", p(class="panel-title", tags$small("Map Options"))),
        div(class="panel-body",
          column(7,
            radioButtons("selectYear", "Survey Year", c("2003", "2008"), "2008", inline=T),
            radioButtons("selectRes", "Residence", c("rural", "urban"), "rural", inline=T),
            radioButtons("selectGender", "Gender", c(`n/a`=""), "", inline=T)
          ),
          column(5,
            uiOutput("col"),
            sliderInput("brks", "Legend breaks", 2, 8, 4, ticks=F, sep=""),
            tags$small(actionLink("btnShowBrewer", "Show color palettes"),
              `data-toggle`="modal", `data-target`="#brew")
          )
        ),
        div(class="panel-footer", actionLink("btnUpdate", "Update Map", icon("globe")))
      )
    ),
    
    column(10,
      conditionalPanel(condition="input.btn>0",
        bsAlert("alertNoData"),
        uiOutput("details"),
        tableOutput("svydt"),
        column(6, plotOutput("plot1")),
        column(6, plotOutput("plot2")))
    ),
    
    column(2,
      p(br()),
      selectInput("fileType", "Choose Export Format",
        choices=c(`ESRI Shapefile`="shp", CSV="csv", STATA="dta"),
        selected="csv"),
      downloadButton("saveData", "Save Layer"),
      hr(),
      includeHTML("../dhs/www/txtCredits.html")
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
