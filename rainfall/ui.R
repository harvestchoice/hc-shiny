#####################################################################################
# Title: CRU and PDSI 1960-2013 Time Series across Districts
# Date: December 2014
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

library(shiny)
# Testing 2 versions of leaflet for now, til they merge
library(leaflet, lib.loc="/home/mbacou/R/x86_64-redhat-linux-gnu-library/3.1")
library(dygraphs)
library(shinyBS)

# Month array
mth <- 0:12
names(mth) <- c("All", month.name)


shinyUI(fluidPage(
    title="CRU-TS 3.22 with leaflet",
    theme="bootstrap.css",
    
    # Whole width map
    absolutePanel(top=0, left=0, right=0, bottom=0, width="auto", height="auto",
      leafletMap("map", width="100%", height=380,
        initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
        # Center on Kenya
        options=list(center=c(1, 41), zoom=6)
      )
    ),
    
    fluidRow(style="margin-top: 380px;",
      
      column(3,
        h3("Long-Term Drought and Precipitation", 
          tags$small(br(), "Monthly sub-national time-series for sub-Saharan Africa")),
        hr(),
        uiOutput("selectVar"),
        uiOutput("selectg0"),
        actionButton("btn", "Show Series", icon("globe")),
        hr(),
        includeHTML("../rainfall/www/txtCredits.html"),
        p(br())
      ),
      
      column(7,
        conditionalPanel(condition="input.btn==0",
          includeHTML("../rainfall/www/txtIntro.html")),
        uiOutput("selectedMsg"),
        conditionalPanel(condition="input.btn>0",
          dygraphOutput("dygraph", width="100%", height="320px"),
          br(),
          dygraphOutput("dygraphAnnual", width="100%", height="220px"),
          p())
      ),
      
      column(2,
        p(br()),
        uiOutput("selectg2"),
        sliderInput("rg", "Limit to Date Range", 1960, 2013, value=c(1960, 2013),
          step=1, sep="", ticks=F),
        selectInput("selectMonth", "Limit to Month(s)", mth, selected=0, multiple=T),
        p("The long-term mean is over the selected months and period only (or over the
            entire year if no month is selected). The trend component is generated through
            classical seasonal decomposition by moving averages over the entire 1960-2013 period."),        
        hr(),
        selectInput("fileType", "Choose Export Format", choices=c(
            `ESRI Shapefile`="shp", GeoTiff="tif", netCDF="nc", CSV="csv", STATA="dta"),
          selected="csv"),
        downloadButton("saveData", "Save Layer")
      )
    ),
    
    conditionalPanel(condition="input.btn>0",
      absolutePanel(class="panel panel-default",
        top=20, right=20, width=220, height="auto",
        div(class="panel-body",
          uiOutput("details"),
          bsAlert("alertNoData")
        )
      )
    )
  )
)
