#####################################################################################
# Title: CRU and PDSI 1960-2013 Time Series across Districts
# Date: December 2014
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

library(shiny)
library(leaflet, lib.loc="/home/mbacou/R/x86_64-redhat-linux-gnu-library/3.1")
library(dygraphs)
library(shinyBS)

# Month array
mth <- 0:12
names(mth) <- c("All", month.name)


shinyUI(fluidPage(
        title="CRU-TS 3.22 with leaflet",
        theme="bootstrap.css",

        absolutePanel(top=0, bottom="auto", left=0, right=0, width="auto", height="auto",
            leafletMap("map", width="100%", height=420,
                initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
                # Center on Kenya
                options=list(center=c(1, 41), zoom=6)
            )
        ),

        fluidRow(style="margin-top: 420px;",

            column(3,
                h3("Drought and Precipitation", 
                  tags$small("Monthly sub-National Time-Series for sub-Saharan Africa")),
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
                uiOutput("chartMsg"),
                p(br()),
                conditionalPanel(condition="input.btn>0",
                    dygraphOutput("dygraph", width="100%", height="320px"),
                    p(br()))
            ),

            column(2,
                p(br()),
                uiOutput("selectg2"),
                sliderInput("rg", "Limit to Date Range", 1960, 2013, value=c(1960, 2013),
                  step=1, sep="", ticks=F),
                selectInput("selectMonth", "Limit to Month", mth, selected=0, multiple=T),
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
