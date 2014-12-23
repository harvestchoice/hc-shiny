library(shiny)
library(leaflet)
library(dygraphs)

# Month array
mth <- 0:12
names(mth) <- c("All", month.name)


shinyUI(fluidPage(
  title="CRU-TS 3.22 with leaflet",
  theme="bootstrap.css",

  leafletMap("map", width="100%", height=340,
    initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
    # Center on Ghana
    options=list(center=c(7.79167, -1.20833 ), zoom=6)
  ),

  fluidRow(

    column(3,
      h3("CRU-TS 3.22"),
      uiOutput("selectCRU"),
      uiOutput("selectg0"),
      uiOutput("selectg2"),
      actionButton("btn", "Show Series", icon("globe")),
      p(br())
    ),

    column(7,
      p(br(), htmlOutput("waitMsg")),
      dygraphOutput("dygraph", height="320px"),
      p(br())
    ),

    column(2,
      p(br()),
      sliderInput("rg", "Limit to Date Range", 1901, 2013, value=c(1901, 2013), step=1, format="###0"),
      selectInput("selectMonth", "Limit to Month", mth, selected=0),
      hr(),
      selectInput("fileType", "Choose Export Format", choices=c(
        GeoTiff="tif", `ASCII Raster`="asc", netCDF="nc", `Erdas Images`="img",
        CSV="csv", STATA="dta"), selected="csv"),
      downloadButton("saveData", "Save Layer")
    )
  ),


  absolutePanel(top=320, left=10,
    p(class="cite", "IFPRI/HarvestChoice, 2014. Source code on ",
      a("Github", href="https://github.com/harvestchoice/hc-shiny"),
      ". Monthly observations from ",
      a("University of East Anglia CRU", href="http://www.cru.uea.ac.uk/cru/data/hrg/cru_ts_3.22"), ".")
    )
  )
)


