library(shiny)
library(leaflet)
library(dygraphs)

# Month array
mth <- 0:12
names(mth) <- c("All", month.name)

introMsg <- div(
  h3("About CRU-TS 3.22"),
  p("Showing long-term monthly precipitation and drought index time series for sub-Saharan Africa across districts (level-2 administrative boundaries based on ", a("FAO GAUL 2008", href="http://www.fao.org/geonetwork/srv/en/metadata.show?id=12691"), "). The 1901-2013 monthly time-series are available through the University of East Anglia ", a("Climate Research Unit", href="http://www.cru.uea.ac.uk/cru/data/hrg/cru_ts_3.22"), " (version CRU-TS 3.22)."),
  h3("Palmer Drougth Severity Index"),
  p("The Palmer Drought Severity Index (PDSI), devised in 1965, is the first drought indicator to assess moisture status comprehensively. It uses temperature and precipitation data to calculate water supply and demand, incorporates soil moisture, and is considered most effective for unirrigated cropland. It primarily reflects long-term drought and has been used extensively to initiate drought relief. It is more complex than the SPI and the Drought Monitor."),
  p("The PDSI series used here (1850-2012 self-calibrated PDSI with Penman-Monteith PE) is available through the ", a("Climate Analysis Section", href="http://www.cgd.ucar.edu/cas/catalog/climind/pdsi.html"), " of the National Center for Atmospheric Research."),
  h3("Getting Started"),
  p("To start, select a series and a country from the left and click ", strong("Show Series"), ". Wait a few seconds for the interactive graph and map to render. You can mouse over and click districts on the map to view additional details."),
  p("District means, seasonality-adjusted means (trend), and standard deviations may be downloaded in CSV or STATA formats. The 1960-2013 long-term averages for each country may also be downloaded in TIF, IMG, GRD, and NC raster formats to be used with a desktop GIS."))



shinyUI(fluidPage(
  title="CRU-TS 3.22 with leaflet",
  theme="bootstrap.css",

  leafletMap("map", width="100%", height=460,
    initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
    # Center on Ghana
    options=list(center=c(7.79167, -1.20833 ), zoom=6)
  ),

  fluidRow(style="padding-top: 460px;",

    column(3,
      h3("Monthly Time Series"),
      uiOutput("selectCRU"),
      uiOutput("selectg0"),
      actionButton("btn", "Show Series", icon("globe")),
      conditionalPanel(condition="input.btn>0",
        hr(),
        p("The long-term mean is over the selected months only (or the entire year if no month is selected. The trend component is generated through classical seasonal decomposition by moving averages over the entire selected period."))
    ),

    column(7,
      conditionalPanel(condition="input.btn==0", introMsg),
      uiOutput("chartMsg"), br(),
      dygraphOutput("dygraph", width="100%", height="320px"),
      p(br())
    ),

    column(2,
      p(br()),
      uiOutput("selectg2"),
      sliderInput("rg", "Limit to Date Range", 1960, 2013, value=c(1960, 2013), step=1, format="###0"),
      selectInput("selectMonth", "Limit to Month", mth, selected=0),
      hr(),
      selectInput("fileType", "Choose Export Format", choices=c(
        GeoTiff="tif", `ASCII Raster`="asc", netCDF="nc", `Erdas Images`="img",
        CSV="csv", STATA="dta"), selected="csv"),
      downloadButton("saveData", "Save Layer")
    )
  ),

  conditionalPanel(condition="input.btn>0",
    absolutePanel(class="panel panel-default",
      top=20, left="auto", right=20, bottom="auto", width="auto", height="auto",
      div(class="panel-body",
        uiOutput("details"),
        #plotOutput("rasterPlot", width="auto", height="180px"),
        plotOutput("barPlot", width="auto", height="240px")
      )
    )
  ),

  absolutePanel(top=440, left=10, style="font-size: .9em;",
    p("IFPRI/HarvestChoice, 2014. Source code on ",
      a("Github", href="https://github.com/harvestchoice/hc-shiny"),
      ". Monthly observations from ",
      a("University of East Anglia CRU", href="http://www.cru.uea.ac.uk/cru/data/hrg/cru_ts_3.22"),
      " and ",
      a("NCAR CDG", href="http://www.cgd.ucar.edu/cas/catalog/climind/pdsi.html"), ".")
  )
)
)


