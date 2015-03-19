#####################################################################################
# Title: CRU and PDSI 1960-2013 Time Series across Districts
# Date: December 2014
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################


shinyUI(fluidPage(
  title="HarvestChoice | Monthly Time-Series fo SSA",
  theme="bootstrap.css",

  fluidRow(class="hc-header",
    column(9,
      h3("Long-Term Climate Trends and Variations",
        tags$small("Monthly sub-national time-series for sub-Saharan Africa"))),
    column(2, offset=1,
      h5(a(href="http://harvestchoice.org/", title="Home",
        img(src="global_logo.png", alt="Home"))))
  ),

  fluidRow(style="position: relative;",
    leafletMap("map", width="100%", height=380,
      initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
      # Center on Kenya
      options=list(center=c(1, 41), zoom=6)
    ),

    conditionalPanel(condition="input.btn>0",
      absolutePanel(class="panel panel-default",
        bottom=20, right=20, width=220, height="auto",
        div(class="panel-body",
          uiOutput("details")
        )
      )
    )
  ),

  fluidRow(
    column(3,
      p(br()),
      selectInput("var", "Choose a Variable", d[c(5, 7, 11)], selected="pre"),
      selectInput("selectg0", "Choose a Country", names(g2.list), selected="Kenya"),
      actionButton("btn", "Show Series", icon("globe"), class="btn-primary"),
      hr(),
      p("The ", strong("long-term mean"), " is over the selected season
          and years only (or over the entire year if no specific season is selected).
          The ", strong("trend component"), " is generated through classical seasonal decomposition
          by moving averages over the entire 1960-2013 period. ", strong("Total precipitation"), " at
          the bottom is over the selected season. ", strong("Temperature"),
        " is near-surface temperature in Celsius."),
      p("You may use your mouse to zoom into
          any specific time period or use the range selectors below the chart.
          Double-click to reset the chart to its full length."),
      hr(),
      includeHTML("../rainfall/www/txtCredits.html")
    ),

    column(7,
      conditionalPanel(condition="input.btn==0",
        includeHTML("../rainfall/www/txtIntro.html")),
      uiOutput("selectedMsg"),
      bsAlert("alertNoData"),
      p(br()),
      dygraphOutput("dygraph", width="100%", height="320px"),
      p(br()),
      dygraphOutput("dygraphAnnual", width="100%", height="240px"),
      #hr(),
      #h3("Top/Bottom Five Districts"),
      #plotOutput("distBarPlot", width="100%", height="320px"),
      p(br())
    ),

    column(2,
      p(br()),
      selectInput("selectg2", "Limit to District",
        choices=c(`Entire Country`="Entire Country", g2.list[["Kenya"]]), selected="Entire Country", selectize=T),
      sliderInput("selectMonth", "Limit to Season (Jan-Dec)", 1, 12, value=c(1,12), step=1),
      sliderInput("rg", "Limit to Date Range", 1960, 2013, value=c(1960, 2013), step=1, sep="", ticks=F),
      hr(),

      selectInput("fileType", "Choose Export Format", choices=c(
        `ESRI Shapefile`="shp", GeoTIFF="tif", netCDF="nc", CSV="csv", STATA="dta"),
        selected="csv"),
      downloadButton("saveData", "Save Layer"),
      p(br(), tags$label("Export formats"), br(), "NetCDF and GeoTIFF produce monthly
        multi-band rasters over the 1960-2013 period. ESRI Shapefile returns district
        means over the entire period. CSV and STATA formats return a table of monthly
        statistics for the selected country or district.")
    )
  )
)
)
