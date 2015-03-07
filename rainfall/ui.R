#####################################################################################
# Title: CRU and PDSI 1960-2013 Time Series across Districts
# Date: December 2014
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################


shinyUI(fluidPage(
  title="CRU-TS 3.22 with leaflet",
  theme="bootstrap.css",

  fluidRow(class="hc-header",
    column(9,
      h3("Long-Term Drought and Precipitation",
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
      selectInput("var", "Choose a Variable", d[c(5, 11)], selected="pre"),
      selectInput("selectg0", "Choose a Country", names(g2.list), selected="Kenya"),
      actionButton("btn", "Show Series", icon("globe"), class="btn-primary"),
      conditionalPanel(condition="input.btn>0",
        hr(),
        p(tags$label("Notes"), br(), "The long-term mean is over the selected months and period only (or over the
              entire year if no month is selected). The trend component is generated through
              classical seasonal decomposition by moving averages over the entire 1960-2013 period.")),
      hr(),
      includeHTML("../rainfall/www/txtCredits.html"),
      p(br())
    ),


    conditionalPanel(condition="input.btn==0",
      column(9,
        includeHTML("../rainfall/www/txtIntro.html"))
    ),

    conditionalPanel(condition="input.btn>0",
      column(7,
        uiOutput("selectedMsg"),
        bsAlert("alertNoData"),
        p(br()),
        dygraphOutput("dygraph", width="100%", height="320px"),
        p(br()),
        dygraphOutput("dygraphAnnual", width="100%", height="220px"),
        p(br())
      ),

      column(2,
        p(br()),
        selectInput("selectg2", "Limit to District",
          choices=c(`Entire Country`="Entire Country", g2.list[["Kenya"]]), selected="Entire Country", selectize=T),
        sliderInput("rg", "Limit to Date Range", 1960, 2013, value=c(1960, 2013), step=1, sep="", ticks=F),
        sliderInput("selectMonth", "Limit to Season (Jan-Dec)", 1, 12, value=c(1,12), step=1),
        hr(),

        selectInput("fileType", "Choose Export Format", choices=c(
          `ESRI Shapefile`="shp", GeoTiff="tif", netCDF="nc", CSV="csv", STATA="dta"),
          selected="csv"),
        downloadButton("saveData", "Save Layer")
      )
    )
  )
)
)
