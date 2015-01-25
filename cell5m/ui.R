library(shiny)
library(shinyBS)
library(leaflet)

shinyUI(fluidPage(
  title="Map overlays with leaflet",
  theme="bootstrap.css",

  leafletMap("map", width="100%", height="100%",
    initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
    # Center on Ghana
    options=list(center=c(7.79167, -1.20833 ), zoom=6)
  ),

  absolutePanel(id="results", fixed=F, draggable=T,
    top=20, left="auto", right=20, bottom="auto",
    width=260, height="auto", cursor="move",

    div(class="modal-content",
      h3("Map Layers"),
      bsAlert("alertNoData"),
      uiOutput("selectISO3"),
      uiOutput("selectCat"),
      uiOutput("selectVar"),
      #h4("Province Summary"),
      #tableOutput("tableVar"),
      actionButton("btnLayer", "Show Layer", icon("globe")),
      hr(),
      uiOutput("selectFilter"),
      hr(),
      selectInput("fileType", "Choose Export Format",
        choices=c(`ASCII Raster`="asc", GeoTIFF="tif", STATA="dta", RData="rds"),
        selected="asc"),
      downloadButton("saveData", "Save Layer"),
      p(br())
    )
  ),

  absolutePanel(id="filter", fixed=F, draggable=T,
    top=50, left=40, right="auto", bottom="auto",
    width=560, height="auto", cursor="move",

    div(class="modal-content", style="height:580px",

      h3(htmlOutput("varTitle")),
      p("Click map to show pixel data."),
      tabsetPanel(position="left", selected="Overview",

        tabPanel(title="Overview",
          column(6,
            h4("Histogram"),
            plotOutput("plotHist", height="100%"),
            p(br(), "Showing frequencies for visible pixels.")),
          column(6,
            h4("Layer Statistics"),
            tableOutput("tableSum")
          )
        ),

        tabPanel(title="Summarize",
          column(5,
            p(),
            uiOutput("selectDomain"),
            p("Showing a random list of 10 domains to summarize by."),
            actionButton("btnDomain", "Summarize", icon("cog")),
            p(br(), "Click to summary and map.")
          ),
          column(7,
            h4("Domain Summary"),
            tableOutput("tableDomain")
          )
        ),

        tabPanel(title="Homologue Finder",
          column(5,
            p(br(), "Use the Layer choices on the right to select up to 5 layers."),
            p(htmlOutput("selectRank")),
            p(actionLink("btnAddRank", "Add Layer", icon("plus"))),
            p("Then select a district on the map (mouse over and click."),
            uiOutput("showRank"),
            actionButton("btnRank", "Rank all districts", icon("cog")),
            hr(),
            actionLink("btnClearRank", "Clear All", icon("refresh"))
          ),
          column(7,
            h4("District Comparison"),
            tableOutput("tableRank")
          )
        )
      )
    )
  ),

  absolutePanel(id="cite", fixed=F, draggable=F,
    top="auto", left=10, right="auto", bottom=0,
    width="auto", height="auto",
    p("IFPRI/HarvestChoice, 2014. Source code on ",
      a("Github", href="https://github.com/harvestchoice/hc-shiny"), ".")

  )
)
)


