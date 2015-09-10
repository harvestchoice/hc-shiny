#####################################################################################
# Title:   HarvestChoice Data API with leaflet
# Date:    July 2015
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################


header <- dashboardHeader(title="HarvestChoice", dropdownMenuOutput("layerMenu"))

sidebar <- dashboardSidebar(
  selectInput("selectISO3", "Choose a Country", iso, selected="GHA"),
  hr(),

  sidebarMenu(id="selectCat",
    menuItem(names(catlst)[1], icon=icon("leaf"), catlst[[1]]),
    menuItem(names(catlst)[2], icon=icon("tree"), catlst[[2]]),
    menuItem(names(catlst)[3], icon=icon("male"), catlst[[3]]),
    menuItem(names(catlst)[4], icon=icon("road"), catlst[[4]])
  ),

  hr(),

  sidebarMenu(id="selectTool",
    menuItem("Analysis", icon=icon("cog", lib="glyphicon"),
      menuSubItem("Explore", tabName="Overview", icon=icon("angle-right")),
      menuSubItem("Summarize", tabName="Summarize", icon=icon("angle-right")),
      menuSubItem("Homologues", tabName="Homologues", icon=icon("angle-right"))
    ),
    menuItem("About", tabName="About", icon=icon("info-sign", lib="glyphicon"), selected=T)
  )

)

body <- dashboardBody(
  useShinyjs(),

  fluidRow(style="position:relative;margin-top:-15px;",

    # Map
    leafletOutput("map", height=450),

    # Details
    hidden(
      absolutePanel(id="panelDetails", class="panel panel-default",
        bottom=20, right=20, width=220, height="auto",
        htmlOutput("details", class="panel-body")
      )
    ),

    # Indicator menu (hidden initially)
    hidden(
      absolutePanel(id="panelInd", class="panel panel-default",
        top=-4, left=-4, width=280,
        div(class="panel-body", style="height:390px;overflow-y:auto;",
          uiOutput("chkGroupVar")),
        div(class="panel-footer",
          actionButton("btnLayer", "Select Layers", icon("globe"))
        )
      )
    )
  ),


  # Tools tabs
  tabItems(

    tabItem("About",
      includeMarkdown("../cell5m/www/txtIntro.md")),

    tabItem("Overview",
      br(),
      column(3,
        wellPanel(
          p("Click map to show pixel data."),
          hr(),
          selectInput("fileType", "Choose Export Format",
            choices=c(`Comma-separated`="csv", `ASCII Raster`="asc", GeoTIFF="tif", STATA="dta", RData="rds"),
            selected="asc"),
          downloadButton("saveData", "Save Layer")
        )
      ),

      column(9,
        uiOutput("cell5mUI")
      )
    ),

    tabItem("Summarize",

      box(width=5,
        br(),
        uiOutput("selectDomain"),
        p("Showing a random list of 10 domains to summarize by."),
        actionButton("btnDomain", "Summarize", icon("cog")),
        p(br(), "Click to summary and map.")
      ),

      box(width=7,
        h4("Domain Summary"),
        tableOutput("tableDomain")
      )
    ),

    tabItem("Homologues",

      box(width=5,
        p(br(), "Use the Layer choices on the right to select up to 5 layers."),
        p(htmlOutput("selectRank")),
        p(actionLink("btnAddRank", "Add Layer", icon("plus"))),
        p("Then select a district on the map (mouse over and click."),
        uiOutput("showRank"),
        actionButton("btnRank", "Rank all districts", icon("cog")),
        hr(),
        actionLink("btnClearRank", "Clear All", icon("refresh"))
      ),

      box(width=7,
        h4("District Comparison"),
        tableOutput("tableRank")
      )
    )
  ),

  fluidRow(
    column(12,
      hr(),
      includeMarkdown("../cell5m/www/txtCredits.md")
    )
  )
)


dashboardPage(header, sidebar, body)
