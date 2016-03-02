#####################################################################################
# Title:   HarvestChoice Tablr v2.0
# Date:    February 2016
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

header <- dashboardHeader(title="HarvestChoice Tablr")

sidebar <- dashboardSidebar(
  selectInput("selectISO3", "Choose a Region", iso, selected="GHA"),

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

  column(width=9,

    # Indicator menu (hidden initially)
    hidden(
      absolutePanel(id="panelInd",
        class="panel panel-default",
        top=-20, left=-20, width=280, height=520,
        div(class="panel-body", style="height:490px;overflow-y:auto;",
          uiOutput("chkGroupVar")),
        div(class="panel-footer",
          actionButton("btnLayer", "Select Layers", icon("globe")))
      )
    ),

    # Map
    rpivotTableOutput("pvt", width="70%"),

    # Details
    hidden(
      absolutePanel(id="panelDetails", class="panel panel-default",
        bottom=20, right=20, width=220, height="auto",
        htmlOutput("details", class="panel-body")
      )
    )
  ),

  box(title="Selected Indicators", width=3, solidHeader=T,
    uiOutput("selectedVarList"),
    footer=actionButton("btnClear", "Clear selection", icon("times"))
  )
)

dashboardPage(header, sidebar, body)
