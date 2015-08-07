shinyUI(fluidPage(
  title="HarvestChoice | sub-National Poverty Maps",
  theme="bootstrap.css",

  fluidRow(class="hc-header",
    column(9,
      h3("Poverty Prevalence",
        tags$small("sub-national time-series for sub-Saharan Africa"))),
    column(2, offset=1,
      h5(a(href="http://harvestchoice.org/", title="Home",
        img(src="global_logo.png", alt="Home"))))
  ),

  fluidRow(style="position: relative;",
    leafletOutput("map", height=380),
    p()
  ),

  column(3,
    selectInput("var", "Select a poverty indicator", vars),
    selectInput("selectISO3", "Select a country", iso, selected="KEN"),
    selectInput("selectYear", "Select a survey year", def, selected="2005"),
    actionButton("btn", "Map Indicator", icon=icon("cog"), class="btn-primary"),
    helpText(p("Make a selection and click to refresh the map.
          Click regions on the map to view details."), p(tags$u("Note:"),
      "mapping the whole of SSA is very slow and might hang your browser."))
  ),

  column(6,
    conditionalPanel(condition="input.btn==0",
      includeMarkdown("../subnatpov/www/txtIntro.md")),

    conditionalPanel(condition="input.btn>0",
      h2("Details"),
      uiOutput("hText"),
      rHandsontableOutput("dtDetails", width="100%", height="200px"))
  ),

  column(3,
    p(),
    radioButtons("opts", "Map indicator for the following sub-population", selected="total",
      choice=c("total", "rural", "urban", "male", "female")),
    hr(),
    selectInput("fileType", "Choose Export Format", choices=c(
      `ESRI Shapefile`="shp", CSV="csv", STATA="dta", `PDF Document`="pdf"), selected="csv"),
    downloadButton("saveData", "Save Layer"),
    helpText("Use the download options below to export the map or table.")
  ),

  column(5,
    h2("Ranking"),
    p("[graphs, TBD]")
  ),

  column(12,
    includeMarkdown("../subnatpov/www/txtCredits.md")
  )
)
)
