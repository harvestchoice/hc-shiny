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

  tabsetPanel(type="tabs", selected="Indicators",

    tabPanel("About",
      column(12,
        includeMarkdown("../subnatpov/www/txtIntro.md"))
    ),

    tabPanel("Indicators",
      p(),
      column(3,
        selectInput("var", "Select a poverty indicator", vars),
        selectInput("selectISO3", "Select a country", selected="KEN", iso),
        selectInput("selectYear", "Select a survey year", def, selected="2005"),
        actionButton("btn", "Map Indicator", icon=icon("cog"), class="btn-primary")
      ),

      column(3,
        p(), p(),
        helpText("Make a selection and click to refresh the map. Click regions on the map to view details."),
        p(),
        helpText("Both map and table may be saved into multiple formats."),
        selectInput("fileType", "Choose Export Format", choices=c(
          `ESRI Shapefile`="shp", CSV="csv", STATA="dta", `Print format`="png"), selected="csv"),
        downloadButton("saveData", "Save Layer")
      ),

      column(6,
        h2("Details"),
        rHandsontableOutput("dtDetails", width="100%", height="300px")
      ),

      column(6,
        h2("Ranking"),
        p()
      ),

      column(6,
        p()
      )
    )
  ),

  column(12,
    p(tags$label("Credits"), br(), "Azzarri, Carlo; Bacou, Melanie; Signorelli, Sara.",
      br(), "©IFPRI/HarvestChoice, 2015. Source code on",
      a(href="https://github.com/harvestchoice/hc-shiny/tree/master/subnatpov", target="github",
        "GitHub."))
  )
)
)
