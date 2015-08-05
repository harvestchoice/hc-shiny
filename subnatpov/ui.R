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
    p(),

    column(3,
      selectInput("var", "Select a poverty indicator", vars),
      actionButton("btn", "Update", icon=icon("cog"), class="btn-primary")
    ),

    column(3,
      selectInput("selectISO3", "Select a country", selected="GHA", iso)
    ),

    column(3,
      selectInput("selectYear", "Select a survey year", def, selected="2012")
    ),

    column(3,
      selectInput("fileType", "Choose Export Format", choices=c(
        `ESRI Shapefile`="shp", CSV="csv", STATA="dta"), selected="csv"),
      downloadButton("saveData", "Save Layer")
    ),

    column(6,
      helpText("Click the map to view details for every administrative unit."),
      h2("Ranking"),
      p(),
      p(),
      p(tags$label("Credits"), br(), "Azzarri, Carlo; Bacou, Melanie; Signorelli, Sara.",
        br(), "&copy;IFPRI/HarvestChoice, 2015. Source code on",
        a(href="https://github.com/harvestchoice/hc-shiny/tree/master/subnatpov", target="github",
          "GitHub"), ".")
    ),

    column(6,
      h2("Details"),
      rHandsontableOutput("dtDetails", width="100%", height="320px"),
      p()
      )
  )
))
