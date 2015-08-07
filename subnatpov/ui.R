shinyUI(fluidPage(
  title="HarvestChoice | sub-National Poverty Maps",
  theme="../assets/bootstrap.css",

  fluidRow(class="hc",
    column(9,
      h3("Poverty Prevalence",
        tags$small("sub-national time-series for sub-Saharan Africa"))),
    column(2, offset=1,
      h5(a(href="http://harvestchoice.org/", title="Home",
        img(src="../assets/global_logo.png", alt="Home"))))
  ),

  fluidRow(style="position: relative;",
    leafletOutput("map", height=380),
    p()
  ),

  fluidRow(
    column(3,
      p(),
      selectInput("var", "Select a poverty indicator", vars),
      selectInput("selectISO3", "Select a country", iso, selected="KEN"),
      selectInput("selectYear", "Select a survey year", def, selected="2005"),
      actionButton("btn", "Map Indicator", icon=icon("cog"), class="btn-primary"),
      helpText(p("Make a selection and submit to refresh the map and graphs.
          Click regions on the map to view details."), p(tags$u("Note:"),
            "mapping the whole of SSA is very slow and might hang your browser."))
    ),

    column(7,
      conditionalPanel(condition="input.btn==0",
        includeMarkdown("../subnatpov/www/txtIntro.md")),

      conditionalPanel(condition="input.btn>0",
        h2("Details"),
        uiOutput("hText"),
        rHandsontableOutput("dtDetails", width="100%"))
    ),

    column(2,
      p(),
      radioButtons("opts", "Map indicator for the following sub-population", selected="total",
        choice=c("total", "rural", "urban", "male", "female")),
      hr(),
      selectInput("fileType", "Choose Export Format", choices=c(
        `ESRI Shapefile`="shp", CSV="csv", STATA="dta", `PDF Document (map only)`="pdf"), selected="csv"),
      downloadButton("saveData", "Save Layer"),
      helpText("Use the download options above to export the map or table.")
    ),

    column(5,
      h2("Ranking"),
      p("[graphs, TBD]")
    ),

    column(12,
      includeMarkdown("../subnatpov/www/txtCredits.md")
    )
  ),


  fluidRow(class="hc",
    column(3,
      p("HarvestChoice generates knowledge products to help guide strategic investments
        to improve the well-being of poor people in sub-Saharan Africa through more
        productive and profitable farming.")
    ),

    column(3,
      p("©IFPRI/HarvestChoice, 2015. Source code on",
        a(href="https://github.com/harvestchoice/hc-shiny/tree/master/subnatpov", "GitHub."),
        "Powered by", a(href="http://shiny.rstudio.com/", "RStudio Shiny."),
        "Code and datasets are licensed under a",
        a(href="http://creativecommons.org/licenses/by-nc-sa/4.0/",
          "Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License."))
    ),

    column(6, style="text-align:right;",
      p(
        a(href="http://ifpri.org/", img(src="../assets/R_ifpri.png"),
          title="International Food Policy Research Institute"),
        a(href="http://www.pim.cgiar.org/", img(src="../assets/R_pim.png"),
          title="CGIAR Research Program on Policies Institutions and Markets"),
        a(href="http://umn.edu/", img(src="../assets/R_umn.png"),
          title="University of Minnesota"))
    )
  )

)
)
