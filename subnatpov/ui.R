shinyUI(fluidPage(
  title="HarvestChoice | sub-National Poverty Maps",
  theme="../assets/bootstrap.css",

  fluidRow(class="hc",
    column(9,
      h3("Expenditure, Poverty, and Inequality Trends",
        tags$small("sub-national time-series for sub-Saharan Africa"))),
    column(2, offset=1,
      h5(a(href="http://harvestchoice.org/", title="Home",
        img(src="../assets/global_logo.png", alt="Home"))))
  ),

  fluidRow(style="position: relative;",
    leafletOutput("map", height=400),
    br()
  ),

  fluidRow(
    column(3,
      wellPanel(
        br(),
        selectInput("var", "Select a poverty indicator", vars),
        selectInput("selectISO3", "Select a country", iso, selected="KEN"),
        selectInput("selectYear", "Select a survey year", def, selected="2005"),
        actionButton("btn", "Map Indicator", icon=icon("cog"), class="btn-primary"),
        helpText(br(), p("Select an indicator and region of interest and submit to update
          the map and graphs. Click over shaded areas on the map to view details."))
      ),
      br(), br(), br(), br(), br(), br(),
      withMathJax(includeMarkdown("../subnatpov/www/txtCredits.md"))
    ),

    column(9,

      tabsetPanel(id="ts", selected="About",
        tabPanel("About", icon=icon("question-circle"),
          column(12, includeMarkdown("../subnatpov/www/txtIntro.md"), hr()),
          column(9, ggvisOutput("p1")),
          column(3,
            h3("Country Trajectories"),
            p("Showing trajectories for a sample of 10 countries in
            sub-Saharan Africa. Movements towards the top right quadrant mark progress towards
            poverty reduction and greater income equality."),
            actionLink("p1Update", "Click to circle countries", icon=icon("refresh")),
            helpText("Mouse over any segment to identify a country.")),
          column(12, hr(), includeMarkdown("../subnatpov/www/txtIntro2.md"), br())
        ),

        tabPanel("Details", icon=icon("table"),
          column(9,
            uiOutput("hText"),
            rHandsontableOutput("dtDetails", width="100%")
          ),

          column(3,
            br(), br(),br(),
            radioButtons("opts", "Map indicator for the following sub-population", selected="total",
              choice=c(
                `entire population`="total",
                `rural households`="rural",
                `urban households`="urban",
                `female-headed households`="female",
                `male-headed households`="male")),
            br(), br(), hr(),
            selectInput("fileType", "Choose Export Format", choices=c(
              `ESRI Shapefile`="shp", CSV="csv", STATA="dta", `PDF Document (map only)`="pdf"), selected="csv"),
            downloadButton("saveData", "Save Layer"),
            helpText("Use the download options above to export the map or table.")
          ),

          column(12,
            uiOutput("svar"),
            ggvisOutput("p2"),
            br()
          )
        ),

        tabPanel("References", icon=icon("file-text-o"),
          column(3,
            h3("Data Inventory"),
            p("Showing the number of data points across countries and years at which
              nationally-representative household surveys are available. Two maps for
              the entire subcontinent are available for reference years 2005 and 2008.")),
          column(9, br(), ggvisOutput("p4")),
          column(12, includeMarkdown("../subnatpov/www/txtDoco.md"), br())
        )
      )

    )
  ),


  fluidRow(class="hc-footer",
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

    column(2, a(style="color:#3C3A2E;", href="http://ifpri.org/", img(src="../assets/R_ifpri.png"),
      title="International Food Policy Research Institute")),
    column(2, a(style="color:#3C3A2E;", href="http://www.pim.cgiar.org/", img(src="../assets/R_pim.png"),
      title="CGIAR Research Program on Policies Institutions and Markets")),
    column(2, a(style="color:#3C3A2E;", href="http://umn.edu/", img(src="../assets/R_umn.png"),
      title="University of Minnesota"))
  )

)
)
