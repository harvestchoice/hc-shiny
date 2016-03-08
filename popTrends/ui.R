#####################################################################################
# Title:   Population Hotspots 2000-2020 - Africa
# Date:    March 2016
# Project: HarvestChoice/IFPRI for PIM
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################



shinyUI(fluidPage(
  title="HarvestChoice | Population Hotspots in SSA",
  theme="../assets/bootstrap.css",
  tags$head(includeScript("../assets/ga.js")),

  fluidRow(class="hc",
    column(9,
      h3("Population Hotspots ",
        tags$small(" sub-national time-series for sub-Saharan Africa"))),
    column(2, offset=1,
      h5(a(href="http://harvestchoice.org/", title="Home",
        img(src="../assets/global_logo.png", alt="Home"))))
  ),

  fluidRow(style="position: relative;",

    leafletOutput("map", height=420),

    conditionalPanel(condition="input.btn>0",
      absolutePanel(class="panel panel-default",
        bottom=20, right=20, width=220, height="auto",
        div(class="panel-body", uiOutput("details"))
      )
    )
  ),

  fluidRow(

    column(3,
      br(),
      wellPanel(
        selectInput("selectISO3", "Select a Country", iso, selected="SSA"),
        helpText("Update all the graphs on this page for the selected country.")),
      includeMarkdown("./www/txtIntro.md")
    ),

    column(9,
      htmlOutput("title")
    ),

    column(5,
      h4("Population Trends", tags$small("2000-2020 (headcount)")),
      p("Urban and rural population growth and projected trends in coastal and inland areas.
        Coastal areas are within", em("150 km", "of the nearest coastline.")),
      ggvisOutput("p1")),

    column(4,
      h4("Urbanization Rate", tags$small("2000-2020 (percent)")),
      p("Trends in urbanization rate between coastal and inland areas."),
      br(),
      ggvisOutput("p2")),

    column(6,
      h4("Major Urban Hotspots by Country"),
      ggvisOutput("p3")
    ),

    column(3,
      br(),br(),
      wellPanel(
        selectInput("fileType", "Choose Export Format", choices=c(
          `ESRI Shapefile`="shp", CSV="csv", STATA="dta", `PNG Document (map only)`="png"), selected="csv"),
        downloadButton("saveData", "Save Layer"),
        helpText("Use the download options above to export the map data.")
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
        a(href="https://github.com/harvestchoice/hc-shiny/tree/master/popTrends", "GitHub."),
        "Powered by", a(href="http://shiny.rstudio.com/", "RStudio Shiny."),
        "Code and datasets are licensed under a",
        a(href="http://creativecommons.org/licenses/by-nc-sa/4.0/",
          "Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License."))
    ),

    column(2, offset=2, a(style="color:#3C3A2E;", href="http://ifpri.org/",
      img(src="../assets/R_ifpri.png"),
      title="International Food Policy Research Institute")),
    column(2, a(style="color:#3C3A2E;", href="http://www.pim.cgiar.org/",
      img(src="../assets/R_pim.png"),
      title="CGIAR Research Program on Policies Institutions and Markets"))
  )


))
