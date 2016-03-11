#####################################################################################
# Title:   CGIAR Site Integration
# Date:    March 2016
# Project: HarvestChoice/IFPRI for CGIAR CO
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################



shinyUI(fluidPage(
  title="CGIAR | Site Integration",
  theme="../assets/bootstrap_cgiar.css",
  tags$head(includeScript("../assets/ga.js")),

  fluidRow(class="hc",
    column(9,
      h3("CGIAR Site Integration",
        tags$small(" Proposed plan, March 2016"))),
    column(2, offset=1,
      h5(a(href="http://cgiar.org/", title="Home",
        img(src="../assets/global_cgiar_logo.png", alt="Home"))))
  ),

  fluidRow(style="position: relative;",

    leafletOutput("map", height=640),

    absolutePanel(class="panel panel-default",
      bottom=20, right=20, width=220, height="auto",
      div(class="panel-body",
        p("stuff")
      )
    )
  ),

  fluidRow(

    column(12,
      br(),
      includeMarkdown("./www/txtIntro.md")
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
