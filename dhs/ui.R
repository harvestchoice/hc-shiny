#####################################################################################
# Title: Visualize DHS Regional Estimates
# Date: Janurary 2015
# Project: HarvestChoice for A4NH
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################
library(rCharts)

shinyUI(fluidPage(
  title="DHS - Subnational Nutrition and Health Statistics",
  theme="bootstrap.css",

  fluidRow(class="hc-header",
    column(9,
      h3("Health and Nutrition", tags$small(" sub-national indicators"))),
    column(2, offset=1,
      h5(a(href="http://harvestchoice.org/", title="Home",
        img(src="global_logo.png", alt="Home"))))
  ),

  fluidRow(style="position: relative;",
    leafletOutput("map", width="100%", height=380),

    conditionalPanel("input.btn>0",
      absolutePanel(right=10, bottom=20, width=380,
        div(class="panel panel-primary",
          div(class="panel-heading", uiOutput("txtHead")),
          div(class="panel-body",
            column(7,
              radioButtons("selectYear", "Survey Year", c("2003", "2008"), "2008", inline=F),
              radioButtons("selectRes", "Residence", c("rural", "urban"), "rural", inline=T),
              radioButtons("selectGender", "Gender", c(`n/a`=""), "", inline=T)
            ),
            column(5,
              selectInput("col", "Color palette", row.names(brewer.pal.info), selected="RdBu"),
              sliderInput("brks", "Legend breaks", 2, 8, 5, ticks=F, sep=""),
              tags$small(actionLink("btnShowBrewer", "Show color palettes"),
                `data-toggle`="modal", `data-target`="#brew")
            )
          ),
          div(class="panel-footer", actionLink("btnUpdate", "Update Map", icon("globe")))
        )
      )
    )
  ),

  fluidRow(
    p(br()),

    column(5,
      includeHTML("../dhs/www/txtIntro.html")),
    column(3, offset=1,
      selectInput("selectCat", "Choose a Theme", dhs.lbl[, unique(varCat)], selected="wealth index"),
      uiOutput("selectVar")),
    column(3,
      selectInput("selectISO", "Choose a Country", iso, selected="GH"),
      actionButton("btn", "Show Indicator", icon("bar-chart"), class="btn-primary")),

    conditionalPanel("input.btn>0",
      column(12, hr()),
      column(9,
        h3(uiOutput("txtTitle")),
        bsAlert("alertNoData"),
        tableOutput("svydt")),

      column(3,
        selectInput("fileType", "Choose Export Format",
          choices=c(`ESRI Shapefile`="shp", CSV="csv", STATA="dta"),
          selected="csv"),
        downloadButton("saveData", "Save Layer")),

      column(12, plotOutput("mapplot"))
    )
  ),

  fluidRow(
    column(12, hr()),
    column(5,
      includeHTML("../dhs/www/txtCredits.html")
    ),
    column(6, offset=1)
  ),

  # Modal color palette (a bit code heavy)
  div(class="modal fade", id="brew", tabindex="-1", role="dialog",
    `aria-labelledby`="Brewer Color Palettes", `aria-hidden`="true",
    div(class="modal-dialog", style="height:auto;",
      div(class="modal-content",
        div(class="modal-header",
          tags$button(class="close", `data-dismiss`="modal", `aria-label`="Close",
            span(`aria-hidden`="true", HTML("&times;"))),
          h4("Brewer Color Palettes")),
        div(class="modal-body", style="height:540px", plotOutput("plotBrewer"))
      )
    )
  )

)
)
