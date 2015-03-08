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

  #   fluidRow(style="position: relative;",
  #     leafletOutput("map", width="100%", height=380)
  #   ),

  fluidRow(

    column(4,
      includeHTML("../dhs/www/txtIntro.html")),

    column(3, offset=1,
      p(br()),
      selectInput("selectISO", "Choose a Country", iso, selected="GH"),
      selectInput("selectCat", "Choose a Theme", dhs.lbl[, unique(varCat)],
        selected="wealth index", size=7, selectize=F)),

    column(4,
      p(br()),
      uiOutput("selectVar")),

    column(7, offset=5,
      actionButton("btn", "Show Indicator", icon("bar-chart"), class="btn-primary"))
  ),

  fluidRow(
    column(12,
      tabsetPanel(position="above",
        tabPanel("Maps",

          sidebarLayout(position="right",
            sidebarPanel(width=3, style="margin-top:20px;",
              radioButtons("selectYear", "Survey Year", c("2003", "2008"), "2008", inline=T),
              radioButtons("selectRes", "Residence", c("rural", "urban"), "rural", inline=T),
              radioButtons("selectGender", "Gender", c(`n/a`=""), "", inline=T),
              selectInput("col", "Color palette", row.names(brewer.pal.info), selected="RdBu"),
              tags$small(actionLink("btnShowBrewer", "Show color palettes"),
                `data-toggle`="modal", `data-target`="#brew"),
              checkboxInput("revcol", "Reverse colors", value=F),
              sliderInput("brks", "Legend breaks", 2, 8, 5, ticks=F, sep=""),
              actionButton("btnUpdate", "Update Map", icon("globe"))
            ),

            mainPanel(width=9,
              h3(uiOutput("txtTitle")),
              bsAlert("alertNoData"),
              plotOutput("mapplot", height="auto", clickId="mplot"))
          )
        ),

        tabPanel("Data",

          sidebarLayout(position="left",
            sidebarPanel(width=3, style="margin-top:20px;",
              p(br()),
              selectInput("fileType", "Choose Export Format",
                choices=c(`ESRI Shapefile`="shp", CSV="csv", STATA="dta"),
                selected="csv"),
              downloadButton("saveData", "Save Layer")
            ),

            mainPanel(width=9,
              h3(uiOutput("txtSubTitle")),
              p(br()),
              tableOutput("svydt"))
          )
        )
      )
    ),

    column(12,
      hr(),
      includeHTML("../dhs/www/txtCredits.html"))
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
