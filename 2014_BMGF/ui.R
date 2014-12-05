library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(title="Ethiopia Segmentation",
        
        tags$head(tags$link(rel='stylesheet', type='text/css', href='style.css')),       
        
        # Application title
        titlePanel("Ethiopia Segmentation, 2014"),
        
        # Sidebar with controls to select the random distribution type
        # and number of observations to generate. Note the use of the
        # br() element to introduce extra vertical spacing
        sidebarLayout(
            
            sidebarPanel(width=3,
                
                h4("Spatial Classification"),                
                uiOutput("sldelev"),

                uiOutput("sldrain"),
                p(tags$small("Select break values for low and high lands.")),
                uiOutput("sldtt20k"),
                p(tags$small("Select break values for low, medium, and high market access.")),
                uiOutput("sldpopden"),
                p(tags$small("Select break values for low, medium, and high population density zones.")),
                br(),
                submitButton("Update View")
            ),
            
            mainPanel(position="left", width=9,
                uiOutput("map")
            )
        ),
        
        fluidRow(
            column(4,
                h4("Summary"),
                tableOutput("sum1"), br(),
                tableOutput("sum2"), br(),
                tableOutput("sum3"), br(),
                tableOutput("sum4"),
                p(em("HarvestChoice/IFPRI, 2014.", class="caption"))                
            ),
            
            column(8,
                h4("Household Statistics"),
                tabsetPanel(type="tabs", position="above",
                    tabPanel("Segments",
                        dataTableOutput("table")
                    ),
                    
                    tabPanel("Population",
                        tableOutput("stats")
                    )                 
                )
            )
        )
    )
)