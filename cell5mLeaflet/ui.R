library(shiny)
library(shinyBS)
library(leaflet)

# Define UI for random distribution application 
shinyUI(fluidPage(
        title="Map overlays with leaflet",
        theme="bootstrap.css",      
        
        leafletMap("map", width="100%", height="100%",
            initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
            initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
            # Center on Ghana
            options=list(center=c(7.79167, -1.20833 ), zoom=6)
        ),          
        
        absolutePanel(id="results", fixed=F, draggable=T,
            top=20, left="auto", right=20, bottom="auto",
            width=260, height="auto", cursor="move",              
            
            div(class="modal-content",
                h3("Map Layers"),
                bsAlert("alertNoData"), 
                uiOutput("selectISO3"),                 
                uiOutput("selectCat"),
                uiOutput("selectVar"),           
                #h4("Province Summary"),
                #tableOutput("tableVar"),
                hr(),
                uiOutput("selectFilter"),
                hr(),
                selectInput("fileType", "Choose Export Format",
                    choices=c(`ASCII Raster`="asc", GeoTIFF="tif", STATA="dta", RData="rds"),
                    selected="ASCII Raster"),
                downloadButton("saveData", "Save Layer")
            )
        ),
        
        absolutePanel(id="filter", fixed=F, draggable=T,
            top=20, left=40, right="auto", bottom="auto",
            width=560, height="auto", cursor="move",              
            
            div(class="modal-content", style="height:600px",
                
                h3(htmlOutput("varTitle")),
                p("Click map to show pixel data."),
                tabsetPanel(position="left", selected="Overview",
                    tabPanel(title="Overview",
                        column(3, offset=0,
                            h4("Histogram"),
                            plotOutput("plotHist", height="100%")),
                        column(2, 
                            h4("Layer Statistics"),
                            tableOutput("tableSum")
                        )
                    ),
                    
                    tabPanel(title="Summarize",
                        column(2,
                            p(),
                            uiOutput("selectDomain"),
                            p("Showing a random list of 10 domains to summarize by."),
                            actionButton("btnDomain", "Summarize", icon("cog")),
                            hr(),
                            actionButton("btnMapDomain", "Map It", icon("globe")),
                            p("Click to display results on the map."),
                            p(htmlOutput("txtDomain"))
                        ),
                        column(3,
                            h4("Domain Summary"),
                            tableOutput("tableDomain")
                        )
                    )
                )
            )
        ),       
        
        absolutePanel(id="cite", fixed=F, draggable=F,
            top="auto", left=10, right="auto", bottom=0,
            width="auto", height="auto",  
            p("IFPRI/HarvestChoice, 2014. Source code on ",
                a("Github", href="https://github.com/harvestchoice/hc-shiny"), ".")
        
        )
    )
)


