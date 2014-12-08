library(shiny)
library(shinyBS)
library(leaflet)

# Define UI for random distribution application 
shinyUI(fluidPage(
        title="Map overalys with leaflet",
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
                h3("Map Overlays"),
                bsAlert("alertNoData"),                
                uiOutput("selectCat"),
                uiOutput("selectVar"),
                uiOutput("selectISO3"),            
                #h4("Province Summary"),
                #tableOutput("tableVar"),
                hr(),
                uiOutput("selectFilter"),
                hr(),
                selectInput("fileType", "Choose Export Format",
                    choices=c(`ASCII Raster`="asc", GeoTIFF="tif", STATA="dta", RData="rdata"),
                    selected="ASCII Raster"),
                downloadButton("saveData", "Save Layer")
            )
        ),
        
        absolutePanel(id="filter", fixed=F, draggable=T,
            top="auto", left=40, right="auto", bottom=60,
            width=280, height="auto", cursor="move",              
            
            div(class="modal-content",
                h3(htmlOutput("varTitle")),
                p("Click map to show pixel data."),
                plotOutput("plotHist", height="100%"),
                p(),
                tableOutput("tableSum")
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


