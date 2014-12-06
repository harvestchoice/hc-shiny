library(shiny)
library(ggvis)
library(leaflet)

# Define UI for random distribution application 
shinyUI(fluidPage(
        title="Canvas Plots with ggvis",
        theme="bootstrap.css",      
        
        leafletMap("map", width="100%", height="100%",
            initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
            initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
            options=list(center=c(20, -20), zoom=7)
        ),          
        
        
        absolutePanel(id="results", fixed=F, draggable=T,
            top="auto", left="auto", right=20, bottom=10,
            width=480, height="auto", cursor="move",              
            
            div(class="modal-content",
                #h4("Province Summary"),
                #tableOutput("tableVar"),
                h4("Layer Statistics"),
                ggvisOutput("hist"),
                tableOutput("tableSum")
            )
        ),        
        
        absolutePanel(id="controls", fixed=T, draggable=F,
            top=20, left="auto", right=20, bottom="auto",
            width=260, height="auto",
            
            div(class="modal-content",
                uiOutput("selectCat"),
                uiOutput("selectVar"),
                uiOutput("selectISO3"),
                #uiOutput("ggvis_ui"),
                uiOutput("hist_ui")
            )
        ),
        
        absolutePanel(id="cite", fixed=F, draggable=F,
            top="auto", left=10, right="auto", bottom=0,
            width="auto", height="auto",  
            p("IFPRI/HarvestChoice, 2014.")
        
        )
    )
)


