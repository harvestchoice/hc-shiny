library(shiny)
library(shinyBS)
library(leaflet)

# Define UI for random distribution application 
shinyUI(fluidPage(
        title="Canvas Plots with ggvis",
        theme="bootstrap.css",      
        
        leafletMap("map", width="100%", height="100%",
            initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
            initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
            options=list(center=c(20, 20), zoom=7)
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
                hr(),                
                #h4("Province Summary"),
                #tableOutput("tableVar"),
                plotOutput("plotHist", height="100%"),
                p(),
                tableOutput("tableSum")
            )
        ),
        
        absolutePanel(id="filter", fixed=F, draggable=T,
            top="auto", left=20, right="auto", bottom=60,
            width="auto", height="auto", cursor="move",              
            
            div(class="modal-content", style="height:180px;",
                h3(htmlOutput("varTitle")),
                p("Filter Layer to Min/Max"),
                column(2, uiOutput("selectMin", inline=T)),
                column(2, uiOutput("selectMax", inline=T))
            )
        ),        
        
        absolutePanel(id="cite", fixed=F, draggable=F,
            top="auto", left=10, right="auto", bottom=0,
            width="auto", height="auto",  
            p("IFPRI/HarvestChoice, 2014. Source code at ",
            a("https://github.com/harvestchoice/hc-shiny"), ".")
        
        )
    )
)


