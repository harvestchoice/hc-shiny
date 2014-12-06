library(shiny)
library(ggvis)

# Define UI for random distribution application 
shinyUI(fluidPage(
        title="Canvas Plots with ggvis",
        theme="bootstrap.css",      
        
        column(2,             
            
            uiOutput("selectCat"),
            uiOutput("selectVar"),
            uiOutput("selectISO3"),
            uiOutput("hist_ui"),               
            uiOutput("ggvis_ui")                 
        ),
        
        column(3, offset=1,
            #h4("Province Summary"),
            #tableOutput("tableVar"), 
            ggvisOutput("ggvis")
        ),
        
        column(3, offset=1,
            ggvisOutput("hist")
        ),
        
        column(2,
            tableOutput("tableSum")
        ),
        
        column(12, id="cite", 
            p("IFPRI/HarvestChoice, 2014.")
        
        )
    )
)

