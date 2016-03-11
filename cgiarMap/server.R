
#####################################################################################
# Main
#####################################################################################
shinyServer(function(input, output, session) {


  # Init
  values <- reactiveValues(iso3="SSA")
  #output$map <- renderLeaflet(m)


  })


  # Download handler
  output$saveData <- downloadHandler(function() {
    f <- "popTrends_SSA_2000-2020"
    if (input$fileType %in% c("csv", "dta", "png")) paste0(f, ".", input$fileType)
    else paste0(f, ".zip")

  }, function(file) {
    switch(input$fileType,
      csv = file.copy("./www/popTrends_SSA_2000-2020.csv", file),
      dta = file.copy("./www/popTrends_SSA_2000-2020.dta", file),
      shp = file.copy("./www/popTrends_SSA_2000-2020.zip", file),
      png = file.copy("./www/popTrends_SSA_2000-2020.png", file)
    )
  })

})
