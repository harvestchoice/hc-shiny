#####################################################################################
# Title:   Map Population Hotspots 2000-2020 or 1990-2015
# Date:    March 2016
# Project: SDA for PIM
# Author:  Bacou, Melanie <mel@mbacou.com>
#####################################################################################


shinyServer(function(input, output, session) {

  # Init
  values <- reactiveValues(iso3="SSA")
  output$map <- renderLeaflet(m)

  # Update country select
  observeEvent(input$selectISO3, values$iso3 <- input$selectISO3)

  observe({

    # Title
    output$title <- renderUI(
      h2(names(iso)[iso==values$iso3],
        tags$small("  2020 population projected at",
          if(values$iso3!="SSA") gpw.dt[ISOALPHA==values$iso3,
            prettyNum(sum(UN_2020_E, na.rm=T)/1E6, digits=2, big.mark=",")]
          else gpw.dt[,
            prettyNum(sum(UN_2020_E, na.rm=T)/1E6, digits=2, big.mark=",")],
          "million")))

    # Charts
    p1(values$iso3) %>% bind_shiny("p1")
    p2(values$iso3) %>% bind_shiny("p2")
    p3(values$iso3) %>% bind_shiny("p3")

    # Recenter
    if(values$iso3 != "SSA") {
      ext <- bbox(gpw.urb2[gpw.urb2$ISOALPHA==values$iso3,])
      leafletProxy("map") %>%
        fitBounds(ext[1,1], ext[2,1], ext[1,2], ext[2,2])
    }


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
