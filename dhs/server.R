#####################################################################################
# Title: Visualize DHS Regional Estimates
# Date: Januray 2015
# Project: HarvestChoice for A4NH
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

load("../dhs/data/dhsMap.2014.10.16.RData")

# Helper - Archive spatial formats for download
writeZip <- function(x, file, filename, format, ...) {
  if (format=="ESRI Shapefile") {
    rgdal::writeOGR(x, "./", filename, format, overwrite_layer=T, check_exists=T)
  } else {
    writeRaster(x, filename, format, bylayer=F, overwrite=T, ...)
  }
  f <- list.files(pattern=paste0(strsplit(filename, ".", fixed=T)[[1]][1], ".*"))
  zip(paste0(filename, ".zip"), f, flags="-9Xjm", zip="zip")
  file.copy(paste0(filename, ".zip"), file)
  file.remove(paste0(filename, ".zip"))
}


# Helper - Merge attributes and symbolize (all years at once)
genMap <- function(iso, res, var, col, brks) {
  dt <- dhs[country_code==iso & hv025==res, .SD, .SDcols=c("svyCode", "hv024", var)]
  setnames(dt, var, "var")
  m <- gis.web[gis.web$iso==iso, ]
  setkey(dt, svyCode, hv024)
  dt <- dt[J(m$svyCode, m$regCode)]
  rg <- range(dt$var, na.rm=T)

  # Try equal interval breaks
  cv <- try(classInt::classIntervals(dt$var, n=min(brks, dt[, length(unique(var))], na.rm=T))$brks)
  if (class(cv)=="try-error") {
    return(class(cv))

  } else {
    # Symbolize
    dt[, cl := cut(var, unique(c(rg[1]-1, cv, rg[2]+1)), cutlabels=F, ordered_result=T)]
    dt[, cl := brewer.pal(length(cv)+1, col)[cl]]
    m@data <- cbind(m@data, dt)
    return(m)
  }
}



shinyServer(function(input, output, session) {

  # Init map
  #   map <- leaflet() %>%
  #     addTiles("http://{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
  #       attribution=HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>')) %>%
  #     setView(8, 8, 6)

  # Render map
  #  output$map <- renderLeaflet(map)

  # List of indicators
  output$selectVar <- renderUI({
    var <- dhs.lbl[varCat==input$selectCat, varCode]
    names(var) <- dhs.lbl[varCat==input$selectCat, varLabel]
    selectInput("selectVar", "Choose an Indicator", var,
      selected=var[1], size=11, width="100%", selectize=F)
  })

  # Init values
  values <- reactiveValues(
    var="i101_hv271_wealth_index",
    y=c("2003", "2008"),
    g=genMap("GH", "rural", "i101_hv271_wealth_index", "RdBu", 5))

  # Primary observer
  observeEvent(input$btn, priority=1, {

    s_iso <- input$selectISO
    s_var <- input$selectVar

    # Get all country surveys
    vars <- names(dhs)[names(dhs) %like% s_var]
    dt <- dhs[country_code==s_iso, .SD, .SDcols=c("year", "hv025", "hv024", "regName", vars)]
    if (dim(dt)[2]>5) setnames(dt, 5:6, c("Female", "Male")) else setnames(dt, 5, "mean")
    dt <- melt(dt, id.vars=c("year", "hv025", "hv024", "regName"))
    setnames(dt, 1:4, c("year", "residence", "regCode", "regName"))
    values$dt1 <- dt

    # Update other title
    output$txtSubTitle <- renderText({
      out <- h3("Sub-National Series", br(),
        tags$small(dhs.lbl[varCode==s_var, varLabel]))
      return(as.character(out))
    })

    # Update details
    output$txtHead <- renderText({
      out <- span(strong(toupper(names(iso)[iso==s_iso])), br(), dhs.lbl[varCode==s_var, varLabel])
      return(as.character(out))
    })

    # Update year
    values$y <- svyYear[[s_iso]]
    updateRadioButtons(session, "selectYear", choices=values$y, selected=tail(values$y, 1))

    # Update gender
    if (dhs.lbl[varCode==s_var, gender]==T) {
      updateRadioButtons(session, "selectGender", choices=c(male="_m", female="_f"), selected="_f")
    } else {
      updateRadioButtons(session, "selectGender", choices=c(`n/a`=""), selected="")
    }

    # Export
    values$iso <- s_iso
  })


  # Secondary observer
  observeEvent({input$btn+input$btnUpdate}, priority=-1, {

    # Clear any alert
    closeAlert(session, "noData")
    closeAlert(session, "noInd")

    svyCode <- paste0(input$selectISO, input$selectYear)

    # Validate choices
    if (!svyCode %in% unique(gis.web@data$svyCode)) {
      # Data is missing for that survey
      createAlert(session, "alertNoData", alertId="noData",
        message="Try another country year combination.",
        title="Missing Data", type="warning", append=F)

    } else {

      # Symbolize features
      var <- paste0(input$selectVar, input$selectGender)
      g <- genMap(input$selectISO, input$selectRes, var,
        input$col, input$brks)

      if (class(g)=="character") {
        # Data is either missing or classInt failed
        createAlert(session, "alertNoData", alertId="noInd",
          message="Sorry, no data for this indicator.",
          title="Missing Data", type="warning", append=F)

      } else {

        # Map it
        s_g <- g[g$svyCode==svyCode,]
        # Export
        values$var <- var
        values$g <- g
        values$s_g <- s_g

        #           coords <- apply(sp::coordinates(s_g), 2, mean, na.rm=T)
        #           m <- map %>%
        #             setView(coords[1]+3, coords[2], 6) %>%
        #             addPolygons(data=s_g, layerId=row.names(s_g), fillColor=s_g@data$cl,
        #               weight=.6, color="white", fillOpacity=0.7,
        #               popup=paste0(
        #                 "<small>Region</small><strong><br/>", s_g@data$regName, "</strong><br/>",
        #                 "<small>Value</small><strong><br/>", round(s_g@data$var, 2), "</strong>"))
        #           output$map <- renderLeaflet(m)

      }
    }
  })


  # List of survey tables
  output$svydt <- renderTable(digits=1, include.rownames=F, {
    if (is.null(values$dt1)) return()
    isolate({
      dt <- values$dt1
      dt <- dcast.data.table(dt, regCode+regName~year+residence+variable)
      dt <- setnames(dt, gsub("_", "\n", names(dt), fixed=T))
      dt <- setnames(dt, 1:2, c("Code", "Region"))
      return(dt)
    })
  })


  # Update title
  output$txtTitle <- renderText({
    if (is.null(values$g)) return()
    isolate({
      out <- h3(names(iso)[iso==input$selectISO], " - ", input$selectRes, br(),
        tags$small(dhs.lbl[varCode==values$var, varLabel]))
      return(as.character(out))
    })
  })


  # Plot maps
  output$mapplot <- renderPlot(
    height=function() {if(length(values$y)>2) 800 else 400}, {
      if (is.null(values$g)) return()
      isolate({
        g <- values$g
        par(fg="#444444", bty="n", family="Helvetica-Narrow",
          cex.main=1.6, cex.axis=.7, font.main=1, adj=0)

        switch(as.character(length(values$y)),
          `2`=layout(matrix(1:3, 1, 3, byrow=T), widths=c(3,3,1)),
          `3`=layout(matrix(1:4, 2, 2, byrow=T), widths=c(1,1)),
          `4`=layout(matrix(c(1,2,5,3,4,5), 2, 3, byrow=T), widths=c(3,3,1)))

        for (i in values$y) {
          j <- g[g$svyYear==i,]
          plot(j, col=j@data$cl, border="#ffffff", main=paste0("Survey Year: ", i))
          axis(1, tck=1, lty=3, lwd=.5, col="gray")
          axis(2, tck=1, lty=3, lwd=.5, col="gray")
          text(coordinates(j), labels=j@data$regName, col="#444444", cex=.8, font=1)
        }

        plot(0:1, 0:1, type="n", xlab="", ylab="", axes=F)
        legend("left", title="Legend", xpd=NA,
          bty="n", lty=-1, pch=15, cex=1.6, pt.cex=3.2,
          legend=c("High", rep("", input$brks-2), "Low"),
          col=rev(brewer.pal(input$brks, input$col)))
      })
    })


  #   # Time serie plot
  #   output$tsplot <- renderChart({
  #     if (is.null(values$dt1)) return()
  #     dt <- values$dt1
  #     p <- rPlot(value~year|residence, data=dt, color="regName", type="line")
  #     p$set(dom="tsplot")
  #     return(p)
  #   })


  # Show admin details on mouseover
  #   output$tips <- renderText({
  #     evt <- input$map_mouseover
  #     if (!is.null(evt)) {
  #       evt <- g@data[evt,]
  #       out <- p(
  #         evt$regName, " (code: ", evt$regCode, ")", br(),
  #         "Year: ", strong(evt$year), br(),
  #         "Value: ", strong(evt$value))
  #     }
  #     return(as.character(out))
  #   })


  # Brewer color palettes
  output$plotBrewer <- renderPlot(height=500, {
    if(input$btnShowBrewer==0) return()
    par(mar=c(0,3,0,0))
    display.brewer.all()
  })


  # Download
  output$saveData <- downloadHandler(function() {
    t <- input$fileType
    f <- paste0(input$selectISO, "-", input$selectVar)
    if (t %in% c("csv", "dta")) {
      # Complete file path
      paste0(f, ".", t)
    } else {
      # File path with `.zip`
      paste0(year(), "-", f, ".", t, ".zip")
    }
  }, function(file) {

    t <- input$fileType
    f <- paste0(input$selectISO, "-", input$selectVar, ".", t)

    switch(t,
      csv = write.csv(values$dt1, file, row.names=F, na=""),
      dta = foreign::write.dta(values$dt1, file, version=9L),
      shp = {
        # TODO Use original boundaries for download
        writeZip(values$s_g, file, f, "ESRI Shapefile")
      })
  })

})
