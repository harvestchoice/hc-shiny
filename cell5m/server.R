#####################################################################################
# Title:   HarvestChoice Data API with leaflet
# Date:    July 2015
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################


# Helper - construct list of variables per category
varlst <- function(cat="Population") {
  tmp <- vi[genRaster==T & type=="continuous" & cat2==cat, varCode, by=cat3]
  tmp <- split(tmp, tmp$cat3, drop=T)
  tmp <- lapply(tmp, function(x) x$varCode)
  for (i in 1:length(tmp)) names(tmp[[i]]) <- vi[J(tmp[[i]])][, varLabel]
  return(tmp)
}

# Helper - return list of domain layers
domlst <- function() {
  # Just select random 10 variables to test
  tmp <- c(my_dom, vi$varCode[sample(1:600, 10)])
  names(tmp) <- vi[tmp][, varLabel]
  return(tmp)
}


# Helper - Filter layer
dtFilter <- function(x, filter) {
  tmp <- x[var >= filter[1] & var <= filter[2]]
  return(tmp)
}

# Helper - Compute 5 stats
stats <- function(x) {
  tmp <- summary(x)
  tmp <- data.table(Statistic=names(tmp), Value=tmp)
  return(tmp)
}


# Helper - Return layer as data.table and add color legend
getIndicator <- function(var="PN05_TOT", iso3="GHA", ...) {
  # Query hcapi3
  tmp <- getLayer(var, iso3, ...)
  setkey(tmp, X, Y)
  setnames(tmp, length(names(tmp)), "var")
  tmp <- tmp[!is.na(var)]
  tmp <- tmp[ADM1_NAME_ALT!="buffer gridcell"]

  # Get default symbology from `vi`
  cc <-  as.character(unlist(strsplit(vi[var][, classColors], "|", fixed=T)))
  cv <- try(classIntervals(tmp$var, style="kmeans")$brks)

  if (class(cv)=="try-error") {
    # Not enough data for kmeans, alert, and create empty data.table
    createAlert(session, "alertNoData",
      title="No Data!",
      message="Choose another combination.",
      type="warning", block=T)
    tmp <- data.table(X=NA, Y=NA, var=NA, col=NA)

  } else {
    # kmeans worked, good to classify
    rg <- range(tmp$var, na.rm=T)
    tmp[, col := cut(var, unique(c(rg[1]-1, cv, rg[2]+1)), cutlabels=F, ordered_result=T)]
    tmp[, col := colorRampPalette(cc)(length(cv)+1)[col]]
    tmp[is.na(col), col := "#ffffff"]
  }

  return(tmp)
}


shinyServer(function(input, output, session) {

  # Init reactive values
  selected <- reactiveValues(
    iso3="GHA")

  session <- reactiveValues(
    iso=character(0),
    var=character(0),
    varTitle=character(0))

  #onclick("selectCat", shinyjs::toggle(id="panelInd", anim=T))

  # Create the map
  map <- leaflet() %>%
    # Center map on Ghana
    setView(20, 1, 4) %>%
    # Add basemap
    addTiles(
      urlTemplate="http://{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution="Mapbox", layerId="basemap", group="Basemap")

  output$map <- renderLeaflet(map)


  # Update indicator menu
  observeEvent(input$selectCat, priority=3, {

    # Toggle panel
    shinyjs::show(id="panelInd", anim=T)

    selected$cat <- gsub("-", " ", input$selectCat, fixed=T)

    output$selectVar <- renderUI({
      var <- varlst(selected$cat)
      lapply(1:length(var), function(x) checkboxGroupInput("selectVar",
        label=names(var)[x], choices=var[[x]]))
    })

  })


  # TODO Update selected layer panel


  # Main observer
  observeEvent(input$btnLayer, priority=2, {

    # Toggle panels
    show(id="panelDetails", anim=T)
    hide(id="panelInd", anim=T)

    # Hide layer
    leafletProxy("map") %>%
      hideGroup(selected$varTitle)

    # Update input values
    selected$var <- input$selectVar
    selected$iso3 <- input$selectISO3
    selected$varTitle <- vi[selected$var][, varTitle]

    # Update session history
    session$iso3 <- c(session$iso3, selected$iso3)
    session$var <- c(session$iso3, selected$var)
    session$varTitle <- c(session$iso3, selected$varTitle)

    # Query layer stats
    tmp <- getIndicator(selected$var, selected$iso3)
    selected$dt <- tmp
    s <- stats(tmp$var)

    output$tableSum <- renderTable(digits=0, include.rownames=F,
      format.args=list(big.mark=",", decimal.mark="."), s)

    output$plotHist <- renderPlot(width=220, height=220, {
      par(mar=c(2,2,0,0), bty="n", family="Helvetica-Narrow", cex.axis=.8)
      hist(tmp$var, col=4, border="white", main=NULL, ylab=NULL, xlab=NULL)
    })

    # Update filter
    updateSliderInput(session, "selectFilter",
      min=s[1, Value], max=s[6, Value], value=c(s[1, Value], s[6, Value]))

    # Convert to raster
    r <- SpatialPixelsDataFrame(tmp[, list(X, Y)], data.frame(tmp),
      tolerance=0.00360015, proj4string=CRS("+init=epsg:4326"))
    r <- raster(r, layer="var")
    cc <-  as.character(unlist(strsplit(vi[selected$var][, classColors], "|", fixed=T)))

    # Update map
    leafletProxy("map") %>%
      # Recenter map if country has changed
      setView(mean(tmp$X+2, na.rm=T), mean(tmp$Y, na.rm=T), 6) %>%
      # Add raster
      addRasterImage(r, group=selected$varTitle, opacity=0.8, maxBytes=20*1024*1024,
        colors=cc)

    # Add clickable gridcells as circles
    if (!selected$iso3 %in% session$iso3) {
      leafletProxy("map") %>%
        addCircles(data=tmp, layerId=~CELL5M,
          group=paste0(selected$iso3, " 10km grid"),
          lng=~X, lat=~Y, radius=5000, stroke=F, fillColor=F)
    }


  })



  observeEvent(input$btnFilter, priority=0, {

    # Filter results
    tmp <- dtFilter(selected$dt, input$selectFilter)

    # Convert to raster
    r <- SpatialPixelsDataFrame(tmp[, list(X, Y)], data.frame(tmp),
      tolerance=0.00360015, proj4string=CRS("+init=epsg:4326"))
    r <- raster(r, layer="var")
    cc <-  as.character(unlist(strsplit(vi[selected$var][, classColors], "|", fixed=T)))

    # Update map
    leafletProxy("map") %>%
      clearGroup(selected$varTitle) %>%
      # Add raster
      addRasterImage(r, group=selected$varTitle, opacity=0.8, maxBytes=20*1024*1024,
        colors=cc)

    # Update histogram
    output$plotHist <- renderPlot(width=220, height=220, {
      par(mar=c(2,2,0,0), bty="n", family="Helvetica-Narrow", cex.axis=.8)
      hist(tmp$var, col=4, border="white", main=NULL, ylab=NULL, xlab=NULL)
    })

  })


  # Update title
  output$varTitle <- renderText({selected$varTitle})

  # Update popup details
  output$details <- renderText({
    e <- selected$event
    s <- p(
      "CELL5M: ", e$CELL5M,
      "Lat: ", e$Y,
      "Long: ", e$X,
      "Province: ", e$ADM1_NAME_ALT,
      "District: ", e$ADM2_NAME_ALT,
      "Value: ", e$var, " ", vi[selected$var][, unit])
    return(as.character(s))
  })

  output$saveData <- downloadHandler(
    function() paste0("data-", Sys.Date(), ".zip"),
    function(file) file.copy(genFile(selected$var, selected$iso3, format=input$fileType), file)
  )


  # When features are clicked, show details in panelDetails
  observeEvent(input$map_shape_click, priority=3, {
    e <- input$map_shape_click
    selected$event <- selected$dt[CELL5M==e$id]
  })


  ################################################################################
  # Domain Summary
  ################################################################################

  #   output$selectDomain <- renderUI({ selectInput("selectDomain",
  #     "Choose a layer to summarize by", domlst())
  #   })
  #
  #   output$tableDomain <- renderTable(digits=0, include.rownames=F,
  #     format.args=list(big.mark=",", decimal.mark="."), {
  #       dtDomain()
  #     })
  #
  #   domby <- reactive({
  #     # Bound to btnDomain
  #     if (input$btnDomain==0) my_dom else isolate(input$selectDomain)
  #   })
  #
  #   # Summarize layer
  #   dtDomain <- reactive({
  #     tmp <- getLayer(var(), iso3(), domby())
  #     setkeyv(tmp, domby())
  #     setnames(tmp, 1:2, vi[c(domby(), var())][, varLabel])
  #   })
  #
  #   drawObsDomain <- observe({
  #     # Bound to btnMapDomain
  #     if (input$btnDomain==0) return()
  #
  #     isolate({
  #       # Clear existing circles before drawing
  #       map$clearShapes()
  #       # Summarize and symbolize layer
  #       tmp <- getCircles(var(), iso3(), domby(), collapse=F)
  #       # Draw circles
  #       map$addCircle(
  #         tmp$Y, tmp$X, 5000, tmp$CELL5M,
  #         options=list(stroke=F, fillOpacity=0.55, fill=T),
  #         eachOptions=list(fillColor=tmp$my_col))
  #     })
  #   })


  ################################################################################
  # Homologue Tool
  ################################################################################


  #   selectedLayer <- as.character(NA)
  #
  #   output$selectRank <- reactive({
  #     paste(unique(addRank()[!is.na(addRank())]), collapse="<br/>")
  #   })
  #
  #   addRank <- reactive({
  #     input$btnAddRank
  #     isolate({ selectedLayer <<- c(selectedLayer, input$selectVar) })
  #     return(selectedLayer)
  #   })
  #



})
