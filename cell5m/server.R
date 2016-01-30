
#####################################################################################
# Helper - Make leaflet color palette
#####################################################################################
# Sensible legend wrapping
titleWrap <- function(x) gsub("\n", "<br />", stringr::str_wrap(x, 15), fixed=T)


#####################################################################################
# Helper - construct list of variables per category
varlst <- function(cat="Population") {
  tmp <- vi[genRaster==T & cat2==cat, varCode, keyby=list(cat3, sortOrder)]
  if (nrow(tmp)<1) return(character(0))
  tmp <- split(tmp, tmp$cat3, drop=T)
  tmp <- lapply(tmp, function(x) x$varCode)
  for (i in 1:length(tmp)) names(tmp[[i]]) <- vi[tmp[[i]]][, paste0(varLabel, " (", unit, ")")]
  return(tmp)
}

# Helper - return list of domain layers
domlst <- function() {
  # Just select random 10 variables to test
  tmp <- c(my_dom, vi$varCode[sample(1:600, 10)])
  names(tmp) <- vi[tmp][, varLabel]
  return(tmp)
}


#####################################################################################
# Helper - Return layer as data.table
#####################################################################################
getIndicator <- function(var="PN05_TOT", iso3="GHA", ...) {
  tmp <- hcapi(var, iso3, ...)
  tmp <- tmp[ADM1_NAME_ALT!="buffer gridcell"]
  return(tmp)
}


#####################################################################################
# Helper - Make generic CELL5M layer UI box
#####################################################################################
# Built from a list of shiny UI widgets
# Pass the actual dataset coz we don't want to re-hit the API constantly

genCell5mUI <- function(x, var, iso3) {

  # var is a unique varCode
  var <- var[1]

  # Title
  l1 <- h3(paste(names(iso)[iso==iso3], vi[var][, varTitle], sep=" - "))

  # 5 stats
  s <- x[, lapply(.SD, summary), .SD=var]
  row.names(s) <- c("Min", "1st-Qtl", "Median", "Mean", "3rd-Qtl", "Max", "NAs")[1:nrow(s)]
  l2 <- renderTable(digits=0, include.rownames=T,
    format.args=list(big.mark=",", decimal.mark="."), s)

  # Histogram
  l3 <- renderPlot(height=180, {
    par(mar=c(2,2,0,0), bty="n", family="Helvetica-Narrow", cex.axis=.8)
    hist(x[[var]], col="grey50", border="white", main=NULL, ylab=NULL, xlab=NULL)
  })

  # Update filter
  l4 <- sliderInput(paste0("selectFilter_", var), "Filter layer to Min/Max",
    min=s[[var]][1], max=s[[var]][6], value=c(s[[var]][1], s[[var]][6]))

  l5 <- actionButton(paste0("btnFilter_", var), "Update Layer", icon("globe"))

  box(width=12,
    column(6, l1, l3, l4, l5),
    column(6, l2))
}




#####################################################################################
# Main
#####################################################################################
function(input, output, session) {

  # Init reactive values
  selected <- reactiveValues(
    iso3="GHA",
    cat=character(0),
    var=character(0))

  # Init user session
  usr <- reactiveValues(
    iso=character(0),
    var=character(0))

  # UI
  onclick("selectCat", hide(id="panelInd", anim=T))

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(20, 1, 4) %>%  # Ghana
      addTiles(
        urlTemplate="http://{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution="Mapbox", layerId="basemap", group="Basemap")
  })

  # obsCat - Update indicator menu
  observeEvent(input$selectCat, label="obsCat", {
    selected$cat <- gsub("-", " ", input$selectCat, fixed=T)
    output$chkGroupVar <- renderUI({
      v <- varlst(selected$cat)
      lapply(1:length(v), function(x) checkboxGroupInput(paste0("selectVar", x),
        label=names(v)[x], choices=v[[x]]))
    })
    show(id="panelInd", anim=T)
  })


  # TODO Update selected layer panel

  #   output$layerMenu <- renderMenu({
  #     msg <- lapply(usr$var, function(x) messageItem(icon=icon("file-alt", lib="font-awesome"),
  #       from=vi[x][, varTitle], message=vi[x][, varDesc]))
  #     dropdownMenu(type="messages", .list=msg, icon=icon("folder-close"))
  #   })

  output$layerMenu <- renderMenu({
    msg <- lapply(usr$var,
      function(x) tags$li(tags$strong(vi[x][, varTitle]), br(), vi[x][, varDesc]))
    dropdownMenu(.list=msg)
  })


  # obsMap - Main observer
  observeEvent(input$btnLayer, label="obsMap", priority=2, {

    # Update input values
    svar <- names(input)
    svar <- svar[svar %like% "selectVar"]
    svar <- unlist(sapply(svar, function(x) input[[x]]))
    svar <- svar[!is.na(svar)]

    # Hide unselected layers
    leafletProxy("map") %>% hideGroup(usr$var)
    if (is.na(svar)) return()

    selected$var <- svar
    selected$iso3 <- input$selectISO3

    # Append to session history
    usr$var <- unique(c(usr$var, selected$var))
    usr$iso3 <- unique(c(usr$iso3, selected$iso3))

    # Query layer stats
    dt <- getIndicator(selected$var, selected$iso3)

    # Export
    selected$dt <- dt
    selected$svar <- svar

    # Show tab
    updateTabItems(session, "selectTool", selected="Overview")

    # Output cell5mUI boxes
    output$cell5mUI <- renderUI({
      for (i in svar) genCell5mUI(dt, i, selected$iso3)
    })

    # Convert to raster
    r <- SpatialPixelsDataFrame(dt[, list(X, Y)], data.frame(dt),
      tolerance=0.00360015, proj4string=CRS("+init=epsg:4326"))
    r <- brick(r)
    # r <- projectRaster(r, crs=CRS("+init=epsg:3857"))
    cc <- unlist(strsplit(vi[svar][, classColors], "|", fixed=T))
    cc <- gsub("ffffffff", "00000000", cc, fixed=T)
    ext <- bbox(r)

    # Update map
    leafletProxy("map") %>%
      fitBounds(ext[1,1], ext[2,1], ext[1,2], ext[2,2]) %>%
      addRasterImage(raster(r, layer=svar), group=svar, opacity=.8, colors=cc,
        project=F, maxBytes=Inf)

    # Update legend
    #     addLegend("bottomright", layerId="lgd", opacity=1,
    #       pal="Spectral",
    #       title=titleWrap(selected$varTitle),
    #       labFormat=labelFormat(digits=2))

    # Add clickable gridcells as circles
    if (!selected$iso3 %in% c("SSA", usr$iso3)) {
      leafletProxy("map") %>%
        addCircles(data=dt, layerId=~CELL5M,
          group=paste0(selected$iso3, " 10km grid"),
          lng=~X, lat=~Y, radius=4000, stroke=F,
          fillColor="#fff", fillOpacity=0)
    }


  })


  # obsFilter
  observeEvent(input$btnFilter, label="obsFilter", {

    # Import
    svar <- selected$svar
    dt <- copy(selected$dt)

    # Filter raster
    setnames(dt, svar, "value")
    dt <- dt[value %between% input$selectFilter]
    r <- SpatialPixelsDataFrame(dt[, list(X, Y)], data.frame(value=dt$value),
      tolerance=0.00360015, proj4string=CRS("+init=epsg:4326"))
    r <- raster(r)
    cc <- unlist(strsplit(vi[svar][, classColors], "|", fixed=T))
    cc <- gsub("ffffffff", "00000000", cc, fixed=T)

    # Update map
    leafletProxy("map") %>%
      #clearGroup(svar) %>%
      hideGroup(usr$var) %>%
      addRasterImage(r, group=svar, opacity=0.8, project=F, colors=cc)

    # Update histogram
    output$plotHist <- renderPlot(width=220, height=220, {
      par(mar=c(2,2,0,0), bty="n", family="Helvetica-Narrow", cex.axis=.8)
      hist(r, col="grey50", border="white", main=NULL, ylab=NULL, xlab=NULL)
    })

  })


  # Update popup details
  #   output$details <- renderText({
  #     e <- selected$event
  #     s <- p(
  #       "CELL5M: ", e$CELL5M,
  #       "Lat: ", e$Y,
  #       "Long: ", e$X,
  #       "Province: ", e$ADM1_NAME_ALT,
  #       "District: ", e$ADM2_NAME_ALT,
  #       "Value: ", e$var, " ", vi[selected$var][, unit])
  #     return(as.character(s))
  #   })

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


}
