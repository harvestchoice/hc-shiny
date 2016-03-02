#####################################################################################
# Title:   HarvestChoice Tablr v2.0
# Date:    February 2016
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################


# Helper - construct list of CELL5M variables
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


# Helper - Return layer as data.table
getIndicator <- function(var="PN05_TOT", iso3="GHA", ...) {
  tmp <- hcapi(var, iso3, ...)
  tmp <- tmp[ADM1_NAME_ALT!="buffer gridcell"]
  tmp[, X := NULL]
  tmp[, Y := NULL]
  setkey(tmp, "ADM1_NAME_ALT")
  setnames(tmp, vi[names(tmp), varLabel])
  setnames(tmp, "Adm-1 Name (GAUL, 2008 mod.)", "Province")
  setnames(tmp, "Adm-2 Name (GAUL, 2008 mod.)", "District")
  setnames(tmp, "Adm-0 Name (GAUL, 2008)", "Country")
  return(tmp)
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


  # Init pivot table with default vars
  output$pvt <- renderRpivotTable({
    dt <- getIndicator(c("ADM1_NAME_ALT", "AEZ5_CLAS", "AREA_TOTAL"), selected$iso3)
    rpivotTable(dt,
      rows=c("Province"), cols="AEZ-5 Tropics", vals="Land Area '00",
      aggregatorName="Sum", rendererName="Table")
  })


  # obsMap - Main observer
  observeEvent(input$btnLayer, label="obsMap", priority=2, {

    # Update input values
    var <- names(input)
    var <- var[var %like% "selectVar"]
    var <- unlist(sapply(var, function(x) input[[x]]))
    var <- var[!is.na(var)]

    # Hide unselected layers
    hide("panelInd", T)
    if (is.na(var)) return()
    selected$var <- var
    selected$iso3 <- input$selectISO3

    # Query layer stats
    dt <- getIndicator(c("ADM1_NAME_ALT", selected$var), selected$iso3)

    # Pivot table
    output$pvt <- renderRpivotTable({
      rpivotTable(dt,
        rows=c("Province"), vals=last(selected$var),
        aggregatorName="Sum", rendererName="Table")
    })



  })

}
