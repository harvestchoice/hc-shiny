#####################################################################################
# Title:   HarvestChoice CELL5M Data Validation Tests
# Date:    July 2015
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

# Load libraries
library(hcapi3)
library(shiny)

# Load GAUL admin-2
g2 <- readRDS("/home/projects/hc-cell5m/rdb/g2_2008v09.web.rds")
g2.dt <- data.table(g2@data)
g2.dt[, rn := row.names(g2)]

# Key vi
setkey(vi, varCode)

# Renderer for rhandsometable
convertNA <- function() htmlwidgets::JS(
  "function(instance, TD, row, col, prop, value, cellProperties) {
  if (value === 'NA' | value === 'NaN') {
  value = '--';
  Handsontable.renderers.TextRenderer(instance, TD, row, col, prop, value, cellProperties);
  } else {
  Handsontable.renderers.NumericRenderer(instance, TD, row, col, prop, value, cellProperties);
  }
  }")
