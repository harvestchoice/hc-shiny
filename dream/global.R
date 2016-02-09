#####################################################################################
# Title:   DREAM Relaunched
# Date:    January 2016
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################


library(knitr)
library(tools)
library(rhandsontable)
#library(Matrix)
#library(data.table)

# Load sample input data and model
#load("./data/dream.RData")

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
