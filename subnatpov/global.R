#####################################################################################
# Title:   HarvestChoice Subnational Poverty
# Date:    August 2015
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

setwd("/home/projects/shiny/tmp")

# Load common libraries
library(data.table)
library(leaflet)
library(rgdal)
library(shiny)
library(rhandsontable)

# Load latest revision
m <- readRDS("../subnatpov/svyPov_20150804.rds")
m@data <- data.frame(m@data)

# constants
def <- c("circa 2008", "circa 2005")
pal <- rev(RColorBrewer::brewer.pal(11, "RdYlGn"))

# Make a list of indicators
vars <- c(
  `Headcount ratio under $2/day`="hc_poor2",
  `Headcount ratio under $1.25/day`="hc_poor1",
  `Poverty gap under $2/day`="povgap_ppp2",
  `Poverty gap under $1.25/day`="povgap_ppp1",
  `Poverty severity under $2/day`="sevpov_ppp2",
  `Poverty severity under $1.25/day`="sevpov_ppp1",
  `HCR Std. Dev. under $2/day`="sd_poor2",
  `HCR Std. Dev. under $1.25/day`="sd_poor1",
  `Gini coefficient`="gini",
  `Poverty headcount below $2/day`="num_poor2",
  `Poverty headcount below $1.25/day`="num_poor1",
  `Per capita expenditure, mean per annum`="pcexp_ppp_m",
  `Food expenditure, mean per annum`="foodexp_ppp_m",
  `Non-food expenditure, mean per annum`="nfoodexp_ppp_m",
  `Total population`="totpop")

# List of countries
load("/home/projects/cell5m/rdb/latest/lookup.RData")
rm(g, readme)
iso <- iso[iso %in% c("SSA", levels(m@data$ISO3)[-c(13,20,33,36,47)])]

# GAUL Country boundaries
g0 <- readRDS("/home/projects/cell5m/rdb/g0.epsg3857.rds")

# Helper - Renderer for rhandsometable
convertNA <- function() htmlwidgets::JS(
  "function(instance, TD, row, col, prop, value, cellProperties) {
  if (value === 'NA' | value === 'NaN') {
  value = '--';
  Handsontable.renderers.TextRenderer(instance, TD, row, col, prop, value, cellProperties);
  } else {
  Handsontable.renderers.NumericRenderer(instance, TD, row, col, prop, value, cellProperties);
  }
  }")

