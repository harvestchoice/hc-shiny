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
library(ggvis)

# Load latest revision (web minified)
m <- readRDS("../subnatpov/svyPov_web_20150808.rds")

# Note: weirdly leaflet needs shapes at epsg:4326,
# tmap needs epsg:3857 and had to rename vars
# m$rn <- NULL
# m <- spTransform(m, CRS("+init=epsg:4326"))
# names(m)[11:25] <- paste0("total_", names(m)[11:25])
# names(m)[6] <- "adminUnit"
# m@data <- data.frame(m@data)
# saveRDS(m, "../subnatpov/svyPov_web_20150808.rds")


# Load Povcalnet country stats
pcn <- readRDS("../subnatpov/PovCalServlet_15.08.06.rds")

# constants
def <- c("circa 2008", "circa 2005")

# Make a list of indicator attributes
varList <- list(
  hc_poor2=list(name="Headcount ratio under $2/day", pal="-RdYlGn", format="0,0.0# %", rev=TRUE),
  hc_poor1=list(name="Headcount ratio under $1.25/day", pal="-RdYlGn", format="0,0.0# %", rev=TRUE),
  povgap_ppp2=list(name="Poverty gap under $2/day", pal="YlOrRd", format="0,0.0# %", rev=TRUE),
  povgap_ppp1=list(name="Poverty gap under $1.25/day", pal="YlOrRd", format="0,0.0# %", rev=TRUE),
  sevpov_ppp2=list(name="Poverty severity under $2/day", pal="YlOrBr", format="0,0.0# %", rev=TRUE),
  sevpov_ppp1=list(name="Poverty severity under $1.25/day", pal="YlOrBr", format="0,0.0# %", rev=TRUE),
  sd_poor2=list(name="HCR Std. Dev. under $2/day", pal="-RdYlGn", format="0,0.0# %", rev=TRUE),
  sd_poor1=list(name="HCR Std. Dev. under $1.25/day", pal="-RdYlGn", format="0,0.0# %", rev=TRUE),
  gini=list(name="Gini coefficient", pal="-Spectral", format="0,0.0# %", rev=TRUE),
  num_poor2=list(name="Poverty headcount below $2/day", pal="Oranges", format="0,#", rev=TRUE),
  num_poor1=list(name="Poverty headcount below $1.25/day", pal="Oranges", format="PPP$ 0,0.0#", rev=TRUE),
  pcexp_ppp_m=list(name="Per capita expenditure, mean per month", pal="RdYlBu", format="PPP$ 0,0.0#", rev=FALSE),
  foodexp_ppp_m=list(name="Food expenditure, mean per month", pal ="RdYlBu", format="PPP$ 0,0.0#", rev=FALSE),
  nfoodexp_ppp_m=list(name="Non-food expenditure, mean per month", pal="RdYlBu", format="0,#", rev=FALSE),
  totpop=list(name="Total population", pal="Purples", rev=FALSE))

vars <- names(varList)
names(vars) <- sapply(varList, `[[`, "name")

# List of countries
load("/home/projects/cell5m/rdb/latest/lookup.RData")
rm(g, readme)
iso <- iso[iso %in% c("SSA", levels(m@data$ISO3)[-c(13,20,33,36,47)])]

# GAUL Country boundaries (water bodies cut out)
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
