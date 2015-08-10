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

# Clear cache
file.remove(list.files())

# Load latest data revision (web minified)
m <- readRDS("../subnatpov/svyPov_web_20150808.rds")

# Note: weirdly leaflet needs shapes at epsg:4326 and tmap needs epsg:3857
# m$rn <- NULL
# m <- spTransform(m, CRS("+init=epsg:4326"))
# names(m)[11:25] <- paste0("total_", names(m)[11:25])
# names(m)[6] <- "adminUnit"
# m@data <- data.frame(m@data)
# saveRDS(m, "../subnatpov/svyPov_web_20150808.rds")

# Load Povcalnet country stats
pcn <- readRDS("../subnatpov/PovCalServlet_15.08.06.rds")

# List of indicators
varList <- list(
  hc_poor2=list(name="Headcount ratio under $2/day", label="HCR $2",
    pal="-RdYlGn", format="0,0.0# %", rev=TRUE),
  hc_poor1=list(name="Headcount ratio under $1.25/day", label="HCR $1.25",
    pal="-RdYlGn", format="0,0.0# %", rev=TRUE),
  povgap_ppp2=list(name="Poverty gap under $2/day", label="Gap $2/d",
    pal="YlOrRd", format="0,0.0# %", rev=TRUE),
  povgap_ppp1=list(name="Poverty gap under $1.25/day", label="Gap $1.25",
    pal="YlOrRd", format="0,0.0# %", rev=TRUE),
  sevpov_ppp2=list(name="Poverty severity under $2/day", label="Sev. $2",
    pal="YlOrBr", format="0,0.0# %", rev=TRUE),
  sevpov_ppp1=list(name="Poverty severity under $1.25/day", label="Sev. $1.25",
    pal="YlOrBr", format="0,0.0# %", rev=TRUE),
  sd_poor2=list(name="HCR Std. Dev. under $2/day", label="HCR Std. $2",
    pal="-RdYlGn", format="0,0.0# %", rev=TRUE),
  sd_poor1=list(name="HCR Std. Dev. under $1.25/day", label="HCR Std. $1.25",
    pal="-RdYlGn", format="0,0.0# %", rev=TRUE),
  gini=list(name="Gini coefficient", label="Gini",
    pal="-Spectral", format="0,0.0# %", rev=TRUE),
  num_poor2=list(name="Poverty headcount below $2/day", label="HC $2",
    pal="Oranges", format="0,#", rev=TRUE),
  num_poor1=list(name="Poverty headcount below $1.25/day", label="HC $1.25",
    pal="Oranges", format="PPP$ 0,0.0#", rev=TRUE),
  pcexp_ppp_m=list(name="Per capita expenditure, mean per month", label="PC Exp.",
    pal="RdYlBu", format="PPP$ 0,0.0#", rev=FALSE),
  foodexp_ppp_m=list(name="Food expenditure, mean per month", label="Food Exp.",
    pal ="RdYlBu", format="PPP$ 0,0.0#", rev=FALSE),
  nfoodexp_ppp_m=list(name="Non-food expenditure, mean per month", label="NFood exp.",
    pal="RdYlBu", format="0,#", rev=FALSE),
  totpop=list(name="Total population", label="Pop.",
    pal="Purples", rev=FALSE))

vars <- names(varList)
names(vars) <- sapply(varList, `[[`, "name")

# List of countries, years
load("/home/projects/cell5m/rdb/latest/lookup.RData")
rm(g, readme)
iso <- iso[iso %in% c("SSA", levels(m@data$ISO3)[-c(13,20,33,36,47)])]
years <- data.table(m@data)[order(-year), list(year=unique(year)), keyby=ISO3]

# Constants
def <- c("circa 2008", "circa 2005")

# GAUL Country boundaries for plotting (water bodies cut out)
g0 <- readRDS("/home/projects/cell5m/rdb/g0.epsg3857.rds")

# Helper - Formatter for rhandsometable
convertNA <- function() htmlwidgets::JS(
  "function(instance, TD, row, col, prop, value, cellProperties) {
  if (value === 'NA' | value === 'NaN') {
  value = '--';
  Handsontable.renderers.TextRenderer(instance, TD, row, col, prop, value, cellProperties);
  } else {
  Handsontable.renderers.NumericRenderer(instance, TD, row, col, prop, value, cellProperties);
  }
  }")

