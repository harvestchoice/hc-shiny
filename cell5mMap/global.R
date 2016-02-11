#####################################################################################
# Title:   HarvestChoice Data API with leaflet
# Date:    July 2015
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

library(data.table)
library(hcapi3)
library(leaflet)
library(classInt)
library(rgdal)
library(raster)
library(shinydashboard)
library(shinyjs)
library(curl) # make the jsonlite suggested dependency explicit

# Create layer menu
setorder(vi, cat1, cat2, cat3, sortOrder)
catlst <- vi[genRaster==T & published==T, list(cat2=unique(cat2)), by=cat1]
tmp <- apply(catlst, 1, function(row) menuSubItem(
  row[["cat2"]], icon=icon("angle-right"),
  tabName=gsub(" ", "-", row[["cat2"]], fixed=T)))
catlst <- split(tmp, catlst$cat1)
rm(tmp)
setkey(vi, varCode)
