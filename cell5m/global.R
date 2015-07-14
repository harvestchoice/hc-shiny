#####################################################################################
# Title:   HarvestChoice Data API with leaflet
# Date:    July 2015
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

library(data.table)
library(hcapi3)
library(leaflet, lib.loc="/home/mbacou/R/x86_64-redhat-linux-gnu-library/3.2")
library(classInt)
library(sp)
library(raster)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)

setwd("/home/projects/shiny/tmp")

# Create layer menu
setorder(vi, cat1, cat2, cat3, varLabel)
catlst <- vi[genRaster==T & published==T, list(cat2=unique(cat2)), by=cat1]
tmp <- apply(catlst, 1, function(row) menuSubItem(
  row[["cat2"]], icon=icon("angle-right"),
  tabName=gsub(" ", "-", row[["cat2"]], fixed=T)))
catlst <- split(tmp, catlst$cat1)
rm(tmp)
setkey(vi, varCode)
