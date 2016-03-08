#####################################################################################
# Title:   Population Hotspots 2000-2020 - Africa
# Date:    March 2016
# Project: HarvestChoice/IFPRI for PIM
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

library(data.table)
library(tmap)
library(ggvis)
library(foreign)
library(rgdal)
library(leaflet)

load("./tmp/popTrends.RData")
