#####################################################################################
# Title: Visualize DHS Regional Estimates
# Date: Januray 2015
# Project: HarvestChoice for A4NH
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

library(shiny)
library(shinyBS)
library(data.table)
library(reshape2)
library(sp)
library(leaflet, lib.loc="/usr/lib64/R/library")
library(RColorBrewer)

if (.Platform$OS.type=="windows") {
  setwd("~/Projects/hc-shiny/tmp")
} else {
  setwd("/home/projects/shiny/tmp")
}

load("../dhs/data/dhsMap.2014.10.16.RData")


