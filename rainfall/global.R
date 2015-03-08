#####################################################################################
# Title: CRU and PDSI 1960-2013 Time Series across Districts
# Date: December 2014
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

library(shiny)
library(shinyBS)
library(data.table)
library(sp)
library(reshape2)
library(leaflet, lib.loc="/home/mbacou/R/x86_64-redhat-linux-gnu-library/3.1")
library(dygraphs)
library(latticeExtra)

options(shiny.trace=TRUE)

setwd("/home/projects/shiny/tmp")

# CRU and PDSI variables
d <- c("cld", "dtr", "frs", "pet", "pre", "tmn", "tmp", "tmx", "vap", "wet", "pdsi", "eratp")
names(d) <- c("Cloud Cover (%)", "dtr", "frs", "pet", "Precipitation (mm)", "tmn",
  "Temperature (C)", "tmx", "vap", "wet", "Palmer Drought Severity Index (-10, 10)",
  "ERA Synoptic Monthly Mean Precipitation (mm/day)")

# CRU 3.22 precipitation time series (from 1901 onwards)
path.pre <- "../rainfall/data/cru_ts3.22.1901.2013.tmp.dat.nc"
tm.pre <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
col.pre <- rev(c("#2F6FBF", "#69DB4D", "#F9EF58", "#DC5207", "#830000"))

# PDSI (from 1850 onwards)
path.pdsi <- "../rainfall/data/pdsisc.monthly.maps.1850-2012.nc"
tm.pdsi <- seq(as.Date("1850-01-01"), as.Date("2012-12-31"), "month")
col.pdsi <- c("#FF9900", "#FFFF66", "#FFFFFF", "#99FF99", "#009900")

# ERA Total Precipitation (from 1970 onwards)
path.eratp <- "../rainfall/data/era-interim.monthly.pre.water.1979-2014.nc"
tm.eratp <- seq(as.Date("1979-01-01"), as.Date("2014-12-01"), "month")
col.eratp <- c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8", "#253494", "#081D58")

# Load GAUL 2014 district boundaries (full version)
g2 <- readRDS("../../cell5m/rdb/g2_2014v15.rds")
g2.dt <- data.table(g2@data)[, .N, by=list(ADM0_CODE, ADM0_NAME)]
setkey(g2.dt, ADM0_NAME)

# Load country/province/district list to populate controls
g2.list <- readRDS("../../cell5m/rdb/g2_2014v15.list.rds")



