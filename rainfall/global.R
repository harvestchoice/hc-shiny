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
library(leaflet)
library(dygraphs)

# List of layers
vars <- list(
  # CRU 3.22 precipitation time series (from 1901 onwards)
  pre=list(
    name="Precipitation (mm)",
    path="./data/cru_ts3.22.1901.2013.pre.dat.nc",
    tm=seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month"),
    pal=rev(c("#2F6FBF", "#69DB4D", "#F9EF58", "#DC5207", "#830000"))),

  # CRU 3.22 temperatures time series (from 1901 onwards)
  tmp=list(
    name="Temperature (C)",
    path="./data/cru_ts3.22.1901.2013.tmp.dat.nc",
    tm=seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month"),
    pal=c("#801FEF", "#0000FF", "#4169E1", "#1C90FF", "#00BFFF", "#8CCDEF", "#FFFFC8",
      "#FFE131", "#FFAA00", "#FF6E00", "#FF0000", "#C80000", "#FFB1B1")
  ),

  # PDSI (from 1850 onwards)
  pdsi=list(
    name="Palmer Drought Severity Index (-10, 10)",
    path="./data/pdsisc.monthly.maps.1850-2012.nc",
    tm=seq(as.Date("1850-01-01"), as.Date("2012-12-31"), "month"),
    pal=c("#FF9900", "#FFFF66", "#FFFFFF", "#99FF99", "#009900")
  ),

  # ERA Total Precipitation (from 1970 onwards)
  eratp=list(
    name="ERA Synoptic Monthly Mean Precipitation (mm/day)",
    path="./data/era-interim.monthly.pre.water.1979-2014.nc",
    tm=seq(as.Date("1979-01-01"), as.Date("2014-12-01"), "month"),
    pal=c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4",
      "#1D91C0", "#225EA8", "#253494", "#081D58")
  ),

  # Others (not yet processed)
  cld=list(name="Cloud Cover (%)"),
  dtr=list(name="dtr"),
  frs=list(name="frs"),
  pet=list(name="pet"),
  tmn=list(name="tmn"),
  tmx=list(name="tmx"),
  vap=list(name="vap"),
  wet=list(name="wet")
)

# Make named array of layers
d <- names(vars)
names(d) <- sapply(vars, `[[`, "name")

# Load all monthly districts stats (already intersected with g2)
data <- readRDS("./data/dt2.rds")

# Load GAUL 2014 district boundaries (full version)
g2 <- readRDS("./data/g2_2014v15.rds")
g2.dt <- data.table(g2@data)[, .N, by=list(ADM0_CODE, ADM0_NAME)]
setkey(g2.dt, ADM0_NAME)

# Load country/province/district list to populate controls
g2.list <- readRDS("./data/g2_2014v15.list.rds")
