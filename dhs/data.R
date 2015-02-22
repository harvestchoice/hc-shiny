#####################################################################################
# Title: Prepare DHS Persistent Datasets
# Date: January 2015
# Project: HarvestChoice for A4NH
# Author: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

# The survey admin boundaries and DHS regional estimates were reconciled in
# /cell5m/R/cell5mDataUpdate.2014.12.R. We just need a few additional steps to prep
# the data for web use.

library(data.table)
library(rgdal)
library(leaflet)

setwd("~/Projects/hc-shiny/dhs")
setwd("/home/projects/shiny/dhs")
load("./data/dhsMap.2014.10.16.RData")

# Load metadata from Joe Green at
# https://docs.google.com/spreadsheets/d/1v8DDLgi9lQbS4uKnxYPM83PYC90rijJvjKN4xAntKlE/edit#gid=916658631
svy <- fread("./data/dhsSvyList.csv")
# Some country codes are missing
svy[1:6, country_code := c("ML", "PE", "HN", "SN", "EK", "PE")]
iso <- svy[, unique(country_code)]
names(iso) <- svy[, unique(country)]
iso <- iso[order(iso)]

# Create pretty survey list (svy)
svy[, svyCode := paste0(country_code, year)]
setnames(svy, c("country", "iso", "year", "phase", "recode", "released", "gps", "recall", "svyCode"))
setcolorder(svy, c(9,2,1,3,4:8))

# Survey years (svyYear)
setkey(svy, iso, year)
svyYear <- svy[, list(year=paste(year, collapse=",")), by=iso]
svyYear[, year := sapply(strsplit(year, ",", fixed=T), unlist)]
svyYear <- split(svyYear$year, svyYear$iso)
svyYear <- lapply(svyYear, unlist)

# Create pretty indicator list (dhs.lbl)
var <- fread("./data/dhsVarGroup.csv")
dhs.lbl <- fread("./data/dhs.lbl.csv")

dhs.lbl[, varGroup := sapply(strsplit(varCode, "_", fixed=T), unlist)[1], by=varCode]
dhs.lbl[, varGroup := as.integer(gsub("i", "", varGroup, fixed=T))]
setkey(dhs.lbl, varGroup)
setkey(var, `indicator\ngroup`)
dhs.lbl$varCat <- var[dhs.lbl][, summary]
dhs.lbl[1:8, varCat := "survey design"]

# Add male/female attribute
dhs.lbl[, gender := substr(varCode, nchar(varCode)-1, nchar(varCode))]
dhs.lbl <- dhs.lbl[gender!="_m"]
dhs.lbl[gender=="_f", varCode := substr(varCode, 1, nchar(varCode)-2)]
dhs.lbl[, gender := gender=="_f"]

# Fix labels by hand
write.csv(dhs.lbl, "./data/dhs.lbl.gender.csv", na="", row.names=F)
dhs.lbl <- fread("./data/dhs.lbl.gender.csv")
setkey(dhs.lbl, varGroup, varLabel)


# Load simplified boundaries and test (simplified and transformed to EPSG:4326 in QGIS)
# The raw boundaries from DHS are in `gis`
gis.web <- readOGR("~/Projects/2010-HH/DHS/maps", "MEASURE_DHS_Regions_2014_proj4326")
proj4string(gis.web)
# [1] "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

dt <- gis.web[gis.web$svyCode=="GH2008", ]
coords <- apply(sp::coordinates(dt), 2, mean, na.rm=T)
plot(dt)

leaflet() %>%
  addTiles("http://{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    attribution=HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>')) %>%
  setView(coords[2], coords[1], 6) %>%
  addPolygons(data=dt, fillColor=topo.colors(10, alpha=.2)[dt@data$regCode], stroke=T)


# Test print::xtable on `dhs`
dt <- dhs[country_code=="GH", .SD,   .SDcols=c("year", "hv025", "hv024", "hv024_name",
  "i316_exclusive_bf0_5_f", "i316_exclusive_bf0_5_m")]
dt[, hv024_name := stringi::stri_trans_totitle(hv024_name)]
dt <- melt(dt, id.vars=c("year", "hv025", "hv024", "hv024_name"))
dt[, variable := factor(variable, labels=c("female", "male"))]
dt <- tabular(Heading("Region")*factor(hv024_name)~Heading()*factor(year)*Heading()*factor(hv025)*Heading()*variable*Heading()*value*Heading()*mean, dt)


# Merge in region names from `gis` into `dhs` by survey
tmp <- lapply(gis, function(x) x@data[, c("svyCode", "regCode", "regName")])
tmp <- do.call(rbind, tmp)
tmp <- data.table(tmp)
setkey(tmp, svyCode, regCode)
setkey(dhs, svyCode, hv024)
dhs$regName <- tmp[dhs][, regName]
dhs[is.na(regName), .N, by=svyCode]
#    svyCode  N
# 1:  BO2003  4
# 2:  EG2000  2
# 3:  EG2005  2
# 4:  EG2008  2
# 5:  HN2011  4
# 6:  HT2005  2
# 7:  HT2012  4
# 8:  ID2002 51
# 9:  KH2000 11
# 10:  PE2003  3
# 11:  PE2009  3
# 12:  PE2010  3
# 13:  PE2011  3
# 14:  SN2012 20
# 15:  TD2004  1
# 16:  TZ1999 39


rm(tmp, i, f, var, gis.dt, dhs.svy, dt, coords)
save.image("./data/dhsMap.2014.10.16.RData")


#####################################################################################
# 2015.01.31 Update: Improved DHS boundaries
#####################################################################################

rm(list=ls())
setwd("/home/projects/shiny")
load("./data/dhsMap.2014.10.16.RData")

# Simplified boundaries using http://mapshaper.org/, load here
gis.web <- readOGR("./data", "MEASURE_DHS_Regions_2014")
proj4string(gis.web) <- CRS("+init=epsg:4326")
# [1] "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"





