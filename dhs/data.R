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
library(rgeos)

setwd("/home/projects/shiny/dhs")
load("./data/dhsMap.2014.10.16.RData")

# Load metadata from Joe Green at
# https://docs.google.com/spreadsheets/d/1v8DDLgi9lQbS4uKnxYPM83PYC90rijJvjKN4xAntKlE/edit#gid=916658631
svy <- fread("./data/dhsSvyList.csv")
iso <- svy[, unique(country_code)]
names(iso) <- svy[, unique(country)]

# Create survey year start/end
setkey(svy, country_code, year)
# Some country codes are missing
svy[1:6, country_code := c("ML", "PE", "HN", "SN", "EK", "PE")]
setkey(svy, country_code, year)
svyYear <- svy[, list(year=paste(year, collapse=",")), by=country_code]
svyYear[, yearStart := sapply(strsplit(year, ",", fixed=T), unlist)[1], by=country_code]
svyYear[, yearEnd := tail(sapply(strsplit(year, ",", fixed=T), unlist), 1), by=country_code]
setnames(svyYear, c("iso", "year", "yearStart", "yearEnd"))

# Create pretty survey list
svy[, svyCode := paste0(country_code, year)]
setnames(svy, c("country", "iso", "year", "phase", "recode", "released", "gps", "recall", "svyCode"))
setcolorder(svy, c(9,2,1,3,4:8))

# Create pretty indicator list
dhs.lbl[, varGroup := sapply(strsplit(varCode, "_", fixed=T), unlist)[1], by=varCode]
dhs.lbl[, varGroup := as.integer(gsub("i", "", varGroup, fixed=T))]
setkey(dhs.lbl, varGroup)
setkey(var, `indicator\ngroup`)
dhs.lbl$varCat <- var[dhs.lbl][, summary]
dhs.lbl[1:8, varCat := "survey design"]
dhs.lbl <- split(dhs.lbl, dhs.lbl$varCat)
tmp <- dhs.lbl
dhs.lbl <- lapply(dhs.lbl, function(x) x$varCode)
for (i in 1:length(dhs.lbl)) names(dhs.lbl[[i]]) <- tmp[[i]]$varLabel


## Simplify admin boundaries
# Load gis.web prepared on Jan 5, that already has simplified boundaries
gis <- readRDS("./data/gis.web.rds")

# Beautify attributes and add X, Y centroids
dt <- data.table(gis@data)
dt[, REGNAME := NULL]
setnames(dt, c("iso", "year", "country", "svyRegName", "svyID", "svyRegID", "svyRegVar",
  "svyRegCode", "svyCode", "rn"))
setcolorder(dt, c(9,5,1,3,2,6,8,4,7,10))
dt[, svyRegVar := NULL]
dt[, X := coordinates(gis)[, 1]]
dt[, Y := coordinates(gis)[, 2]]
gis@data <- dt
rm(dt)

# Save to geoJSON, then re-save into .rds format (`rn` identifies unique polygons)
for (i in unique(gis@data$svyCode)) {
  tmp <- gis[gis$svyCode==i,]
  f <- paste0("./data/json/", i, ".json")
  writeOGR(tmp, f, i, "GeoJSON")
  tmp <- jsonlite::fromJSON(f, simplifyVector=F)
  saveRDS(tmp, paste0("./data/rds/", i, ".json.rds"), compress=T)
}

save.image("./data/dhsMap.2014.10.16.RData")
