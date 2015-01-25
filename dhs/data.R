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


# Load preprocessed GeoJSON resources as list
gis.web <- readRDS("./data/gis.web.rds")
gis <- readRDS("./data/gis.simple.rds")

# Test print::xtable on `dhs`
dt <- dhs[country_code=="GH", .SD,   .SDcols=c("year", "hv025", "hv024", "hv024_name",
  "i316_exclusive_bf0_5_f", "i316_exclusive_bf0_5_m")]
dt[, hv024_name := stringi::stri_trans_totitle(hv024_name)]
dt <- melt(dt, id.vars=c("year", "hv025", "hv024", "hv024_name"))
dt[, variable := factor(variable, labels=c("female", "male"))]
dt <- tabular(Heading("Region")*factor(hv024_name)~Heading()*factor(year)*Heading()*factor(hv025)*Heading()*variable*Heading()*value*Heading()*mean, dt)


rm(tmp, i, f, var, gis.dt, dhs.svy, dt)
save.image("./data/dhsMap.2014.10.16.RData")
