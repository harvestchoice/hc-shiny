#####################################################################################
# Title:   HarvestChoice Subnational Poverty
# Date:    August 2015
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

# Load common libraries
library(data.table)
library(foreign)
library(rgdal)


#####################################################################################
# 2016.03.24 Update
#####################################################################################
# Merge in Sara's complete set of 2011 PPPs (all survey years) to publish to /subnatpov/ tool

setwd("~/Projects/hc-shiny/subnatpov")
load("../../hc-cell5m/temp/cell5mDataUpdate.2016.03_svy.RData")

# Latest version of the survey maps is in `svy.map.2008`


# Load Sara's PPP estimates
# 2011 PPP (urban/rural and male/female)
ppp <- read.dta("~/Dropbox (IFPRI)/SDA/Data/analysis/_global_codes/temp/2016.01/poor_ppp_25 Jan 2016.dta")
ppp.rur <- read.dta("~/Dropbox (IFPRI)/SDA/Data/analysis/_global_codes/temp/2016.01/rural_ppp_25 Jan 2016.dta")
ppp.fem <- read.dta("~/Dropbox (IFPRI)/SDA/Data/analysis/_global_codes/temp/2016.01/female_ppp_25 Jan 2016.dta")

ppp <- data.table(ppp)
ppp.rur <- data.table(ppp.rur)
ppp.fem <- data.table(ppp.fem)

# Recode SSN and SWA
levels(svy.map.2011.dt$ISO3)
ppp[ISO3=="SSN", ISO3:="SSD"]
ppp[ISO3=="SWA", ISO3:="SWZ"]


# Try to merge PPPs into all survey maps
setkey(ppp, svyCode, svyL1Cd)
setkey(svy.map.2011.dt, svyCode, svyL1Cd)
ppp <- svy.map.2011.dt[, .SD, .SDcols=-c("year", "ISO3")][ppp]
ppp[is.na(rn), .N, by=.(svyCode, svyL1Cd)]


# Load and merge in the earlier 2005 and 2008 PPPs
