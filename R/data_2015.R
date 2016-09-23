#####################################################################################
# Title:   Bioclimatic Estimations
# Date:    Dec. 2014 - Dec. 2015
# Project: HarvestChoice for various
# Author:  Bacou, Melanie <mel@mbacou.com>
#####################################################################################

library(stringr)
library(data.table)
library(reshape2)
library(raster)
library(rgdal)



#####################################################################################
# 2015.03.18 Update: Africa-wide yearly stats for Julia Collins
#####################################################################################

library(maptools)
setwd("~/Projects/hc-data")

load("../hc-cell5m/rdb/g0.rda")
pre <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.pre.dat.nc")
plot(g0)
ssa <- unionSpatialPolygons(g0, g0$CONTINENT)

dt <- extract(pre, ssa, fun=mean, na.rm=T, df=T, small=T)
dt <- data.table(dt)
dt <- melt(dt, id.vars="ID", variable.name="month", variable.factor=F)
dt[, month := as.Date(gsub("X", "", month, fixed=T), "%Y.%m.%d")]

# Yearly mean
dt.year <- dt[, list(
  monthly.mean=mean(value, na.rm=T),
  year.total=sum(value, na.rm=T)), by=year(month)]
write.csv(dt.year, "./JC/CRU.SSA.1901-2013.csv", na="", row.names=F)


#####################################################################################
# 2015.04.09 Generate Climate Stats for Costa Rica
#####################################################################################
# For Eduardo WB

setwd("~/Projects/hc-data")
library(rgdal)
library(raster)
library(data.table)
library(reshape2)

# Helper - summarize raster over districts
genStats <- function(x, var) {
  dt <- extract(get(var), x, fun=mean, na.rm=T, df=T, small=T)
  dt <- cbind(x@data, dt)
  dt <- data.table(dt)
  setnames(dt, 21:dim(dt)[2], format(tm, "%Y-%m-%d"))
  dt <- melt(dt, id.vars=c(names(x), "ID"), variable.name="month", variable.factor=F)
  dt[, month := as.Date(month)]
  # Limit to 1960 onwards
  dt <- dt[month>=as.Date("1960-01-01")]
  return(dt)
}

cri <- readOGR("./out/EM", "district471")

# Process `pre`
pre <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.pre.dat.nc")
tm <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
pre <- setZ(pre, tm, "month")
dt <- genStats(cri, "pre")

cri.pre <- dt[, list(
  pre_mean=mean(value, na.rm=T),
  pre_min=min(value, na.rm=T),
  pre_85=quantile(value, 0.85, na.rm=T),
  pre_max=max(value, na.rm=T),
  pre_sd=sd(value, na.rm=T)), keyby=list(ADM1_CODE, ADM2_CODE, ADM3_CODE)]


# Process `pdsi`
pdsi <- brick("./CRU_TS.3.22/pdsisc.monthly.maps.1850-2012.nc")
tm <- seq(as.Date("1850-01-01"), as.Date("2012-12-31"), "month")
pdsi <- setZ(pdsi, tm, "month")
dt <- genStats(cri, "pdsi")

cri.pdsi <- dt[, list(
  pdsi_mean=mean(value, na.rm=T),
  pdsi_min=min(value, na.rm=T),
  pdsi_85=quantile(value, 0.85, na.rm=T),
  pdsi_max=max(value, na.rm=T),
  pdsi_sd=sd(value, na.rm=T)), keyby=list(ADM1_CODE, ADM2_CODE, ADM3_CODE)]


# Process `tmp`
tmp <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.tmp.dat.nc")
tm <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
tmp <- setZ(tmp, tm, "month")
dt <- genStats(cri, "tmp")

cri.tmp <- dt[, list(
  tmp_mean=mean(value, na.rm=T),
  tmp_min=min(value, na.rm=T),
  tmp_85=quantile(value, 0.85, na.rm=T),
  tmp_max=max(value, na.rm=T),
  tmp_sd=sd(value, na.rm=T)), keyby=list(ADM1_CODE, ADM2_CODE, ADM3_CODE)]

dt <- cri.tmp[cri.pdsi][cri.pre]
cri.dt <- data.table(cri@data)
cri.dt[, rn := row.names(cri)]
setkey(cri.dt, ADM1_CODE, ADM2_CODE, ADM3_CODE)
setkey(dt, ADM1_CODE, ADM2_CODE, ADM3_CODE)
dt <- cri.dt[, .SD, .SDcols=c(1:7,16)][dt]
setkey(dt, rn)
cri@data <- dt[row.names(cri)]
writeOGR(cri, "./out/EM", "cri_adm3_climate", "ESRI Shapefile")

# Let's reprocess but keep all years so we can then generate the bio18 variables
# We need `tmn` an `tmx` instead of `tmp`
tmn <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.tmn.dat.nc")
tm <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
tmn <- setZ(tmn, tm, "month")

tmx <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.tmx.dat.nc")
tm <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
tmx <- setZ(tmx, tm, "month")

cri <- spTransform(cri, proj4string(pre))
pre <- crop(pre, cri)
tmp <- crop(tmp, cri)
tmn <- crop(tmn, cri)
tmx <- crop(tmx, cri)

# Save
save(pre, tmp, tmn, tmx, file="./out/EM/cri_bioclim.RData")


#####################################################################################
# 2015.04.13 Generate Annual Mean and CV for CELL5M
#####################################################################################
# Need to extract and summarize `pre` values over CELL5M SSA grid
library(hcapi3)

grid <- getLayer("ADM0_NAME")
grid <- SpatialPointsDataFrame(grid[, list(X, Y)], data.frame(CELL5M=grid[["CELL5M"]]),
  proj4string=CRS("+init=epsg:4326"))

pre <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.pre.dat.nc")
tm <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
pre <- setZ(pre, tm, "month")

pre_grid <- extract(pre, grid, fun="mean", na.rm=T)
pre_grid <- cbind(grid@data, pre_grid)
pre_grid <- data.table(pre_grid)
setnames(pre_grid, 2:dim(pre_grid)[2], format(tm, "%Y-%m-%d"))
# Limit to 1960 onwards
pre_grid <- pre_grid[, .SD, .SDcols=c(1, 710:1357)]
pre <- melt(pre_grid, id.vars="CELL5M", variable.name="month", variable.factor=F)
pre[, year := as.integer(substr(month, 1, 4))]

# Annual total rainfall
pre <- pre[, list(value=sum(value, na.rm=T)), by=list(CELL5M, year)]
pre[, CELL5M := as.integer(CELL5M)]
saveRDS(pre, "./CRU_TS.3.22/cell5m_pre_cru3.22.rds", compress=T)



#####################################################################################
# 2015.06.07 Update: Add CRU monthly temperatures min/max
#####################################################################################
library(dismo)

rm(list=ls())
load("../hc-shiny/rainfall/tmp/rainfall_2014v15.RData")

# These are needed to generate bioclimatic variables
tm <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
pre <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.pre.dat.nc")
pre <- setZ(pre, tm, "month")
tmn <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.tmn.dat.nc")
tmn <- setZ(tmn, tm, "month")
tmx <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.tmx.dat.nc")
tmx <- setZ(tmx, tm, "month")

# Helper - summarize rasters over districts
genStats <- function(var) {
  dt <- extract(get(var), g2.web, fun=mean, na.rm=T, df=T, small=T)
  dt <- cbind(g2.web@data, dt)
  dt <- data.table(dt)
  setnames(dt, 8:dim(dt)[2], format(tm, "%Y-%m-%d"))
  dt <- melt(dt, id.vars=c(names(g2.web), "ID"), variable.name="month", variable.factor=F)
  dt[, month := as.Date(month)]
  # Limit to 1960 onwards
  dt <- dt[month>=as.Date("1960-01-01")]
  return(dt)
}

dt2.bio <- genStats("tmx")
dt2.tmn <- genStats("tmn")
setnames(dt2.bio, "value", "tmx")

setkey(dt2.bio, ADM0_CODE, ADM1_CODE, ADM2_CODE, month)
setkey(dt2.tmn, ADM0_CODE, ADM1_CODE, ADM2_CODE, month)
setkey(dt2.pre, ADM0_CODE, ADM1_CODE, ADM2_CODE, month)
dt2.bio$tmn <- dt2.tmn[dt2.bio][, value]
dt2.bio$pre <- dt2.pre[dt2.bio][, value]
rm(dt2.tmn, dt2.pre)

# The bio indicators need 12 consecutive months (no season allowed), so let's
# pre-process all the years across districts
dt2.bio[, .N, by=list(ADM0_CODE, ADM1_CODE, ADM2_CODE, year(month))][N!=12]
# Empty data.table (0 rows) of 5 cols: ADM0_CODE,ADM1_CODE,ADM2_CODE,year,N

dt2.bio[, sum(is.na(pre)), by=list(ADM0_CODE, ADM1_CODE, ADM2_CODE, year(month))][V1==12]
#     ADM0_CODE ADM1_CODE ADM2_CODE year V1
#  1:        76      1198     15828 1960 12
#  2:        76      1198     15828 1961 12
#  3:        76      1198     15828 1962 12
#  4:        76      1198     15828 1963 12
#  5:        76      1198     15828 1964 12
#  6:        76      1198     15828 1965 12
# ...

# What country is that?
g2.dt <- data.table(g2.web@data)
g2.dt[ADM0_CODE==76, unique(ADM0_NAME)]
# [1] Equatorial Guinea

bio <- dt2.bio[ADM0_CODE!=76, biovars(pre, tmn, tmx),
  by=list(ADM0_CODE, ADM1_CODE, ADM2_CODE, year(month))]


# Save for re-use
saveRDS(bio, file="./CRU_TS.3.22/ts_yearly_biovars_g2.rds")



#####################################################################################
# 2015.07.28 Update: Process biovars across GLSS6 districts
#####################################################################################

library(dismo)
library(raster)
library(data.table)
library(rgdal)
library(hcapi3)

setwd("~/Projects/hc-data")
load("../hc-shiny/rainfall/tmp/rainfall_2014v15.RData")

gha <- getData(country="GHA", level=2)
# => 137 districts, try GAUL 2008 instead

g2.dt <- data.table(g2.web@data)
g2.dt[, rn := row.names(g2.web)]
g2.dt[ADM0_NAME=="Ghana", length(unique(ADM2_CODE))]
# => 216 districts (since 2008), was 170 in 2008
# Found one with 173 district at GeoCommons
# http://geocommons.com/overlays/201941.zip
download.file("http://geocommons.com/overlays/201941.zip", "./out/DSG/GHA_adm2_170.zip")
unzip("./out/DSG/GHA_adm2_170.zip", exdir="./out/DSG")


# Eduardo provided the GLSS6 district codelist in `./data/GLSS6_codelist.csv`
gha.lbl <- fread("./out/DSG/GLSS6_codelist.csv")
setnames(gha.lbl, c("id", "svyL2Name", "svyL2Code", "svyL1Name"))
# Verify
gha.lbl[, list(.N, length(unique(svyL2Code))), by=svyL1Name]
#        svyL1Name  N V2
# 1:       Western 17 17
# 2:       Central 17 17
# 3: Greater Accra 10 10
# 4:         Volta 18 18
# 5:       Eastern 21 21
# 6:       Ashanti 27 27
# 7:   Brong Ahafo 22 22
# 8:      Northern 20 20
# 9:    Upper East  9  9
# 10:   Upper West  9  9


gha <- readOGR("./out/DSG", "ghana_districts")
gha.dt <- data.table(gha@data)
gha.dt[, rn := row.names(gha)]
dist <- adist(gha.lbl$svyL2Name, gha.dt$DISTRICT, ignore.case=T)
dist <- apply(dist, 1, function(x) order(x)[1:2])
gha.lbl[, match1 := gha.dt[dist[1,], DISTRICT]]
gha.lbl[, match2 := gha.dt[dist[2,], DISTRICT]]

setkey(gha.lbl, match1)
setkey(gha.dt, DISTRICT)
gha.lbl$rn1 <- gha.dt[gha.lbl, mult="first"][, rn]
setkey(gha.dt, match2)
gha.dt$rn2 <- gha.dt[gha.lbl, mult="first"][, rn]

write.csv(gha.lbl, "./out/DSG/GLSS6_district.csv", row.names=F, na="")

# Reload with matching here
gha.lbl <- fread("./out/DSG/GLSS6_district.csv")

# Merge into gha map
gha.lbl[, rn := as.character(rn)]
setkey(gha.dt, rn)
setkey(gha.lbl, rn)
gha.dt <- gha.lbl[gha.dt]
gha.dt[is.na(svyL2Code), DISTRICT]
# [1] NZEMA EAST       BOLE             SAWLA-TUNA-KALBA

setkey(gha.dt, rn)
gha@data <- gha.dt[row.names(gha)]

# Need to remove duplicate feature 154 (island), ignore 171, 172 (slivers)
gha <- gha[-c(155,172,173),]

# Export to QGIS and fix the sliver, and reload
writeOGR(gha, "./out/DSG", "GHA_adm2_170", "ESRI Shapefile", overwrite=T)
gha <- readOGR("./out/DSG", "GHA_adm2_170")
gha.dt <- data.table(gha@data)
gha.dt[, rn := row.names(gha)]


# Intersect with CELL5M variables
# Helper - return dominant class
dominant <- function(x, ...) names(sort(table(x), decreasing=T, ...))[1]

var <- c("AEZ16_CLAS", "LGP_AVG", "LGP_CV", "FS_2012_TX", "TT_20K", "TT_50K",
  "ELEVATION", "PN12_TOT", "pre_mean", "pre_cv", "TPOV_PT200", "TPOV_PT125",
  "soc_d5", "soc_d15", "soc_d30")

var <- getLayer(var, iso3="GHA")
var[, AEZ16_CLAS := factor(AEZ16_CLAS)]
var[, FS_2012_TX := factor(FS_2012_TX)]

var <- SpatialPixelsDataFrame(var[, list(X, Y)], data.frame(var),
  proj4string=CRS("+init=epsg:4326"), tolerance=0.000120005)

r <- brick(var[, c("AEZ16_CLAS", "FS_2012_TX")])
tmp <- extract(r, gha, fun=dominant, na.rm=T, factors=T)
tmp <- data.frame(tmp)
gha.dt <- cbind(gha.dt, tmp)

r <- brick(var[, c("LGP_AVG", "LGP_CV", "TT_20K",
  "TT_50K", "ELEVATION", "pre_mean", "pre_cv", "TPOV_PT200",
  "TPOV_PT125", "soc_d5", "soc_d15", "soc_d30" )])
tmp <- extract(r, gha, fun=mean, na.rm=T)
tmp <- data.frame(tmp)
gha.dt <- cbind(gha.dt, tmp)

r <- raster(var[, "PN12_TOT"])
tmp <- extract(r, gha, fun=sum, na.rm=T)
tmp <- data.frame(PN12_TOT=tmp)
sum(tmp)
# [1] 24904949
gha.dt <- cbind(gha.dt, tmp)

# Clean up a bit
gha.dt <- gha.dt[, .SD, .SDcols=c(4,2,1,3,5,6,13,15,18:33)]
gha <- spTransform(gha, CRS("+init=epsg:3857"))
gha.dt[, area_km := area(gha)/1000000]
summary(gha.dt$area_km)


# Generate biovars from CRU 3.22
# These are needed to generate bioclimatic variables
tm <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
pre <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.pre.dat.nc")
pre <- setZ(pre, tm, "month")
tmn <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.tmn.dat.nc")
tmn <- setZ(tmn, tm, "month")
tmx <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.tmx.dat.nc")
tmx <- setZ(tmx, tm, "month")

gha <- spTransform(gha, proj4string(pre))
pre <- crop(pre, gha)
tmn <- crop(tmn, gha)
tmx <- crop(tmx, gha)

# Keep 1960/01-2013/12
pre <- subset(pre, 709:1356)
tmn <- subset(tmn, 709:1356)
tmx <- subset(tmx, 709:1356)

# Generate biovars year by year
bio <- lapply(seq(1, 648, 12), function(i) biovars(
  subset(pre, i:(i+11)), subset(tmn, i:(i+11)), subset(tmx, i:(i+11))))
length(bio)
# 54

tmp <- lapply(bio, extract, gha, fun=mean, na.rm=T)
bio <- lapply(tmp, as.matrix)
bio <- simplify2array(bio)
dim(bio)
# [1] 170  19  54

dimnames(bio)[[1]] <- gha.dt$svyL2Code
dimnames(bio)[[3]] <- paste0("Y", 1960:2013)
names(dimnames(bio)) <- c("svyL2Code", "biovars", "year")

# Finaly compute long-term mean for each district
bio <- apply(bio, c("svyL2Code", "biovars"), mean, na.rm=T)
bio <- data.frame(bio)
gha.dt <- cbind(gha.dt, bio)
gha.dt[, POPDEN_SKm := NULL]

bio.lbl <- c(
  "Annual Mean Temperature",
  "Mean Diurnal Range (Mean of monthly (max temp - min temp))",
  "Isothermality (BIO2/BIO7) (* 100)",
  "Temperature Seasonality (standard deviation *100)",
  "Max Temperature of Warmest Month",
  "Min Temperature of Coldest Month",
  "Temperature Annual Range (BIO5-BIO6)",
  "Mean Temperature of Wettest Quarter",
  "Mean Temperature of Driest Quarter",
  "Mean Temperature of Warmest Quarter",
  "Mean Temperature of Coldest Quarter",
  "Annual Precipitation",
  "Precipitation of Wettest Month",
  "Precipitation of Driest Month",
  "Precipitation Seasonality (Coefficient of Variation)",
  "Precipitation of Wettest Quarter",
  "Precipitation of Driest Quarter",
  "Precipitation of Warmest Quarter",
  "Precipitation of Coldest Quarter")

gha.dt <- data.frame(gha.dt)
attr(gha.dt, "var.labels") <- c("shape id", "GLSS6 region", "GLSS6 district code", "GLSS6 district",
  "DISTRICT", "CAPITAL", "REGION", vi[names(gha.dt)[8:22]][, varLabel], "area sq. km.", bio.lbl)

# Export to STATA
write.dta(gha.dt, "./out/DSG/gha-glss6-svyMap.dta", version=11L)

# Export to shapefile
setkey(gha.dt, rn)
gha@data <- gha.dt[row.names(gha)]

writeOGR(gha, "./out/DSG", "gha-glss6-svyMap", "ESRI Shapefile")


#####################################################################################
# 2015.08.13 Update: Process biovars across GLSS5 districts
#####################################################################################
# Note: had to run this code on Buster but saved to gha-glss5 survey folder
# Note: Jawoo suggested using a weighted mean by cropland to summarize `soc` layers
# will need to amend the formula in `vi` as well.

library(dismo)
library(raster)
library(data.table)
library(rgdal)
library(hcapi3)  # only available on Buster

setwd("~/Projects/hc-data")

# Load GLSS5 survey map (110 districts)
gha <- readOGR("./out/DSG", "gha-glss5-map_L2")
gha.dt <- data.table(gha@data)
gha.dt[, rn := row.names(gha)]

## Intersect with standard CELL5M biophysical variables
var <- c("AEZ16_CLAS", "LGP_AVG", "LGP_CV", "FS_2012_TX", "cpland_mean_ha",
  "TT_20K", "TT_50K", "TPOV_PT200", "TPOV_PT125",
  "ELEVATION", "PN12_TOT", "pre_mean", "pre_cv",
  "soc_d5", "soc_d15", "soc_d30")

var <- hcapi(var, iso3="GHA")

# Convert to factors (so raster can use them)
var[, AEZ16_CLAS := factor(AEZ16_CLAS)]
var[, FS_2012_TX := factor(FS_2012_TX)]

var <- SpatialPixelsDataFrame(var[, list(X, Y)], data.frame(var),
  proj4string=CRS("+init=epsg:4326"), tolerance=0.000120005)

# Generate dominant class
dominant <- function(x, ...) which.max(table(x))
r <- brick(var[, c("AEZ16_CLAS", "FS_2012_TX")])
tmp <- extract(r, gha, fun=dominant, na.rm=T, factors=T)
tmp <- data.table(tmp)
tmp[, AEZ16_CLAS := levels(r)[[1]]$AEZ16_CLAS[AEZ16_CLAS]]
tmp[, FS_2012_TX := levels(r)[[2]]$FS_2012_TX[FS_2012_TX]]
gha.dt <- cbind(gha.dt, tmp)

# Simple mean
r <- brick(var[, c("LGP_AVG", "LGP_CV", "TT_20K", "TT_50K", "ELEVATION", "pre_mean", "pre_cv")])
tmp <- extract(r, gha, fun=mean, na.rm=T)
tmp <- data.table(tmp)
gha.dt <- cbind(gha.dt, tmp)

# Weighted mean
r <- brick(var[, c("cpland_mean_ha", "soc_d5", "soc_d15", "soc_d30")])
# Construct x*w layers, then sum over polygons
r$soc_d5_w <- r$soc_d5 * r$cpland_mean_ha
r$soc_d15_w <- r$soc_d15 * r$cpland_mean_ha
r$soc_d30_w <- r$soc_d30 * r$cpland_mean_ha

tmp <- extract(r, gha, fun=sum, na.rm=T)
tmp <- data.table(tmp)
tmp[, soc_d5 := soc_d5_w/cpland_mean_ha]
tmp[, soc_d15 := soc_d15_w/cpland_mean_ha]
tmp[, soc_d30 := soc_d30_w/cpland_mean_ha]
gha.dt <- cbind(gha.dt, tmp[, .SD, .SDcols=-c(5:7)])

# weighted mean
r <- brick(var[, c("PN12_TOT", "TPOV_PT200", "TPOV_PT125")])
# Construct x*w layers, then sum over polygons
r$TPOV_PT200_w <- r$TPOV_PT200 * r$PN12_TOT
r$TPOV_PT125_w <- r$TPOV_PT125 * r$PN12_TOT

tmp <- extract(r, gha, fun=sum, na.rm=T)
tmp <- data.table(tmp)
tmp[, TPOV_PT200 := TPOV_PT200_w/PN12_TOT]
tmp[, TPOV_PT125 := TPOV_PT125_w/PN12_TOT]
gha.dt <- cbind(gha.dt, tmp[, .SD, .SDcols=-c(4:5)])


# Add area in sq. km.
gha <- spTransform(gha, CRS("+init=epsg:3857"))
gha.dt[, area_km := area(gha)/1000000]
summary(gha.dt$area_km)


## Generate biovars from CRU 3.22
# These rasters are needed to generate bioclimatic variables (located on server)
tm <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
pre <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.pre.dat.nc")
pre <- setZ(pre, tm, "month")
tmn <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.tmn.dat.nc")
tmn <- setZ(tmn, tm, "month")
tmx <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.tmx.dat.nc")
tmx <- setZ(tmx, tm, "month")
tmp <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.tmp.dat.nc")
tmp <- setZ(tmp, tm, "month")

gha <- spTransform(gha, proj4string(pre))
pre <- crop(pre, gha)
tmn <- crop(tmn, gha)
tmx <- crop(tmx, gha)
tmp <- crop(tmp, gha)

# Keep 1960/01-2013/12 period
pre <- subset(pre, 709:1356)
tmn <- subset(tmn, 709:1356)
tmx <- subset(tmx, 709:1356)
tmp <- subset(tmp, 709:1356)

# Save monthly means across districts
t1 <- extract(pre, gha, mean, na.rm=T)
t1 <- as.data.table(t1)
t1 <- cbind(gha.dt, t1)
t1 <- melt(t1, id.vars=1:9, value.name="pre", variable.name="month")

t2 <- extract(tmp, gha, mean, na.rm=T)
t2 <- as.data.table(t2)
t2 <- cbind(gha.dt, t2)
t2 <- melt(t2, id.vars=1:9, value.name="tmp", variable.name="month")

t3 <- extract(tmn, gha, mean, na.rm=T)
t3 <- as.data.table(t3)
t3 <- cbind(gha.dt, t3)
t3 <- melt(t3, id.vars=1:9, value.name="tmn", variable.name="month")

t4 <- extract(tmx, gha, mean, na.rm=T)
t4 <- as.data.table(t4)
t4 <- cbind(gha.dt, t4)
t4 <- melt(t4, id.vars=1:9, value.name="tmx", variable.name="month")

bio <- cbind(t1, tmp=t2$tmp, tmn=t3$tmn, tmx=t4$tmx)
bio[, month := as.character(month)]
bio[, month := as.Date(month, "X%Y.%m.%d")]


attr(bio, "var.labels") <- c("ISO3 code",
  "Country", "Region", "District",
  "GLSS5 district code", "GLSS5 district", "Capital city", "Region",
  "shape id", "month", "prepitation CRU TS v3.22 (mm)",
  "temperature CRU TS v3.22 (Celsius)", "min. temp. (Celsius)", "max. temp. (Celsius)")

write.dta(bio, "./out/DSG/gha-glss5_L2_bio_month.dta", version=12L)


# Generate biovars year by year
bio <- lapply(seq(1, 648, 12), function(i) biovars(
  subset(pre, i:(i+11)), subset(tmn, i:(i+11)), subset(tmx, i:(i+11))))
length(bio)
# 54 x 19

# Intersect with Ghana districts
tmp <- lapply(bio, extract, gha, fun=mean, na.rm=T)
bio <- lapply(tmp, as.matrix)
bio <- simplify2array(bio)
dim(bio)
# [1] 170  19  54

# Name matrix dimensions
names(dimnames(bio)) <- c("svyL2Code", "biovars", "year")

# Finally compute period mean across districts
bio <- apply(bio, c("svyL2Code", "biovars"), mean, na.rm=T)
bio <- data.table(bio)
gha.dt <- cbind(gha.dt, bio)

# Add labels
bio.lbl <- c(
  "Annual Mean Temperature",
  "Mean Diurnal Range (Mean of monthly (max temp - min temp))",
  "Isothermality (BIO2/BIO7) (* 100)",
  "Temperature Seasonality (standard deviation *100)",
  "Max Temperature of Warmest Month",
  "Min Temperature of Coldest Month",
  "Temperature Annual Range (BIO5-BIO6)",
  "Mean Temperature of Wettest Quarter",
  "Mean Temperature of Driest Quarter",
  "Mean Temperature of Warmest Quarter",
  "Mean Temperature of Coldest Quarter",
  "Annual Precipitation",
  "Precipitation of Wettest Month",
  "Precipitation of Driest Month",
  "Precipitation Seasonality (Coefficient of Variation)",
  "Precipitation of Wettest Quarter",
  "Precipitation of Driest Quarter",
  "Precipitation of Warmest Quarter",
  "Precipitation of Coldest Quarter")

# Export to shapefile
setkey(gha.dt, rn)
gha@data <- data.frame(gha.dt[row.names(gha)])
gha <- spChFIDs(gha, gha$rn)
writeOGR(gha, "./out/DSG", "gha-glss5_L2_bio", "ESRI Shapefile", overwrite=T)

# Export to STATA
attr(gha.dt, "var.labels") <- c("ISO3 code",
  "GLSS5 district code", "GLSS5 district", "Capital city", "Region",
  vi[names(gha.dt)[6:21]][, varLabel],
  "area (sq. km.)", bio.lbl)
write.dta(gha.dt, "./out/DSG/gha-glss5_L2_bio.dta", version=10L)



#####################################################################################
# 2015.07.28 GlobeLand30 land cover classes across GLSS6 survey districts
#####################################################################################

memory.limit(7000)
library(tmap)

gha <- readOGR("./out/DSG", "gha-glss6-svyMap")

# Load forest cover (from GlobeLand30)
lc <- raster("./GLC30/CRLANDUSE/crlu30m")

# Plot it
plot(lc)
tm_shape(lc) +
  tm_raster("crlu30m", palette=palette(),
    legend.hist=T, legend.hist.z=1, title="Land Cover") +
  tm_layout(inner.margins=c(0,0.12,0,0))


# Categorize
lc.lbl <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
names(lc.lbl) <- c(
  "Cultivated land",
  "Forest",
  "Grassland",
  "Shrubland",
  "Wetland",
  "Water body",
  "Tundra",
  "Artificial surface",
  "Bareland",
  "Permanent snow and ice")

lc@data@attributes[[1]]$class <- names(lc.lbl)[-c(7,10)]

# Overlay
dominant <- function(x, ...) which.max(table(x))
lc.ext <- extract(lc, gha, fun=dominant, na.rm=T, factors=T)


######################################################################################
# 2015.09.10 Biovars across SSA countries for Sam Benin
######################################################################################

## Generate biovars from CRU 3.22
# These rasters are needed to generate bioclimatic variables (located on server)
tm <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
pre <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.pre.dat.nc")
pre <- setZ(pre, tm, "month")
tmn <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.tmn.dat.nc")
tmn <- setZ(tmn, tm, "month")
tmx <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.tmx.dat.nc")
tmx <- setZ(tmx, tm, "month")

# Load SSA map and check
g0 <- readRDS("../hc-cell5m/rdb/g0.rds")
plot(g0)
plot(g0[g0$ADM0_NAME=="Ethiopia",])

# Keep only SSA countries
ssa <- fread("../hc-cell5m/rdb/ssa.csv")
g0 <- g0[!is.na(g0$ADM0_NAME),]
g0 <- g0[g0$ADM0_NAME %in% c(ssa$ADM0_NAME, "Côte d'Ivoire"),]
plot(g0)

g0 <- spTransform(g0, proj4string(pre))
pre <- crop(pre, g0)
tmn <- crop(tmn, g0)
tmx <- crop(tmx, g0)

# Keep 1960/01-2013/12 period
pre <- subset(pre, 709:1356)
tmn <- subset(tmn, 709:1356)
tmx <- subset(tmx, 709:1356)

# Generate biovars year by year
bio <- lapply(seq(1, 648, 12), function(i) biovars(
  subset(pre, i:(i+11)), subset(tmn, i:(i+11)), subset(tmx, i:(i+11))))
length(bio)
# 54 x 19

# Intersect with countries
tmp <- lapply(bio, extract, g0, fun=mean, na.rm=T)
bio <- lapply(tmp, as.matrix)
bio <- simplify2array(bio)
dim(bio)
# [1] 48  19  54

# Name matrix dimensions
g0.dt <- data.table(g0@data)
g0.dt[, rn := row.names(g0)]
names(dimnames(bio)) <- c("country", "biovar", "year")
dimnames(bio)$country <- g0.dt$ADM0_CODE
dimnames(bio)$year <- 1960:2013

# Finally convert to flat table
tmp <- as.table(bio)
tmp <- as.data.table(tmp)
setkey(tmp, country)
setkey(g0.dt, ADM0_CODE)
g0.dt[, ADM1_CODE := NULL]
g0.dt[, ADM1_NAME := NULL]
tmp <- g0.dt[tmp]
setcolorder(tmp, c(5:9,1:4,10:12))
tmp[, rn := NULL]
tmp[, STATUS := NULL]
tmp[, STR_YEAR := NULL]
tmp[, EXP_YEAR := NULL]
tmp[, DISP_AREA := NULL]
setnames(tmp, "N", "value")
tmp <- dcast(tmp, ...~biovar, value.var="value")

# Note that biovars were reordered alphabetically!
setcolorder(tmp, c(1:6, 17:24, 7:16))

# Add labels
bio.lbl <- c(
  "Annual Mean Temperature",
  "Mean Diurnal Range (Mean of monthly (max temp - min temp))",
  "Isothermality (BIO2/BIO7) (* 100)",
  "Temperature Seasonality (standard deviation *100)",
  "Max Temperature of Warmest Month",
  "Min Temperature of Coldest Month",
  "Temperature Annual Range (BIO5-BIO6)",
  "Mean Temperature of Wettest Quarter",
  "Mean Temperature of Driest Quarter",
  "Mean Temperature of Warmest Quarter",
  "Mean Temperature of Coldest Quarter",
  "Annual Precipitation",
  "Precipitation of Wettest Month",
  "Precipitation of Driest Month",
  "Precipitation Seasonality (Coefficient of Variation)",
  "Precipitation of Wettest Quarter",
  "Precipitation of Driest Quarter",
  "Precipitation of Warmest Quarter",
  "Precipitation of Coldest Quarter")

# Export to STATA
attr(tmp, "var.labels") <- c("GAUL country code", "GAUL country name",
  "perimeter", "area", "year", bio.lbl)
write.dta(tmp, "./out/SB/ssa_L0_1960-2013_yearly_bio.dta", version=10L)

# Save all
save.image(file="./out/SB/ssa_L0_1960-2013_yearly_bio.RData")



#####################################################################################
# 2015.10.25 Process biovars across district survey maps
#####################################################################################
setwd("~/Projects/hc-data")

# Generate biovars from CRU 3.22
# These are needed to generate bioclimatic variables
tm <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
pre <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.pre.dat.nc")
pre <- setZ(pre, tm, "month")
tmn <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.tmn.dat.nc")
tmn <- setZ(tmn, tm, "month")
tmx <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.tmx.dat.nc")
tmx <- setZ(tmx, tm, "month")

# Keep 1960/01-2013/12
pre <- subset(pre, 709:1356)
tmn <- subset(tmn, 709:1356)
tmx <- subset(tmx, 709:1356)

# Intersect with districts map
l2.map <- readOGR("./2015.10", "svyL2Maps_r1")
l2.map <- spTransform(l2.map, proj4string(pre))
l2.map.dt <- data.table(l2.map@data)

pre <- extract(pre, l2.map, fun=mean, na.rm=T, small=T)
tmn <- extract(tmn, l2.map, fun=mean, na.rm=T, small=T)
tmx <- extract(tmx, l2.map, fun=mean, na.rm=T, small=T)

# Generate biovars year by year
bio <- lapply(seq(1, 648, 12), function(i) biovars(
  pre[, i:(i+11)], tmn[, i:(i+11)], tmx[, i:(i+11)]))
length(bio)
# [1] 54

bio <- simplify2array(bio)
dim(bio)
# [1] 1258   19   54

names(dimnames(bio)) <- c("admin", "biovars", "year")
dimnames(bio)[[1]] <- l2.map$rn
dimnames(bio)[[3]] <- 1960:2013

# Keep the survey year means
bio.svy <- data.table(melt(bio))
bio.svy <- bio.svy[year %between% c(2000,2012) & biovars %in% c("bio1", "bio4", "bio12", "bio15")]
bio.svy <- dcast(admin~biovars+year, data=bio.svy)
bio.svy <- data.table(bio.svy)
bio.svy <- cbind(l2.map@data, bio.svy)
bio.svy <- data.table(bio.svy)
bio.svy[, admin := NULL]

# Finaly compute long-term mean for each admin unit
bio <- apply(bio, c("admin", "biovars"), mean, na.rm=T)
bio <- data.frame(bio)
bio <- cbind(l2.map@data, bio)

# Monthly means
names(dimnames(pre)) <- c("admin", "month")
names(dimnames(tmn)) <- c("admin", "month")
names(dimnames(tmx)) <- c("admin", "month")
pre.mth <- melt(pre)
tmn.mth <- melt(tmn)
tmx.mth <- melt(tmx)
pre.mth <- data.table(pre.mth)
tmn.mth <- data.table(tmn.mth)
tmx.mth <- data.table(tmx.mth)
pre.mth[, var := "pre"]
tmn.mth[, var := "tmn"]
tmx.mth[, var := "tmx"]

bio.mth <- rbind(pre.mth, tmn.mth, tmx.mth)
rm(pre.mth, tmn.mth, tmx.mth)
bio.mth[, mth := month.abb[as.integer(substr(month, 7, 8))]]
bio.mth <- bio.mth[, .(value=mean(value, na.rm=T)), by=.(admin, var, mth)]
bio.mth <- dcast.data.table(admin~var+mth, data=bio.mth)
bio.mth <- cbind(l2.map@data, bio.mth)
bio.mth <- data.table(bio.mth)
bio.mth[, admin := NULL]

# Export
attr <- c("shape id", "ISO3 code", "survey code",
  "adm-1 code (in survey)", "admin-1 name (in survey)",
  "adm-2 code (in survey, 0 is missing or not surveyed)", "admin-2 name (in survey)",
  "print label", "area (sq. km.)", "longitude", "latitude")

bio.lbl <- c(
  "Annual Mean Temperature",
  "Mean Diurnal Range (Mean of monthly (max temp - min temp))",
  "Isothermality (bio2/bio7) (* 100)",
  "Temperature Seasonality (standard deviation *100)",
  "Max Temperature of Warmest Month",
  "Min Temperature of Coldest Month",
  "Temperature Annual Range (bio5-bio6)",
  "Mean Temperature of Wettest Quarter",
  "Mean Temperature of Driest Quarter",
  "Mean Temperature of Warmest Quarter",
  "Mean Temperature of Coldest Quarter",
  "Annual Precipitation",
  "Precipitation of Wettest Month",
  "Precipitation of Driest Month",
  "Precipitation Seasonality (Coefficient of Variation)",
  "Precipitation of Wettest Quarter",
  "Precipitation of Driest Quarter",
  "Precipitation of Warmest Quarter",
  "Precipitation of Coldest Quarter")

bio.svy.lbl <- c(
  paste0(bio.lbl[1], " Y", 2000:2012),
  paste0(bio.lbl[4], " Y", 2000:2012),
  paste0(bio.lbl[12], " Y", 2000:2012),
  paste0(bio.lbl[15], " Y", 2000:2012))

bio.mth.lbl <- c(
  paste0("precipitation", " - ", month.abb),
  paste0("min. temp.", " - ", month.abb),
  paste0("max. temp.", " - ", month.abb))

# Export
attr(bio, "var.labels") <- c(attr, bio.lbl)
attr(bio.svy, "var.labels") <- c(attr, bio.svy.lbl)
attr(bio.mth, "var.labels") <- c(attr, bio.mth.lbl)

write.dta(bio, "./out/2015.10/svyL2Maps-CRU.3.22_1960-2013_r1.dta", version=12L)
write.dta(bio.svy, "./out/2015.10/svyL2Maps-CRU.3.22_2000-2010_year_r1.dta", version=12L)
write.dta(bio.mth, "./out/2015.10/svyL2Maps-CRU.3.22.dta_1960-2013_month_r1.dta", version=12L)


# Save all
save.image(file="./out/2015.10/svyL2Maps.RData")



#####################################################################################
# 2015.10.30 Simplify watershed boundaries for Carleen
#####################################################################################

library(rgdal)
library(rgeos)
library(leaflet)

setwd("~/Projects/hc-data")

m <- readOGR("./HUC", "HUC_Name_clip")
s <- m[1:30,]
s <- gSimplify(m, 0.08, topologyPreserve=T)
s <- SpatialPolygonsDataFrame(s, m@data)

l <- leaflet(m[1:30,]) %>% addTiles() %>% addPolygons()
l <- leaflet(s) %>% addTiles() %>% addPolygons()

writeOGR(s, "./HUC", "HUC_Name_clip_web", "ESRI Shapefile", overwrite_layer=T)


#####################################################################################
# 2015.11.11 Re-run biovars above across district survey maps from 1990-2013
#####################################################################################

library(data.table)
library(reshape2)
library(dismo)
library(foreign)

setwd("~/Projects/hc-data")
load("./out/2015.10/svyL2Maps_2015.10.RData")

# Generate biovars year by year
bio <- lapply(seq(1, 648, 12), function(i) biovars(
  pre[, i:(i+11)], tmn[, i:(i+11)], tmx[, i:(i+11)]))
length(bio)
# [1] 54

bio <- simplify2array(bio)
dim(bio)
# [1] 1258   19   54

names(dimnames(bio)) <- c("admin", "biovars", "year")
dimnames(bio)[[1]] <- l2.map$rn
dimnames(bio)[[3]] <- 1960:2013

# Keep the survey year means
bio.svy <- data.table(melt(bio))
bio.svy <- bio.svy[year %between% c(1990,2013) & biovars %in% c("bio1", "bio4", "bio12", "bio15")]
bio.svy <- dcast(admin~biovars+year, data=bio.svy)
bio.svy <- data.table(bio.svy)
bio.svy <- cbind(l2.map@data, bio.svy)
bio.svy <- data.table(bio.svy)
bio.svy[, admin := NULL]

# Export
bio.svy.lbl <- c(
  paste0(bio.lbl[1], " Y", 1990:2013),
  paste0(bio.lbl[4], " Y", 1990:2013),
  paste0(bio.lbl[12], " Y", 1990:2013),
  paste0(bio.lbl[15], " Y", 1990:2013))

attr(bio.svy, "var.labels") <- c(attr, bio.svy.lbl)
write.dta(bio.svy, "./out/2015.10/svyL2Maps-CRU.3.22_1990-2013_year_r1.dta", version=12L)


# We also need WorldClim biovars at 1km 1990-2000
proj4string(l2.map)
l2.map.xy <- coordinates(l2.map)
dim(l2.map.xy)
dimnames(l2.map.xy)[[2]] <- c("X", "Y")
bio.wc <- apply(l2.map.xy, 1, function(x) getData("worldclim", var="bio", res=0.5, lon=x["X"], lat=x["Y"]))
dim(bio.wc[[1]])
# [1] 3600 3600   19
class(bio.wc[[1]])
# [1] "RasterStack"

bio.wc.l2 <-  lapply(1:length(bio.wc), function(i) extract(bio.wc[[i]], l2.map[i,],
  fun=mean, na.rm=T, small=T))
class(bio.wc.l2[[1]])
# [1] "matrix"
dim(bio.wc.l2[[1]])
# [1]  1 19

bio.wc.l2 <- do.call(rbind, bio.wc.l2)
dim(bio.wc.l2)
bio.wc.l2 <- data.table(bio.wc.l2)
bio.wc.l2 <- cbind(l2.map@data, bio.wc.l2)
setnames(bio.wc.l2, 12:30, paste0("bio", 1:19))

# Export period means
attr(bio.wc.l2, "var.labels") <- c(attr, bio.lbl)
write.dta(bio.wc.l2, "./out/2015.10/svyL2Maps-WorldClim-1km_1950-2000_r1.dta", version=12L)


# Also get 12 monthly files for tmin, tmax, prec over 1950-2000
tmin.wc <- apply(l2.map.xy, 1, function(x) getData("worldclim", var="tmin", res=0.5, lon=x["X"], lat=x["Y"]))
tmax.wc <- apply(l2.map.xy, 1, function(x) getData("worldclim", var="tmax", res=0.5, lon=x["X"], lat=x["Y"]))
tpre.wc <- apply(l2.map.xy, 1, function(x) getData("worldclim", var="prec", res=0.5, lon=x["X"], lat=x["Y"]))

# I assume each tile covers the entire admin unit, but maybe I should combine the tiles
# first into 1 single raster instead? Let's try to map them to check
leaflet(l2.map[1,]) %>%
  addRasterImage(raster(tmin.wc[[1]], layer=1)) %>%
  addPolygons(fillColor="#000")
# => tiles are very large indeed, so code below is fine

tmin.wc.l2 <-  lapply(1:length(bio.wc), function(i) extract(tmin.wc[[i]], l2.map[i,],
  fun=mean, na.rm=T, small=T))
tmax.wc.l2 <-  lapply(1:length(bio.wc), function(i) extract(tmax.wc[[i]], l2.map[i,],
  fun=mean, na.rm=T, small=T))
tpre.wc.l2 <-  lapply(1:length(bio.wc), function(i) extract(tpre.wc[[i]], l2.map[i,],
  fun=mean, na.rm=T, small=T))

# Monthly summaries
# Values are listed across admin units for each 12 months in 1990-2000 period
tmin.wc.l2[[1]]
# tmin1_25 tmin2_25 tmin3_25 tmin4_25 tmin5_25 tmin6_25 tmin7_25 tmin8_25 tmin9_25
# [1,] 202.2451 215.1861 220.9259 221.9773 222.3661 216.6641 213.6172 208.0666 215.1513
# tmin10_25 tmin11_25 tmin12_25
# [1,]  214.5855  216.0756  209.3404
tmin.wc.l2 <- do.call(rbind, tmin.wc.l2)
tmax.wc.l2 <- do.call(rbind, tmax.wc.l2)
tpre.wc.l2 <- do.call(rbind, tpre.wc.l2)

tmin.wc.l2 <- cbind(l2.map@data, tmin.wc.l2)
setnames(tmin.wc.l2, 12:23, paste0("tmin_", substr(month.name, 1, 3)))
setnames(tmax.wc.l2, paste0("tmax_", substr(month.name, 1, 3)))
setnames(tpre.wc.l2, paste0("tpre_", substr(month.name, 1, 3)))

bio.wc.mth <- cbind(tmin.wc.l2, tmax.wc.l2, tpre.wc.l2)

# Export monthly means
bio.mth.lbl <- c(
  paste0("min. temp.", " - ", month.abb, " (degree C*10)"),
  paste0("max. temp.", " - ", month.abb, " (degree C*10)"),
  paste0("precipitation", " - ", month.abb, " (mm)"))

attr(bio.wc.mth, "var.labels") <- c(attr, bio.mth.lbl)
write.dta(bio.wc.mth, "./out/2015.10/svyL2Maps-WorldClim-1km_month_1950-2000_r1.dta", version=12L)


# Sara also needs yearly drought index across districts 1990-2013
pdsi <- brick("./data/pdsisc.monthly.maps.1850-2012.nc")
tm <- seq(as.Date("1850-01-01"), as.Date("2012-12-31"), "month")
pdsi <- setZ(pdsi, tm, "month")
pdsi <- subset(pdsi, 1681:1956)
pdsi.l2 <- extract(pdsi, l2.map, fun=mean, na.rm=T, small=T)
dim(pdsi.l2)
# [1] 1258  276

tm <- seq(as.Date("1990-01-01"), as.Date("2012-12-31"), "month")
names(dimnames(pdsi.l2)) <- c("admin", "month")
dimnames(pdsi.l2)[[1]] <- l2.map$rn
dimnames(pdsi.l2)[[2]] <- tm

# Keep the survey year means
tm <- data.table(mInt=names(pdsi.svy), mStr=tm)
pdsi.svy <- data.table(pdsi.l2)
pdsi.svy[, admin := row.names(pdsi.svy)]
pdsi.svy <- melt(pdsi.svy, id.vars="admin", variable.name="month")
setkey(pdsi.svy, month)
setkey(tm, mInt)
pdsi.svy$m <- tm[pdsi.svy][, as.Date(mStr)]
pdsi.svy <- pdsi.svy[, .(pdsi=mean(value, na.rm=T)), by=.(admin, year(m))]
pdsi.svy <- dcast(admin~year, data=pdsi.svy)
pdsi.svy <- data.table(pdsi.svy)
pdsi.svy <- cbind(l2.map@data, pdsi.svy)
pdsi.svy <- data.table(pdsi.svy)
pdsi.svy[, admin := NULL]
setnames(pdsi.svy, 12:34, paste0("pdsi", 1990:2012))


# Export
pdsi.lbl <- paste0("Index - Y", 1990:2012)
attr(pdsi.svy, "var.labels") <- c(attr, pdsi.lbl)
write.dta(pdsi.svy, "./out/2015.10/svyL2Maps-PDSI-50km_1990-2012_r1.dta", version=12L)


# Check on CRU resolution
tmp <- brick("./CRU_TS.3.22/cru_ts3.22.1901.2013.pre.dat.nc")
tmp <- GDALinfo("./CRU_TS.3.22/cru_ts3.22.1901.2013.pre.dat.nc", returnStats=F)


# Save all
save.image(file="./out/2015.10/svyL2Maps_2015.10.RData")

