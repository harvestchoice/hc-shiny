#####################################################################################
# Title:   Africa RISING Locations
# Date:    July 2016
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################


#####################################################################################
# 2015.07.28: Process biovars across AR villages
#####################################################################################

library(dismo)
library(raster)
library(data.table)
library(rgdal)
library(hcapi3)

setwd("~/Projects/hc-data")
load("../hc-shiny/rainfall/tmp/rainfall_2014v15.RData")

# Import AR village locations
ar <- readRDS("./out/AR/ARPointsZoI_2015.07.28.rds")


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

# Intersect with AR villages
ar <- spTransform(ar, proj4string(pre))
pre <- extract(pre, ar)
tmn <- extract(tmn, ar)
tmx <- extract(tmx, ar)

# Generate biovars year by year
bio <- lapply(seq(1, 648, 12), function(i) biovars(
  pre[, i:(i+11)], tmn[, i:(i+11)], tmx[, i:(i+11)]))
length(bio)
# 54

bio <- simplify2array(bio)
dim(bio)
# [1] 162  19  54

names(dimnames(bio)) <- c("village", "biovars", "year")

# Finaly compute long-term mean for each district
bio <- apply(bio, c("village", "biovars"), mean, na.rm=T)
bio <- data.frame(bio)
ar@data <- cbind(ar@data, bio)

# Save
saveRDS(ar, "./out/AR/ARPointsZoI_2015.07.28.rds")



#####################################################################################
# 2015.09.10 Process biovars across AR Malawi villages
#####################################################################################
# For Beliyou, note we processed these variables across AR villages back in July.
# Then Beliyou noted differences with Joe's earlier version
# [9/2/2015 3:26:55 PM] HC - Haile, Beliyou: I just did keep if country==3
# in your file and did a summary of elevation, slope, etc. which are all different
# from those of Joe's...
#
#     Variable |        Obs        Mean    Std. Dev.       Min        Max
# -------------+---------------------------------------------------------
#    ELEVATION |         54    674.9259    597.3919        132       2669
# from your file
#
#     Variable |        Obs        Mean    Std. Dev.       Min        Max
# -------------+---------------------------------------------------------
#         elev |      1,134    923.2707    260.5408        514       1259
# from joe's file, for example
#
#
#     Variable |        Obs        Mean    Std. Dev.       Min        Max
# -------------+---------------------------------------------------------
#        slope |         54    1.034032    1.831992   .0672607   10.63578
# from your file
#
#     Variable |        Obs        Mean    Std. Dev.       Min        Max
# -------------+---------------------------------------------------------
#         slop |      1,134    1.108095    .8437256     .10428    3.76778
# from joe's file


library(dismo)
library(raster)
library(data.table)
library(hcapi3)

setwd("~/Projects/hc-data")
load("../hc-shiny/rainfall/tmp/rainfall_2014v15.RData")

# Import AR village locations
ar <- readRDS("./data/ARPointsZoI_2015.07.28.rds")


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

# Intersect with AR villages
ar <- spTransform(ar, proj4string(pre))
pre <- extract(pre, ar)
tmn <- extract(tmn, ar)
tmx <- extract(tmx, ar)

# Generate biovars year by year
bio <- lapply(seq(1, 648, 12), function(i) biovars(
  pre[, i:(i+11)], tmn[, i:(i+11)], tmx[, i:(i+11)]))
length(bio)
# 54

bio <- simplify2array(bio)
dim(bio)
# [1] 162  19  54

names(dimnames(bio)) <- c("village", "biovars", "year")

# Finaly compute long-term mean for each district
bio <- apply(bio, c("village", "biovars"), mean, na.rm=T)
bio <- data.frame(bio)
ar@data <- cbind(ar@data, bio)

# Save
saveRDS(ar, "./out/AR/ARPointsZoI_2015.07.28.rds")



#####################################################################################
# 2015.09.19: Process biovars across AR Malawi households
#####################################################################################

setwd("~/Projects/hc-data")

# Import AR village locations
ar <- readRDS("./out/AR/MWI-HH-Coordinates.rds")


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

# Intersect with AR villages
ar <- spTransform(ar, proj4string(pre))
pre <- extract(pre, ar)
tmn <- extract(tmn, ar)
tmx <- extract(tmx, ar)

# Generate biovars year by year
bio <- lapply(seq(1, 648, 12), function(i) biovars(
  pre[, i:(i+11)], tmn[, i:(i+11)], tmx[, i:(i+11)]))
length(bio)
# 54

bio <- simplify2array(bio)
dim(bio)
# [1] 1131   19   54

names(dimnames(bio)) <- c("hhid", "biovars", "year")

# Finaly compute long-term mean for each district
bio <- apply(bio, c("hhid", "biovars"), mean, na.rm=T)
bio <- data.frame(bio)
ar@data <- cbind(ar@data, bio)

# Save
saveRDS(ar, "./out/AR/MWI-HH-Coordinates_bio.rds")



#####################################################################################
# 2016.07.07: Update, save and map AR locations (Zambia)
#####################################################################################

library(data.table)
library(rgdal)
library(leaflet)
library(raster)

setwd("~/Projects/hc-data/out/AR")

# Load latest AR maps, and Zambia village locations
ar <- readOGR("./2016.06", "af_site_points")
ar <- readRDS("./2016.06/af_site_points.rds")
zmb <- readOGR("./2016.06", "af_site_points_ZMB")

# Clean up field names
names(ar)[c(8,13,15)] <- c("megasite_id", "coordinator", "lastUpdated")
names(zmb)[c(2,11)] <- c("megasite_id", "lastUpdated")
ar$site_desc <- gsub("..", ".", ar$site_desc, fixed=T)
ar$site_desc <- gsub(". 1 villages", ". 1 village", ar$site_desc, fixed=T)

# Load SRTM altitude raster for ZMB
alt <- raster("./ZMB/ZMB_msk_alt.vrt")
projection(alt)
# [1] "+proj=longlat +ellps=WGS84 +no_defs"
plot(alt)

# Load stratification for Zambia
strat.zmb <- raster("./ZMB/w001001.adf")
projection(strat.zmb)
# [1] "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"

# Color palettes
pal.elev <- c(
  "#71abd8",
  "#79b2de",
  "#8dc1ea",
  "#96c9f0",
  "#a1d2f7",
  "#a1d2f7",
  "#acdbfb",
  "#b9e3ff",
  "#d8f2fe",
  "#acd0a5",
  "#94bf8b",
  "#94bf8b",
  "#a8c68f",
  "#bdcc96",
  "#e1e4b5",
  "#efebc0",
  "#e8e1b6",
  "#e8e1b6",
  "#ded6a3",
  "#d3ca9d",
  "#c3a76b",
  "#b9985a",
  "#aa8753",
  "#aa8753",
  "#ac9a7c",
  "#baae9a",
  "#e0ded8",
  "#f5f4f2",
  "#f5f4f2")

bad <- c("Chingoma", "Kafa", "Lutangu", "Maondo", "Munguli", "Mwangano", "Ntaisa", "Simalumba",
  # Others from looking at the map
  "Sinoya", "Chilembwe", "Chivwala", "Chilasa", "Mulungavyela", "Kapayika", "Robert",
  "Chipapika", "Matemanga", "Jali Jali", "Mahemu")

# Save all
save.image("/home/projects/hc-shiny/ar/data/ARPoints.RData")


