#####################################################################################
# Title:   Process Biovars across HoA Provinces
# Date:    August 2015
# Project: HoA Resilience, USAID
# Author:  Bacou, Melanie <mel@mbacou.com>
#####################################################################################

# Already produced intersection for Kenya at ./temp/ken_cell5m_2015.05.22.dta
# reload here and use the same selection of CELL5M vars plus rainfall and temperature.

# Miro shared the list of admin units on 8/5/2015 at ./in/Administrative regions.xlsx
# Need to make sure we have corresponding maps.

# Also note that we now have updated pov rates for Uganda (2012) from Carlo

library(raster)
library(data.table)
library(rgdal)
library(foreign)
library(hcapi3)  # only available on Buster

setwd("~/Google Drive/2013-HoA")
setwd("/home/projects/hc-data")
load("./tmp/hoa_bio.RData")
save.image("./tmp/hoa_bio.RData")

# Make the required maps 1 by 1

## TZA - 26 regions (GADM is okay)
tza <- getData("GADM", country="tza", level=1)

## BDI - 17 regions (GADM is okay)
bdi <- getData("GADM", country="bdi", level=1)

## SEN - 14 regions (GADM has 11 regions)
# Get it from latest survey maps instead
svy.map <- readRDS("~/Projects/2010-HH/_global_codes/out/r15.07/svyPov_web_20150808.rds")
sen <- svy.map[svy.map$svyCode=="sen2011",]

## RWA - 5 regions (GADM has 10 regions)
# Found at https://www.humanitarianresponse.info/en/operations/rwanda/dataset/rwanda-admin-level-2-boundaries
rwa <- readOGR("../../Maps/admin/rwa_admin2_2006_nisr", "RWA_Admin2_2006_NISR")

## BFA - 45 regions (GADM has 44 regions)
# Found at http://geonode.ocharowca.info/layers/geonode%3Abfa_admbnd2_1m_salb
bfa <- readOGR("../../Projects/2010-HH/BFA-EBCVM-03/maps", "bfa_admbnd2_1m_salb")

## GIN - 8 regions (GADM is okay)
gin <- getData("GADM", country="gin", level=1)

## UGA - 111 districts (GADM has 162 districts, GAUL 2008 has 160, GAUL 2015 has 170)
# Found a post-2010 version at
# https://www.humanitarianresponse.info/operations/uganda/dataset/uganda-admin-level-3-boundaries
uga <- readOGR("../../Projects/2010-HH/UGA-NPS-09/maps", "Uganda_districts2010")

## KEN - 8 provinces
ken <- getData("GADM", country="ken", level=1)

# Harmonize attributes
names(uga)[c(8,6,7)] <- c("NAME_0", "NAME_2", "NAME_1")
names(sen)[c(1,6)] <- c("NAME_0", "NAME_1")
names(rwa)[4] <- "NAME_1"
names(bfa)[c(2,4,6)] <- c("NAME_0", "NAME_1", "NAME_2")

# Combine
iso <- ls()
m <- list(bdi, bfa, gin, ken, rwa, sen, tza, uga)
names(m) <- iso
n <- c("NAME_0", "NAME_1", "NAME_2")
m <- lapply(m, function(x) x[, names(x) %in% n])
for (i in names(m)) m[[i]]$iso3 <- i
for (i in names(m)) names(m[[i]]) <- tolower(names(m[[i]]))
for (i in names(m)) m[[i]] <- spChFIDs(m[[i]], paste0(i, row.names(m[[i]])))
proj4string(m[["rwa"]]) <- CRS("+init=epsg:4326")
m[["rwa"]]$name_0 <- "Rwanda"
for (i in names(m)) m[[i]] <- spTransform(m[[i]], CRS("+init=epsg:3857"))
for (i in names(m)) if ( dim(m[[i]])[2] < 4 ) m[[i]]$name_2 <- NA

m.all <- do.call(rbind, m)

# Load vars for Kenya (note that this file is bad)
old <- read.dta("./temp/ken_cell5m_2015.05.22.dta")
vars <- names(old)[-c(1:2)]
vars <- c(vars, "TPOV_PT125", "TPOV_PT200", "soc_d5", "soc_d15", "soc_d30", "soc_d60")

# Intersect with standard CELL5M biophysical variables
m.all <- spTransform(m.all, CRS("+init=epsg:4326"))

# Intersect with CELL5M
r <- hcapi("CELL5M")
r <- SpatialPixelsDataFrame(r[, list(X, Y)], data.frame(r$CELL5M),
  tolerance=0.00360015, proj4string=CRS("+init=epsg:4326"))
r <- raster(r)

out <- extract(r, m.all, small=T, weights=T)
m.dt <- lapply(out, function(x) hcapi(vars, ids=x[,1], collapse="all"))
m.dt <- rbindlist(m.dt, fill=T)

# TODO
tmp <- getLayer(vars, by="ISO3")
tmp <- getLayer(vars, ids=out[[1]][,1], collapse=T)


# Combine
m.all@data <- cbind(m.all@data, m.dt)


# Add area in sq. km.
gha <- spTransform(gha, CRS("+init=epsg:3857"))
gha.dt[, area_km := area(gha)/1000000]
summary(gha.dt$area_km)



# Export to shapefile
setkey(hoa.dt, rn)
hoa@data <- data.frame(gha.dt[row.names(gha)])
hoa <- spChFIDs(hoa, hoa$rn)
writeOGR(hoa, "./maps", "hoa_bio_20150813", "ESRI Shapefile", overwrite=T)

# Export to STATA
gha.dt <- data.frame(gha.dt)
attr(gha.dt, "var.labels") <- c("ISO3 code", "GLSS5 district code", "GLSS5 district",
  vi[names(gha.dt)[4:19]][, varLabel],
  "area (sq. km.)", bio.lbl)
write.dta(gha.dt, "./temp/gha-glss5_L2_bio.dta", version=10L)


save.image("./temp/hoa_bio.RData")
