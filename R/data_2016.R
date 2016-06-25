#####################################################################################
# Title:   Survey Maps for Poverty Land Paper
# Date:    May 2016
# Project: SSA Household Analysis for Sara/Carlo
# Author:  Bacou, Melanie <mel@mbacou.com>
#####################################################################################

# Sara: As I mentioned today during the PDU meeting we finally decided to divide the
# Sub-Saharan Africa Poverty paper into two separate ones in order to better assess
# the climate change impact at the district level and to be able to add some additional
# datasets. We have updated the list of biophysical and geospatial variables needed
# (see attachment). Especially for rainfall and temperature it would be great to have
# monthly level data to better construct our measure of shock. In addition, a few
# countries have been added and a few datasets replaced (see details below).
# The new list of admin level 1 and 2 is in attachment!
#
# New countries:
# -          Angola IBEP 2008
# -          Congo 1-2-3 2012
# -          South Africa IES 2011
# Countries with new datasets:
# -          Mali EMOP 2014
# -          Uganda NPS 2012
# -          Burkina Faso EMC 2014
# -          Ethiopia HCES 2010
#
# Climate and weather data:
# -	Long term averages of rainfall and temperature -> need monthly data from 1950-2014
# (if possible to go back to 1950, otherwise to the earliest date possible)
# -	Annual values of NDVI (Joe) and PDSI from 2000 to 2014
# Agriculture potential:
# -	Altitude
# -	Steepness
# -	Soil quality (soil carbon content, % with low soil nutrient)
# -	Irrigated and total cropland area (from SPAM)
# -	Total TLU (from SPAM)
# Access to markets and infrastructure:
# -	Distance to 20K / 50K centers
# -	Road density
# -	Nightlight radiation (Joe)
# Demography and human capital:
# -	Population density
# Other:
# -	Malaria incidence (Bhatt et al. 2015)
# -	Conflict?
#
# For NCAR SPEI and NDVI I would need yearly values for 2000-2014 because I will use
# them to construct the year-specific shocks (so when you describe the indicators below:
# number of months non-drought, etc.. I would need those for each year 2000-2014)
#
# Similarly, for rainfall and temperature, I was looking for the monthly data for each
# month between 1950-2014. I know it’s a lot of variables but with that I can calculate
# any measure of long term averages and year specific shocks relative to the growing
# season within each country. And maybe this is also simpler for you since you do not
# need to compute any statistic but you just need to give me the value of rainfall and
# temperature for the months over the period.
#
# Mel:
# - monthly temp and rainfall is from 1960-2014 for CRU 3.21 at 5 degree resolution
# and from 1950-2000 for WorldClim at 1km resolution. I can summarize both sources and
# let you decide which one to include in your model.
# - PDSI is an odd one to summarize across years. It's a wetness/dryness index,
# averaging will mask out possible extreme conditions that most affect productivity.
# Might be better to count the occurrences of wetness/dryness anomalies in each year?
# - I'll ask Joe to produce NDVI and nightlight reflectance once I have the survey
# maps ready
# - Aside from the 3 new countries and 4 updates, have any other survey admin units
# changed for the other countries? => no
#
# 2016.05.24 update: Sara added district names and corrected BFA.
# 2016.05.25 update: Sara made ETH woreda codes unique
#
# All survey maps were generated on laptop, saved to Dropbox at
# /SDA/analysis/_global_codes/r16.05/svyMaps_2016.06.22_sara.shp
# and also shared with JG.
#
# Malaria incidence downloaded into ./MAP
# Conflict data downloaded into ./ACLED
# CRU_TS.3.23 downloaded into ./CRU_TS.3.23
# SPEI downloaded into ./PDSI
# GPCC V7 monthly climate downloaded into ./GPCC

library(data.table)
library(reshape2)
library(raster)
library(rgdal)
library(tmap)
library(stringr)

setwd("~/Projects/hc-data")
load("./out/r16.05/svyL2Maps_r16.05.RData")

# Load Sara's list of survey admin units
dist <- fread("./out/r16.05/District codes and names.csv")

# Load survey maps
l2 <- readOGR("./out/r16.05", "svyMaps_2016.06.22_sara")
l2.dt <- data.table(l2@data)
l2.dt[, rn := row.names(l2)]

# Verify that all units are covered and note possible recodes
dist[, svyCode := tolower(paste0(ISO3, year))]
dist[svyCode=="eth2010", svyL2Cd := svyL1Cd*10000L+svyL2Cd]
setkey(dist, svyCode, svyL1Cd, svyL2Cd)
setkey(l2.dt, svyCode, svyL1Cd, svyL2Cd)
dist[!l2.dt]

setkey(dist, svyCode, svyL1Cd, svyL2Cd)
setkey(l2.dt, svyCode, svyL1Cd, svyL2Cd)
dist[!l2.dt]
# => differ for `bfa2014`


###############################################
## CELL5M vars
#
# - cpland_mean_ha - sum
# - GMIA_V5 - sum
# - AN05_TLU - sum
# - tt10 - median
# - AD05_TLU - AN05_TLU weighted mean
# - soc - cpland weighted mean
load("./CELL5M/cell5m.rda")
vars <- c("AEZ16_CLAS", "FS_2012_TX", "LGP_AVG", "LGP_CV",
  "cpland_mean_ha", "GMIA_V5", "AN05_TLU", "AD05_TLU", "tt10_20k", "tt10_50k",
  "soc_d5", "soc_d15", "soc_d30", "soc_d60", "X", "Y", "ELEVATION", "SLOPE")

# Convert to factors (so raster can use them)
dt[, AEZ16_CLAS := factor(AEZ16_CLAS)]
dt[, FS_2012_TX := factor(FS_2012_TX)]

r <- SpatialPixelsDataFrame(dt[, list(X, Y)], data.frame(dt[, .SD, .SDcols=vars]),
  proj4string=CRS("+init=epsg:4326"))

# dominant class
rr <- brick(r[, c("AEZ16_CLAS", "FS_2012_TX")])
tmp <- extract(rr, l2, fun=modal, na.rm=T, factors=T, small=T)
tmp <- data.table(tmp)
tmp[, AEZ16_CLAS := levels(rr)[[1]]$AEZ16_CLAS[AEZ16_CLAS]]
tmp[, FS_2012_TX := levels(rr)[[2]]$FS_2012_TX[FS_2012_TX]]
tmp[, table(AEZ16_CLAS)]
tmp[, table(FS_2012_TX)]
l2.dt <- data.table(cbind(l2@data, tmp))

# sum
rr <- brick(r[, c("cpland_mean_ha", "GMIA_V5", "AN05_TLU")])
tmp <- extract(rr, l2, fun=sum, na.rm=T, small=T)
tmp <- data.table(tmp)
l2.dt <- cbind(l2.dt, tmp)

# median
rr <- brick(r[, c("tt10_20k", "tt10_50k")])
tmp <- extract(rr, l2, fun=median, na.rm=T, small=T)
tmp <- data.table(tmp)
l2.dt <- cbind(l2.dt, tmp)

# mean
rr <- brick(r[, c("LGP_AVG", "LGP_CV", "ELEVATION", "SLOPE")])
tmp <- extract(rr, l2, fun=mean, na.rm=T, small=T)
tmp <- data.table(tmp)
l2.dt <- cbind(l2.dt, tmp)

# weighted mean
rr <- brick(r[, c("cpland_mean_ha", "soc_d5", "soc_d15", "soc_d30", "soc_d60")])
# Construct x*w layers, then sum over polygons
rr$soc_d5_w <- rr$soc_d5 * rr$cpland_mean_ha
rr$soc_d15_w <- rr$soc_d15 * rr$cpland_mean_ha
rr$soc_d30_w <- rr$soc_d30 * rr$cpland_mean_ha
rr$soc_d60_w <- rr$soc_d60 * rr$cpland_mean_ha

tmp <- extract(rr, l2, fun=sum, na.rm=T, small=T)
tmp <- data.table(tmp)
tmp[, soc_d5 := soc_d5_w/cpland_mean_ha]
tmp[, soc_d15 := soc_d15_w/cpland_mean_ha]
tmp[, soc_d30 := soc_d30_w/cpland_mean_ha]
tmp[, soc_d60 := soc_d60_w/cpland_mean_ha]
l2.dt <- cbind(l2.dt, tmp[, .SD, .SDcols=c(2:5)])

# Export to STATA
l2.lbl <- c("shape id", "ISO3 code", "survey code",
  "adm-1 code (in survey)", "admin-1 name (in survey)",
  "adm-2 code (in survey, 0 is missing or not surveyed)", "admin-2 name (in survey)",
  "print label", "area (sq. km.)", "longitude (degree)", "latitude (degree)")

l2.lbl.cell5m <- c(
  "dominant agro-ecological zone (class)",
  "dominant farming system (class)",
  "cropland area (ha, sum)", "irrigated area (ha, sum)", "tropical livestock units (TLU 2005, sum)",
  "time to 20k market (hrs, median)", "time to 50k market (hrs, median)",
  "growing period (days, mean)", "growing period (C.V., mean)",
  "elevation - cell5m (meter, mean)", "slope - cell5m (degree, mean)",
  "soil organic carbon content at 0-5 cm (permilles, weighted mean)",
  "soil organic carbon content at 5-15 cm (permilles, weighted mean)",
  "soil organic carbon content at 15-30 cm (permilles, weighted mean)",
  "soil organic carbon content at 30-60 cm (permilles, weighted mean)")

l2.cell5m <- l2.dt
setkey(l2.cell5m, svyCode, svyL1Cd, svyL2Cd)
attr(l2.cell5m, "var.labels") <- c(l2.lbl, l2.lbl.cell5m)
write.dta(l2.cell5m, "./out/r16.05/svyL2Maps-CELL5M.dta", convert.factors="string", version=12L)



###############################################
## PDSI/SPEI
#
# Download latest revision of NCAR and Princeton PDSI
# Update on Feb. 24, 2016: The 1850-2014 scPDSIpm data file was revised with slight
# changes from the previous version of Nov.2, 2015. Users are advised to update their
# applications with this updated version.
pdsi <- brick("./PDSI/pdsisc.monthly.maps.1850-2014.nc")
pdsi <- brick(pdsi)

# Verify
names(pdsi)[1:24]
tail(names(pdsi), 24)
res(pdsi)
# [1] 2.5 2.5 => 250km, Princeton data is downscaled to 1 degree.
# => assume Jan 1950 through Dec 2014
# Also note that there is plenty of missing data/years for Africa
tm_shape(pdsi[[1980]], bbox=bbox(l2)) + tm_raster()


# The SPEI might be a more usable measure for agricultural applications. The SPEI is
# designed to take into account both precipitation and potential evapotranspiration
# (PET) in determining drought. A gridded SPEI data set is available for 1901-2011
# based on CRU TS3.2 input data and the Penman-Monteith method. A real-time SPEI is
# published for global drought monitoring based on the Thornnthwaite method. Finally,
# an R package is available for calculating the SPEI from user-selected input data
# using either the Thornthwaite, Penman-Monteith, or Hargreaves methods.
# See https://cran.r-project.org/web/packages/SPEI/
# Need to compute PET using hargreaves(Tmin, Tmax, Ra = NA, lat = NA, Pre = NA, na.rm = F)
# and with climatic water balance (precipitation minus potential evapotranspiration)
# compute e.g. spei(wichita$PRCP-wichita$PET, 1)
#
# This is already done in SPEIbase v.2.4 [Dataset], 2016
# at http://digital.csic.es/handle/10261/128892
# There are comparisons of indicators for Africa e.g. in
# https://www.researchgate.net/publication/258519549_Comparison_of_drought_indicators_derived_from_multiple_data_sets_over_Africa
# and definitions of drought events and characteristics at e.g.
# http://www.mdpi.com/2073-4433/6/10/1399/pdf
#
# Drought classification based on the SPI and SPEI:
# - Non-drought 0 ≤ Index
# - Mild drought -1.0 < Index < 0
# - Moderate drought -1.5 < Index ≤ -1.0
# - Severe drought -2.0 < Index ≤ -1.5
# - Extreme drought Index ≤ -2.0
#
# A drought event is defined as a period in which the SPI is continuously negative and
# the SPI reaches a value of −1.0 or less according to McKee et al. [5]. Drought starts
# when the SPI first falls below zero and ends with the positive SPI value following
# a value of −1.0 or less [5]. Once a drought event with a start and end month was
# determined, drought-related indicators including duration, severity, and intensity
# were then assigned.

rm(pdsi)
spei <- brick("./PDSI/spei03.nc")
names(spei)[1:24]
tail(names(spei), 24)
tm_shape(spei[[500]], bbox=bbox(l2)) + tm_raster(n=24)
res(spei)
# [1] 0.5 0.5 => 50km
# assume Jan 1901 - Dec 2014
# simply count the months with 5 drought levels, also compute simple monthly mean

tm <- seq(as.Date("1901-01-01"), as.Date("2014-12-31"), "month")
spei <- setZ(spei, tm, "month")

# Keep 1950/01-2014/12
spei <- subset(spei, 589:1368)

# Intersect with survey district maps (mean)
# Note that this needs to be parallelized, very slow
# Use 8 cores to speed up `extract()`
# Note from `raster` doco though, "the use of the cluster is automatic in these
# functions: projectRaster, resample and in extract when using polygons." so maybe
# this is not necessary or makes no difference?
identical(proj4string(spei), proj4string(l2))
l2 <- spTransform(l2, proj4string(spei))

# Note that `crop()` is really needed to speed up `extract()`
spei <- crop(spei, l2)
tmp <- extract(spei, l2, fun=mean, na.rm=T, small=T)
dim(tmp)
# [1] 2078 780
names(dimnames(tmp)) <- c("rn", "month")
dimnames(tmp)[[1]] <- row.names(l2)
dimnames(tmp)[[2]] <- seq(as.Date("1950-01-01"), as.Date("2014-12-31"), "month")


# Monthly and yearly summaries
l2.dt <- data.table(l2@data)
l2.dt[, rn := row.names(l2)]
tmp <- as.data.table(tmp)
setnames(tmp, as.character(seq(as.Date("1950-01-01"), as.Date("2014-12-31"), "month")))
tmp[, rn := row.names(l2)]
tmp <- melt(tmp, id.vars="rn", variable.name="month", value.name="spei")
tmp[, .N, by=rn]
setkey(tmp, rn)
setkey(l2.dt, rn)

l2.spei <- l2.dt[tmp]
l2.spei[, month := as.Date(month)]

l2.spei.year <- l2.spei[, .(spei=mean(spei, na.rm=T)), by=.(rn,  year(month))]
l2.spei.year <- dcast(l2.spei.year, rn~year)
l2.spei.year <- data.table(l2.spei.year)
setkey(l2.spei.year, rn)
setkey(l2.dt, rn)
l2.spei.year <- l2.dt[l2.spei.year]

# Export
spei.lbl <- c(l2.lbl, "month", "mean SPEI index (based in CRU_TS 3.23)")
attr(l2.spei, "var.labels") <- spei.lbl
setkey(l2.spei, svyCode, svyL1Cd, svyL2Cd)
write.dta(l2.spei, "./out/r16.05/svyL2Maps-SPEI_monthly_1950-2014.dta",
  convert.factors="string", version=12L)

spei.lbl <- c(l2.lbl, paste0("mean SPEI index (based in CRU_TS 3.23) in Y", 1950:2014))
attr(l2.spei.year, "var.labels") <- spei.lbl
setkey(l2.spei.year, svyCode, svyL1Cd, svyL2Cd)
write.dta(l2.spei.year, "./out/r16.05/svyL2Maps-SPEI_yearly_1950-2014.dta",
  convert.factors="string", version=12L)

## Table all drought events
# - Non-drought 0 ≤ Index
# - Mild drought -1.0 < Index < 0
# - Moderate drought -1.5 < Index ≤ -1.0
# - Severe drought -2.0 < Index ≤ -1.5
# - Extreme drought Index ≤ -2.0

l2.spei.drought <- l2.spei[, .(
  non_drought = sum(spei >= 0, na.rm=T),
  mild_drought = sum(spei > -1 & spei < 0, na.rm=T),
  mod_drought = sum(spei > -1.5 & spei <= -1, na.rm=T),
  severe_drought = sum(spei > -2 & spei <= -1.5, na.rm=T),
  extreme_drought = sum(spei <= -2, na.rm=T)),
  by=.(rn, ISO3, svyCode, svyL1Cd, svyL1Nm, svyL2Cd, svyL2Nm)]

spei.lbl <- c(l2.lbl[1:7],
  "normal month",
  "months with mild drough",
  "months with moderate drough",
  "months with severe drough",
  "months with extreme drough")
attr(l2.spei.drought, "var.labels") <- spei.lbl
setkey(l2.spei.drought, svyCode, svyL1Cd, svyL2Cd)
write.dta(l2.spei.drought, "./out/r16.05/svyL2Maps-SPEI_drought_1950-2014.dta",
  convert.factors="string", version=12L)


# Check for missing values
l2.spei.year[is.na(`2014`), .SD, .SDcols=1:7]
#      rn ISO3 svyCode svyL1Cd  svyL1Nm svyL2Cd                svyL2Nm
# 1:   31  GHA gha2012       1  Western       4            Ahanta West
# 2:  294  MOZ moz2008       0       NA       0                    IBO
# 3:  213  TZA tza2012       8 Zanzibar      51       KASKAZINI UNGUJA
# 4:  231  TZA tza2012       8 Zanzibar      52          KUSINI UNGUJA
# 5:  232  TZA tza2012       8 Zanzibar      53 MJINI/MAGHARIBI UNGUJA
# 6:  212  TZA tza2012       8 Zanzibar      54        KASKAZINI PEMBA
# 7:  216  TZA tza2012       8 Zanzibar      55           KUSINI PEMBA
# 8: 1331  UGA uga2013       1  Central     101              KALANGALA

tm_shape(l2[l2$svyCode=="gha2012",], is.master=T) + tm_fill("svyL2Nm") +
  tm_shape(spei) + tm_raster(alpha=0.5) +
  tm_shape(l2[l2$rn==31,]) + tm_fill("red")

tm_shape(l2[l2$svyCode=="tza2012",], is.master=T) + tm_fill("svyL2Nm") +
  tm_shape(spei) + tm_raster(alpha=0.5) +
  tm_shape(l2[l2$rn %in% c(213, 231, 232, 212, 216),]) + tm_fill("red")

tm_shape(l2[l2$svyCode=="uga2013",], is.master=T) + tm_fill("svyL2Nm") +
  tm_shape(spei) + tm_raster(alpha=0.5) +
  tm_shape(l2[l2$rn==1331,]) + tm_fill("red")


###############################################
## SRTM
#
# Generate altitude and slope (update survey maps)

iso3 <- unique(as.character(l2$ISO3))
iso3[17] <- "SDN"
alt <- lapply(iso3, function(x) getData("alt", country=x, mask=F, path="./SRTM"))
tm_shape(alt[[22]][[1]]) + tm_raster()
tm_shape(alt[[22]][[2]]) + tm_raster()
# Remove raster for ZAF islands
alt[[22]] <- alt[[22]][[1]]
iso3 <- unique(as.character(l2$ISO3))
names(alt) <- iso3

alt.dt <- lapply(iso3, function(x) {
  tmp.med <- extract(alt[[x]], l2[l2$ISO3==x,], fun=median, na.rm=T, small=T)
  tmp <- extract(alt[[x]], l2[l2$ISO3==x,], fun=mean, na.rm=T, small=T)
  tmp <- data.table(alt_med=tmp.med, alt_mean=tmp)
  tmp[, rn := row.names(l2[l2$ISO3==x,])]
  })
alt.dt <- rbindlist(alt.dt)


slope <- lapply(alt, function(x) terrain(x, opt=c("slope"), unit="degrees"))
names(slope) <- iso3

slope.dt <- lapply(iso3, function(x) {
  tmp.med <- extract(slope[[x]], l2[l2$ISO3==x,], fun=median, na.rm=T, small=T)
  tmp.min <- extract(slope[[x]], l2[l2$ISO3==x,], fun=min, na.rm=T, small=T)
  tmp.max <- extract(slope[[x]], l2[l2$ISO3==x,], fun=max, na.rm=T, small=T)
  tmp <- data.table(slope_med=tmp.med, slope_min=tmp.min, slope_max=tmp.max)
  tmp[, rn := row.names(l2[l2$ISO3==x,])]
  })
slope.dt <- rbindlist(slope.dt)
setnames(slope.dt, c("slope_med", "slope_min", "slope_max", "rn"))

l2.dt <- data.table(l2@data)
l2.dt[, rn := row.names(l2)]
setkey(l2.dt, rn)
setkey(alt.dt, rn)
setkey(slope.dt, rn)
l2.srtm <- alt.dt[slope.dt][l2.dt]
setcolorder(l2.srtm, c(2,7:16,1,3:6))

# Export
srtm.lbl <- c(l2.lbl,
  "altitude (meter, median)", "altitude (meter, mean)",
  "slope (degree, median)", "slope (degree, min)", "slope (degree, max)")
attr(l2.srtm, "var.labels") <- srtm.lbl
setkey(l2.srtm, svyCode, svyL1Cd, svyL2Cd)
write.dta(l2.srtm, "./out/r16.05/svyL2Maps-SRTM.dta", convert.factors="string", version=12L)



###############################################
## CRU_TS 3.23
#
# Download most recent version (requires authentication), BADC FTP site doesn't work
# http://browse.ceda.ac.uk/browse/badc/cru/data/cru_ts/cru_ts_3.23/data

pre <- brick("./CRU_TS.3.23/pre/cru_ts3.23.1901.1910.pre.dat.nc.gz")


# Resolution
# Note that NCER re-analysis and CMAP are also at 2.5 degree, GPCC is also at 30-min
# similar to WorldClim.
# http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis.surface.html
# The NOAA-CIRES Twentieth Century Reanalysis (V2c) is at 2 degree



###############################################
## WorldClim
#
# Also get 12 monthly files for tmin, tmax, prec over 1950-2000
tmin.wc <- apply(l2, 1, function(x) getData("worldclim", var="tmin", res=0.5, lon=x["X"], lat=x["Y"], path="./WorldClim_0.5"))
tmax.wc <- apply(l2, 1, function(x) getData("worldclim", var="tmax", res=0.5, lon=x["X"], lat=x["Y"], path="./WorldClim_0.5"))
tpre.wc <- apply(l2, 1, function(x) getData("worldclim", var="prec", res=0.5, lon=x["X"], lat=x["Y"], path="./WorldClim_0.5"))

# I assume each tile covers the entire admin unit, but maybe I should combine the tiles
# first into 1 single raster instead? Let's try to map them to check
tm_shape(l2[1,]) + tm_fill("#000") +
  tm_shape(raster(tmin.wc[[1]], layer=1)) %>% tm_raster()
# => tiles are very large indeed, so code below is fine

tmin.wc.l2 <-  lapply(1:length(bio.wc), function(i) extract(tmin.wc[[i]], l2[i,],
  fun=mean, na.rm=T, small=T))
tmax.wc.l2 <-  lapply(1:length(bio.wc), function(i) extract(tmax.wc[[i]], l2[i,],
  fun=mean, na.rm=T, small=T))
tpre.wc.l2 <-  lapply(1:length(bio.wc), function(i) extract(tpre.wc[[i]], l2[i,],
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
write.dta(bio.wc.mth, "./out/r16.05/svyL2Maps-WorldClim-1km_month_1950-2000.dta", version=12L)


###############################################
## LandScan 2012
#
# Might also need rural population density (should apply GPWv4 land/water mask first
# to generate density, and the classify according to WDI country rates).
pop <- raster("./LandScan_2012/lspop2012/w001001.adf")
wdi <- fread("./LandScan_2012/wdi/sp.rur.totl.zs_Indicator_en_csv_v2.csv")
land <- raster("./GPWv4/gpw-v4-land-water-area_land.tif")

# Simple pop sum
proj4string(pop)
l2 <- spTransform(l2, proj4string(pop))
pop <- crop(pop, l2)
tmp <- extract(pop, l2, fun=sum, na.rm=T, small=T)
l2.pop <- data.table(rn=row.names(l2), PN12_TOT=tmp)

# Verify
setkey(l2.pop, rn)
setkey(l2.dt, rn)
l2.pop <- l2.dt[l2.pop]
l2.pop[, sum(PN12_TOT, na.rm=T), by=ISO3]
# => OK

# Land area: the total area of a surface represented by a given pixel in a uniform
# grid varies with latitude. In order to capture this spatial variation, surface areas
# were calculated using a locally-specified Mollweide Projection (EPSG:54009) on the
# input administrative vector units. The land and water area grids represent different
# portions of the calculated surface area. The land area grid represents the land area
# (without permanent ice or water) of a pixel in square kilometers. The water area
# grid measures the water area (including permanent ice and water) of a pixel in
# square kilometers. The land area plus the water area of a pixel equal the total
# surface area of that pixel.
proj4string(land)
land <- crop(land, l2)
tm_shape(l2) + tm_borders() + tm_shape(land) + tm_raster(alpha=.5)
tmp <- extract(land, l2, fun=sum, na.rm=T, small=T)
setkey(l2.pop, rn)
l2.pop[row.names(l2), land_area_km := tmp]
l2.pop[, PD12_TOT := PN12_TOT/land_area_km]


# Rural pop density: need to get the density of each pixel, then classify pixels into
# urban/rural according to published WDI country rates.
popdens <- pop/land
writeRaster(popdens, "./LandScan_2012/lspop2012_density.tif")
popdens <- raster("./LandScan_2012/lspop2012_density.tif")
summary(popdens)


iso3 <- l2.dt[, unique(ISO3)]
rural <- wdi[`Country Code` %in% iso3, .(ISO3=`Country Code`, rurate=as.numeric(`2014`))]

for (i in iso3 ) {
  tmp.l2 <- l2[l2$ISO3==i,]
  tmp <- mask(crop(popdens, tmp.l2), tmp.l2)
  tmp.pop <- as.vector(tmp)
  tmp.pop <- sort(tmp.pop)
  cutoff <- pop.dt[cumsum(pop.dt) >= totpop*rural.rate][1]
  cutoff

  # Classify 0-rural/1-urban areas using cutoff value
  m <- c(0, cutoff, 0, cutoff, max(pop.dt)+1, 1)
  m <- matrix(popdens, ncol=3, byrow=T)
  pop.rur <- reclassify(popdens, m, include.lowest=T)

  # Add levels
  rat <- data.frame(ID=0:1, levels=c("rural", "urban"))
  levels(pop.rur) <- rat

  # Count urban/rural pop

}



rm(cl, tmp, i, x, y, svy.split, iso3)
save.image("./out/r16.05/svyL2Maps_r16.05.RData")

