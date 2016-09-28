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
# /SDA/analysis/_global_codes/2016.05/svyMaps_2016.06.22_sara.shp
# and also shared with JG.
#
# Malaria incidence downloaded into ./MAP
# Conflict data downloaded into ./ACLED
# CRU_TS.3.23 downloaded into ./CRU_TS.3.23
# SPEI downloaded into ./PDSI
# GPCC V7 monthly climate downloaded into ./GPCC

library(data.table)
library(raster)
library(tmap)
library(stringr)

setwd("~/Projects/hc-data")
load("./out/2016.05/svyL2Maps_r16.05.RData")

# Load Sara's list of survey admin units
dist <- fread("./out/2016.05/District codes and names.csv")

# Load survey maps
l2 <- readOGR("./out/2016.05", "svyMaps_2016.06.22_sara")
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
write.dta(l2.cell5m, "./out/2016.05/svyL2Maps-CELL5M.dta", convert.factors="string", version=12L)



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
write.dta(l2.spei, "./out/2016.05/svyL2Maps-SPEI_monthly_1950-2014.dta",
  convert.factors="string", version=12L)

spei.lbl <- c(l2.lbl, paste0("mean SPEI index (based in CRU_TS 3.23) in Y", 1950:2014))
attr(l2.spei.year, "var.labels") <- spei.lbl
setkey(l2.spei.year, svyCode, svyL1Cd, svyL2Cd)
write.dta(l2.spei.year, "./out/2016.05/svyL2Maps-SPEI_yearly_1950-2014.dta",
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
write.dta(l2.spei.drought, "./out/2016.05/svyL2Maps-SPEI_drought_1950-2014.dta",
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
# Generate altitude and slope

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
write.dta(l2.srtm, "./out/2016.05/svyL2Maps-SRTM.dta",
  convert.factors="string", version=12L)



###############################################
## UDEL v4.01
#
# Note that NCER re-analysis and CMAP are also at 2.5 degree, GPCC is also at 30-min
# similar to WorldClim.
# http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis.surface.html
# The NOAA-CIRES Twentieth Century Reanalysis (V2c) is at 2 degree
# We choose to use UDEL v4.01 instead:
# Willmott, C. J. and K. Matsuura (2001) Terrestrial Air Temperature and
# Precipitation: Monthly and Annual Time Series (1950 - 1999),
# http://climate.geog.udel.edu/~climate/html_pages/README.ghcn_ts2.html.
# http://www.esrl.noaa.gov/psd/data/gridded/data.UDel_AirT_Precip.html
#
# We have the option to weight the biovars by cropland (not done below)

#pre <- brick("./UDEL/precip.mon.total.v401.nc")
#temp <- brick("./UDEL/air.mon.mean.v401.nc")
pre <- brick("./UDEL/precip.mon.total.v401.epsg4326.nc")
temp <- brick("./UDEL/air.mon.mean.v401.epsg4326.nc")

spplot(pre[["X2010.12.01"]])
spplot(temp[["X2010.12.01"]])

# Note that UDEL longtitudes are from 0, 360 instead of -180, 180. Use `rotate()`.
#system("gdalwarp -t_srs WGS84 -wo SOURCE_EXTRA=1000 -multi -dstnodata None \
#   --config CENTER_LONG 0 -of netCDF \
#   precip.mon.total.v401.nc  precip.mon.total.v401.WGS84.nc")
# system("ncks -d lon,340.0,100.0 -d lat,-50.0,50.0 precip.mon.total.v401.nc precip.mon.total.v401.SSA.nc")
# => these solutions don't work

pre <- rotate(pre, filename="./UDEL/precip.mon.total.v401.epsg4326.nc")
temp <- rotate(temp, filename="./UDEL/air.mon.mean.v401.epsg4326.nc")

res(pre)
# [1] 0.5 0.5 => 50km
res(temp)
# [1] 0.5 0.5
l2 <- spTransform(l2, proj4string(pre))
pre <- crop(pre, l2)
temp <- crop(temp, l2)

# Doco says 1901/01 - 2014/12
names(pre)[1:10]
tail(names(pre), 10)
dim(pre)
# [1]  125  136 1380
tm <- seq(as.Date("1900-01-01"), as.Date("2014-12-31"), "month")
pre <- setZ(pre, tm, "month")
temp <- setZ(temp, tm, "month")

# Keep 1950/01-2014/12
pre <- subset(pre, 601:1380)
temp <- subset(temp, 601:1380)

# Extract monthly mean and total precipitation
tmp <- extract(pre, l2, fun=mean, na.rm=T)
dim(tmp)
# [1] 2078 780
tmp <- data.table(rn=row.names(l2), tmp)
setnames(tmp, 2:781, format(tm[601:1380], "%Y-%m-%d"))
tmp <- melt(tmp, id.vars=c("rn"), variable.name="month", value.name="pre_mean", variable.factor=F)
tmp[, month := as.Date(month)]
l2.udel <- tmp

tmp <- extract(pre, l2, fun=sum, na.rm=T, small=T)
tmp <- data.table(rn=row.names(l2), tmp)
setnames(tmp, 2:781, format(tm[601:1380], "%Y-%m-%d"))
tmp <- melt(tmp, id.vars=c("rn"), variable.name="month", value.name="pre_total", variable.factor=F)
tmp[, month := as.Date(month)]
setkey(l2.udel, rn, month)
setkey(tmp, rn, month)
l2.udel$pre_total <- tmp[l2.udel][, pre_total]

summary(l2.udel$month)
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max.
# "1950-01-01" "1966-03-24" "1982-06-16" "1982-06-16" "1998-09-08" "2014-12-01"
summary(l2.udel$pre_mean)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#   0.000   1.230   6.587   9.180  14.570 111.800    7800
summary(l2.udel$pre_total)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.00    2.44   12.41   31.57   32.47 2175.00

# Extract monthly mean, min and max temperature
tmp <- extract(temp, l2, fun=mean, na.rm=T, small=T)
tmp <- data.table(rn=row.names(l2), tmp)
setnames(tmp, 2:781, format(tm[601:1380], "%Y-%m-%d"))
tmp <- melt(tmp, id.vars=c("rn"), variable.name="month", value.name="temp_mean", variable.factor=F)
tmp[, month := as.Date(month)]
setkey(tmp, rn, month)
l2.udel$temp_mean <- tmp[l2.udel][, temp_mean]

tmp <- extract(temp, l2, fun=min, na.rm=T, small=T)
tmp <- data.table(rn=row.names(l2), tmp)
setnames(tmp, 2:781, format(tm[601:1380], "%Y-%m-%d"))
tmp <- melt(tmp, id.vars=c("rn"), variable.name="month", value.name="temp_min", variable.factor=F)
tmp[, month := as.Date(month)]
setkey(tmp, rn, month)
l2.udel$temp_min <- tmp[l2.udel][, temp_min]

tmp <- extract(temp, l2, fun=max, na.rm=T, small=T)
tmp <- data.table(rn=row.names(l2), tmp)
setnames(tmp, 2:781, format(tm[601:1380], "%Y-%m-%d"))
tmp <- melt(tmp, id.vars=c("rn"), variable.name="month", value.name="temp_max", variable.factor=F)
tmp[, month := as.Date(month)]
setkey(tmp, rn, month)
l2.udel$temp_max <- tmp[l2.udel][, temp_max]

summary(l2.udel$temp_mean)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#    -2.20   19.20   22.50   22.46   25.78   38.50    7800
summary(l2.udel$temp_min)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   -4      18      22     Inf      25     Inf
summary(l2.udel$temp_max)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  -Inf      20      24    -Inf      26      40

# Where are the missing values?
bad.na <- l2.udel[is.na(pre_mean), unique(rn)]
#       rn   N
#  1: 1042 780
#  2: 1043 780
#  3: 1044 780
#  4: 1045 780
#  5: 1046 780
#  6: 1048 780 => all same as "1047"
#  7:  107 780 => 153
#  8:  294 780 => 295
#  9:   31 780 => 138
# 10:  431 780 => 383

# Impute all missing values (in all years)
bad.repl <- c(rep("1047", 6), "153", "295", "138", "383")
bad.na <- data.table(rn=bad.na, repl=bad.repl)
setkey(bad.na, repl)
setkey(l2.udel, rn)
bad.na <- l2.udel[bad.na]
bad.na <- bad.na[, .SD, .SDcols=c("rn", "i.rn", "month", "pre_mean", "temp_mean")]
setnames(bad.na, c("rn", "i.rn"), c("repl", "rn"))

setkey(bad.na, rn, month)
setkey(l2.udel, rn, month)
l2.udel <- bad.na[l2.udel]
l2.udel[is.na(i.pre_mean), i.pre_mean := pre_mean]
l2.udel[is.na(i.temp_mean), i.temp_mean := temp_mean]
l2.udel[, pre_mean := NULL]
l2.udel[, temp_mean := NULL]
setnames(l2.udel, c("i.pre_mean", "i.temp_mean"), c("pre_mean", "temp_mean"))
l2.udel[, repl := NULL]

# Verify
summary(l2.udel$pre_mean)
summary(l2.udel$temp_mean)

# Convert cm/month to mm/month
l2.udel[, `:=`(
  pre_mean=pre_mean*10,
  pre_total=pre_total*10)]

# Verify
tmp.l2 <- SpatialPolygonsDataFrame(l2, data.frame(l2.udel[month==max(l2.udel$month)]), match.ID="rn")
writeOGR(tmp.l2, "./out/2016.05", "svyL2Maps-UDEL", "ESRI Shapefile", overwrite=T)

tm_shape(tmp.l2) +
  tm_polygons("pre_mean", colorNA="grey", border.col="black", palette="Spectral", n=12) +
  tm_shape(pre) + tm_raster("X1380", palette="-Spectral")

# Also check SPEI
tmp.l2 <- SpatialPolygonsDataFrame(l2, data.frame(l2.spei[month==max(l2.spei$month)]), match.ID="rn")
tm_shape(tmp.l2) + tm_fill("spei", colorNA="grey", palette="-Spectral")
# => seems ok

# Also check SRTM
tmp.l2 <- SpatialPolygonsDataFrame(l2, data.frame(l2.srtm), match.ID="rn")
tm_shape(tmp.l2) + tm_fill("alt_med", colorNA="grey", palette="-Spectral")
# => seems ok


# Yearly summary
l2.udel.year <- l2.udel[, .(
  pre_mean=mean(pre_mean, na.rm=T), # mean monthly rain over the year
  pre_total=sum(pre_mean, na.rm=T), # total rain over the year
  temp_mean=mean(temp_mean, na.rm=T),
  temp_min=min(temp_min, na.rm=T),
  temp_max=max(temp_max, na.rm=T)), by=.(rn, year=year(month))]

# 30-year seasonal summary
l2.udel.jandec <- l2.udel[month %between% c("1984-01-01", "2014-12-31"), .(
  pre_mean=mean(pre_mean, na.rm=T), # mean monthly rain
  pre_total=mean(pre_total, na.rm=T), # monthly mean of total rain over the year
  temp_mean=mean(temp_mean, na.rm=T),
  temp_min=min(temp_min, na.rm=T),
  temp_max=max(temp_max, na.rm=T)), by=.(rn, month=month(month))]

# Can we also process 19 long-term biovars?
# dismo::biovars() process 12 layers of monthly data, but requires tmin and tmax
# which does not seem to be available at that resolution, only from CRU_TS at 5 degree

# Export
setkey(l2.udel, rn)
setkey(l2.dt, rn)
setkey(l2.udel.yearly, rn)
setkey(l2.udel.jandec, rn)
l2.udel <- l2.dt[, .SD, .SDcols=1:7][l2.udel]
l2.udel.year <- l2.dt[, .SD, .SDcols=1:7][l2.udel.year]
l2.udel.jandec <- l2.dt[, .SD, .SDcols=1:7][l2.udel.jandec]

attr(l2.udel, "var.labels") <-c(l2.lbl[1:7],
  "month",
  "precipitation (mean, mm/month)",
  "precipitation (total rain over the entire area, mm/month)",
  "temperature (mean, deg C)",
  "temperature (min, deg C)",
  "temperature (max, deg C)")

attr(l2.udel.year, "var.labels") <-c(l2.lbl[1:7],
  "year",
  "precipitation (monthly mean over the year, mm/month)",
  "precipitation (total rain over the year, mm/year)",
  "temperature (mean over the year, deg C)",
  "temperature (min over the year, deg C)",
  "temperature (max over the year, deg C)")

attr(l2.udel.jandec, "var.labels") <-c(l2.lbl[1:7],
  "month",
  "precipitation (30-year mean for that month, mm/month)",
  "precipitation (30-year monthly rain over the entire area, mm/month)",
  "temperature (30-year mean, deg C)",
  "temperature (30-year min, deg C)",
  "temperature (30-year max, deg C)")


write.dta(l2.udel, "./out/2016.05/svyL2Maps-UDEL_monthly_1950-2014.dta",
  convert.factors="string", version=12L)
write.dta(l2.udel.year, "./out/2016.05/svyL2Maps-UDEL_yearly_1950-2014.dta",
  convert.factors="string", version=12L)
write.dta(l2.udel.jandec, "./out/2016.05/svyL2Maps-UDEL_seasonal_1984-2014.dta",
  convert.factors="string", version=12L)


###############################################
## LandScan 2012
#
# Might also need rural population density (should apply GPWv4 land/water mask first
# to generate density, and then classify according to WDI country rates).
# Note that I had to redownload from IFPRI G:\ drive, pop count and densities were not
# plotting all right (seemed inverted)
pop <- raster("./LandScan_2012/RasterGISbinary/lspop2012.flt")
land <- raster("./GPWv4/gpw-v4-land-water-area_land.tif")
wdi <- fread("./LandScan_2012/wdi/sp.rur.totl.zs_Indicator_en_csv_v2.csv")

# Simple pop sum
proj4string(pop)
l2 <- spTransform(l2, proj4string(pop))
pop <- crop(pop, l2)
tmp <- extract(pop, l2, fun=sum, na.rm=T, small=T)
l2.pop <- data.table(rn=row.names(l2), PN12_TOT=tmp)

# TODO Scale to WDI 2012
wdi.pop <- read.csv("./WDI/API_SP.POP.TOTL_DS2_en_csv_v2.csv", header=T)
wdi.pop <- data.table(wdi.pop)
wdi.pop[Country.Code=="NGA"]
l2.pop.iso3 <- l2.pop[, .(ls=sum(PN12_TOT, na.rm=T)), by=.(ISO3, svyCode)]
setkey(wdi.pop, Country.Code)
setkey(l2.pop, ISO3)
l2.pop.iso3$wdi <- wdi.pop[l2.pop.iso3][, X2012]
l2.pop.iso3[, fact := wdi/ls]
setkey(l2.pop.iso3, ISO3)
setkey(l2.pop, ISO3)
l2.pop$fact <- l2.pop.iso3[l2.pop][, fact]

# Verify
l2$PN12_TOT <- NULL
l2$PD12_TOT <- NULL
l2.dt <- data.table(l2@data)
l2.dt[, rn := row.names(l2)]
setkey(l2.pop, rn)
setkey(l2.dt, rn)
l2.pop <- l2.dt[l2.pop]
l2.pop[, sum(PN12_TOT, na.rm=T), by=ISO3][order(V1)]
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
# There are a number of more highly-modeled methods, including dasymetric modeling and
# smart interpolation (Hay et al., 2005), that incorporate additional geographic data.
# These data are used to produce weight matrices for determining how to apportion
# population by pixel. Several global data products use ancillary data in their
# spatial modeling, incorporating remotely sensed data on land cover, urban extent,
# accessibility, or all of the above in order to delineate population surfaces
# (Bhaduri et al., 2002; Balk et al., 2006; Tatem et al., 2007). GPWv4, however, uses
# the areal-weighting approach.
proj4string(land)
land <- crop(land, l2)
tm_shape(l2) + tm_borders() + tm_shape(land) + tm_raster(alpha=.5)
tmp <- extract(land, l2, fun=sum, na.rm=T, small=T)
setkey(l2.pop, rn)
l2.pop[row.names(l2), land_area_km := tmp]
l2.pop[, PD12_TOT := PN12_TOT/land_area_km]
l2.pop[PN12_TOT==0, PD12_TOT := 0]
l2.pop[, PN12_TOT_WDI := PN12_TOT*fact]
l2.pop[, PD12_TOT_WDI := PN12_TOT_WDI/land_area_km]
l2.pop[land_area_km==0, PD12_TOT_WDI := 0]

# Verify again
l2.pop[, sum(PN12_TOT_WDI, na.rm=T), by=ISO3]
l2.pop[, fact := NULL]

# Plot these results
tmp.m <- SpatialPolygonsDataFrame(l2, data.frame(l2.pop), match.ID="rn")
tm_shape(tmp.m) + tm_fill("PD12_TOT_WDI", style="kmeans", n=9)
# => looks ok

# Export
attr(l2.pop, "var.labels") <- c(l2.lbl,
  "land area (sq. km. GPWv4 land mask)",
  "population (pp, 2012, LandScan)",
  "population density (pp/sq. km., 2012, LandScan)",
  "population sacled to WDI (pp, 2012, LandScan)",
  "population density scaled to WDI (pp/sq. km., 2012, LandScan)")
write.dta(l2.pop, "./out/2016.05/svyL2Maps-LandScan2012.dta",
  convert.factors="string", version=12L)


###############################################
# TODO Rural pop density: need to get the density of each pixel, then classify pixels
# into urban/rural according to published WDI country rates.
popdens <- pop/land
popdens <- mask(popdens, pop, maskvalue=0, updatevalue=0)
popdens <- mask(popdens, land, maskvalue=0, updatevalue=0)
popdens <- mask(popdens, pop, maskvalue=NA, updatevalue=NA)
popdens <- mask(popdens, land, maskvalue=NA, updatevalue=NA)

# Verify
pop <- setMinMax(pop)
minValue(pop)
# [1] 0
maxValue(pop)
# [1] 99412
minValue(popdens)
# [1] 0
maxValue(popdens)
# [1] 88751130
# => note that this seems to give erroneous densities

tm_shape(pop) + tm_raster(style="kmeans", n=9)
tm_shape(popdens) + tm_raster(style="kmeans", n=9)

writeRaster(popdens, "./LandScan_2012/lspop2012_density.tif", overwrite=T)
popdens <- raster("./LandScan_2012/lspop2012_density.tif")

iso3 <- l2.dt[, unique(ISO3)]
rural <- wdi[`Country Code` %in% iso3, .(ISO3=`Country Code`, rate=as.numeric(`2014`)/100)]

r.rural <- lapply(iso3, function(x) {
  tmp.m <- l2[l2$ISO3==x,]
  tmp.r <- mask(crop(popdens, tmp.m), tmp.m)
  tmp.v <- as.vector(tmp.r)
  tmp.v <- sort(tmp.v)
  cutoff <- tmp.v[cumsum(tmp.v) >= tmp.v*rural[ISO3==x, rate]]
  cutoff

  # Classify 0-rural/1-urban areas using cutoff value
  m <- c(0, cutoff, 1, cutoff, max(tmp.v)+1, 0)
  m <- matrix(m, ncol=3, byrow=T)
  tmp.rur <- reclassify(tmp.r, m, include.lowest=T)

  # Add levels
  rat <- data.frame(ID=0:1, levels=c("urban", "rural"))
  levels(tmp.rur) <- rat
  return(tmp.rur)
})


###############################################
## TODO WorldClim
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
write.dta(bio.wc.mth, "./out/2016.05/svyL2Maps-WorldClim-1km_month_1950-2000.dta",
  convert.factors="string", version=12L)


rm(cl, tmp, tmp.m, tmp.r, i, x, y, svy.split, iso3, l2.pop.iso3)
save.image("./out/2016.05/svyL2Maps_r16.05.RData")



#####################################################################################
# 2016.09.23 SPEI for UGA, TZA and GHA Panel regressions (IFPRI Brown Bag)
#####################################################################################

# Use most recent SPEIbase v2.4 at multiple scales: 3, 6, 12, 24, and 48 months
# Provide monthly summaries for 1950-2014 period
# Tim shared all admin units, at /hc-data/Admin/2016.09
# Details on SPEI at http://sac.csic.es/spei/home.html
# Downloads at http://digital.csic.es/handle/10261/128892
#
# The Global SPEI database, SPEIbase, offers long-time, robust information about
# drought conditions at the global scale, with a 0.5 degrees spatial resolution and a
# monthly time resolution. It has a multi-scale character, providing SPEI time-scales
# between 1 and 48 months. Currently it covers the period between January 1901 and
# December 2014. The SPEIbase is based on the FAO-56 Penman-Monteith estimation of
# potential evapotranspiration. This is a major difference with respect to the SPEI
# Global Drought Monitor, that uses the Thornthwaite PET estimation. The Penman-Montheith
# method is considered a superior method, so the SPEIbase is recommended for most uses
# including long-term climatological analysis.

# TODO: note that earlier results for Carlo/Sara poverty paper were rin on AWS and I
# forgot to copy over all output files from /hc-data/out/2016.05/to BUSTER.

library(data.table)
library(raster)
library(tmap)
library(foreign)
library(curl)

setwd("~/Projects/hc-data")
load("./out/2016.09/svyL2Maps_r16.09.RData")

# Load, verify and combine shapefiles
g2 <- shapefile("./Admin/2016.09/StudyCountries_06_28_2016_FirstHalf.shp")
tmp <- shapefile("./Admin/2016.09/StudyCountries_07_07_2016_SecHalf.shp")

proj4string(g2)
# [1] "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

proj4string(tmp)
# [1] "+proj=tmerc +lat_0=0 +lon_0=33 +k=0.9996 +x_0=500000 +y_0=200000 +datum=WGS84
# +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

tmp <- spTransform(tmp, proj4string(g2))
unique(g2$svyCode)
# [1] "tza2007" "gha2005" "uga2005" NA
unique(tmp$svyCode)
# [1] "uga2013" "tza2011" "gha2012"

g2$ISO3 <- NULL
names(g2)[2] <- "ISO3"
names(g2)[17] <- "svyL1Cd"
g2 <- rbind(g2[, names(tmp)], tmp)
g2$rn <- row.names(g2)
g2.dt <- data.table(g2@data)
g2.dt[svyCode=="gha2005", ISO3 := "GHA"]
g2.dt[svyCode=="uga2005", ISO3 := "UGA"]
g2.dt[, unique(svyCode)]
g2.dt[, unique(ISO3)]

g2.dt[, .N, keyby=.(ISO3, svyCode)]
# ISO3 svyCode   N
# 1:   NA      NA   2
# 2:  GHA gha2005 110
# 3:  GHA gha2012 170
# 4:  TZA tza2007 136
# 5:  TZA tza2011 168
# 6:  UGA uga2005  56
# 7:  UGA uga2013 114

g2.dt[, paste(range(svyL2Cd), collapse=" - "), keyby=.(ISO3, svyCode)]
# ISO3 svyCode         V1
# 1:   NA      NA      0 - 0
# 2:  GHA gha2005 101 - 1005
# 3:  GHA gha2012     1 - 27
# 4:  TZA tza2007      0 - 8
# 5:  TZA tza2011      0 - 8
# 6:  UGA uga2005  101 - 415
# 7:  UGA uga2013    0 - 426

# => svyL2Cd codes are not unique, combine with svyL1Cd

# Map and recode the NA units (Lake Albert, Lake Victoria)
g2.dt[rn=="276", `:=`(ISO3="UGA", svyCode="uga2005")]
g2.dt[rn=="277", `:=`(ISO3="UGA", svyCode="uga2005")]
g2.dt[is.na(prttyNm), prttyNm := svyL2Nm]
g2.dt[, svyL1Cd := as.integer(svyL1Cd)]
g2.dt[, svyL2Cd := as.integer(svyL2Cd)]
g2.dt[svyL2Cd==0, .N, by=svyCode]

# Reattach
g2 <- SpatialPolygonsDataFrame(g2, data.frame(g2.dt), match.ID="rn")


# Download NC files from CSIC
nc.url <- c(
  "http://digital.csic.es/bitstream/10261/128892/5/spei03.nc",
  "http://digital.csic.es/bitstream/10261/128892/8/spei06.nc",
  "http://digital.csic.es/bitstream/10261/128892/14/spei12.nc",
  "http://digital.csic.es/bitstream/10261/128892/26/spei24.nc",
  "http://digital.csic.es/bitstream/10261/128892/50/spei48.nc")

nc.var <- c(
  "spei03",
  "spei06",
  "spei12",
  "spei24",
  "spei48")

nc <- lapply(1:5, function(x) curl_download(nc.url[x], paste0("./SPEIbase/", nc.var[x], ".nc")))
nc <- unlist(nc)
nc[1] <- "./SPEIbase/spei03.nc"

spei <- brick("./SPEIbase/spei03.nc")
identical(proj4string(spei), proj4string(g2))
g2 <- spTransform(g2, proj4string(spei))
tm <- seq(as.Date("1901-01-01"), as.Date("2014-12-31"), "month")

speiMean <- function(x) {
  spei <- brick(nc[x])
  spei <- setZ(spei, tm, "month")
  spei <- subset(spei, 589:1368)

  spei <- crop(spei, g2)
  tmp <- extract(spei, g2, fun=mean, na.rm=T, small=T)
  names(dimnames(tmp)) <- c("rn", "month")

  # Monthly summaries
  dt <- data.table(g2@data)
  dt[, rn := row.names(g2)]
  tmp <- as.data.table(tmp)
  setnames(tmp, as.character(seq(as.Date("1950-01-01"), as.Date("2014-12-31"), "month")))
  tmp[, rn := row.names(g2)]
  tmp <- melt(tmp, id.vars="rn", variable.name="month", value.name=nc.var[x])

  setkey(tmp, rn)
  setkey(dt, rn)
  dt <- dt[tmp]
  dt[, month := as.Date(month)]
  return(dt)
}

out <- lapply(1:5, speiMean)
sapply(out, dim)
out <- cbind(out[[1]], out[[2]]$spei06, out[[3]]$spei12, out[[4]]$spei24, out[[5]]$spei48)
setnames(out, 11:14, c("spei06", "spei12", "spei24", "spei48"))

# Verify
out[, lapply(.SD, summary), .SDcols=10:14]
# spei03    spei06    spei12    spei24    spei48
# 1:    -4.120    -5.123    -5.193    -4.084    -3.298
# 2:    -0.696    -0.656    -0.616    -0.612    -0.591
# 3:     0.010     0.021     0.038     0.041     0.067
# 4:     0.019     0.034     0.054     0.064     0.090
# 5:     0.710     0.716     0.706     0.730     0.761
# 6:     3.227     3.419     3.446     3.572     3.316
# 7: 22645.000 22620.000 22620.000 22620.000 22620.000

# There are a bunch of empty values (some are okay due to Zanzibar islands and Lake Victoria)
tmp <- out[is.na(spei03), .N, keyby=.(svyCode, rn)]
tmp <- out[is.na(spei03), unique(rn)]

# Plot it
tmap_mode("view")
g2.out <- SpatialPolygonsDataFrame(g2, data.frame(out[month=="2014-01-01"]), match.ID="rn")
tm_shape(spei[["X2014.01.16"]]) + tm_raster() +
  tm_shape(g2.out[g2.out$rn %in% tmp,], is.master=T) + tm_polygons("svyL1Nm") +
  tm_layout(legend.outside=T)

# Look at missing values, and impute using nearest admin unit
tm_shape(g2.out[g2.out$svyCode=="tza2007",]) + tm_polygons()

bad <- c("235", "313", "132", "133", "134", "135", "30", "31", "28", "29", "43", "44",
  "90", "94", "1351", "2271", "2721", "2731", "2741", "2751", "2761", "2771", "2781",
  "2791", "2801", "2811", "260", "931", "58")

impute <- c("241", "422", "91", "91", "91", "91", "91", "91", "131", "131", "131", "131",
  "57", "96", "1821", "1141", "1981", "1141", "1141", "1141", "1981", "1141", "1141",
  "1981", "1141", "1981", "280", "3100", "59")

for (i in 1:29) out[rn==bad[i], `:=`(
  spei03 = out[rn==impute[i], spei03],
  spei06 = out[rn==impute[i], spei06],
  spei12 = out[rn==impute[i], spei12],
  spei24 = out[rn==impute[i], spei24],
  spei48 = out[rn==impute[i], spei48])]



g2.lbl <- c("ISO3 code", "survey code",
  "adm-1 code (in survey)", "admin-1 name (in survey)",
  "adm-2 code (in survey, 0 is missing or not surveyed)", "admin-2 name (in survey)",
  "print label", "shape id")

attr(out, "var.labels") <- c(g2.lbl, "month",
  "SPEI 3-month scale (mean)",
  "SPEI 6-month scale (mean)",
  "SPEI 12-month scale (mean)",
  "SPEI 24-month scale (mean)",
  "SPEI 48-month scale (mean)")

write.dta(out, "./out/2016.09/svyL2Maps-SPEIbase.2.4_1950-2014_monthly.dta",
  convert.factors="string", version=12L)


#####################################################################################
## Same problem with the LSMS-ISA panel GPS coordinates, received from Tim
gps <- c("Tanz_Y1", "Tanz_Y2", "Tanz_Y3", "UGA_Y1", "UGA_Y2", "UGA_Y3")
gps <- lapply(gps, function(x) shapefile(paste0("./Admin/2016.09/", x)))


# | svyCode | var       |    N | HHID | AE+HHID     | Range                                 | Bad N |
# |---------+-----------+------+------+-------------+---------------------------------------+-------|
# | tza2008 | hhid      | 2806 | 1258 | 1726        | 1010140020170    ->    55020180210100 |     0 |
# | tza2008 | hhid_text | 2806 | 2806 | 2806        | 10010030040014   ->     9050123250059 |     0 |
# |---------+-----------+------+------+-------------+---------------------------------------+-------|
# | tza2010 | y2_hhid   | 3917 | 1367 | 1975        | 101014002017000  ->  5502018021010000 |     0 |
# | tza2010 | HH_text   | 3917 | 3495 | 3495        | 1001003004001400 ->   905012325008102 |       |
# |---------+-----------+------+------+-------------+---------------------------------------+-------|
# | tza2012 | y3_hhid   | 4988 | 4988 | no `ea` var | 0001-001         ->          3924-001 |     0 |
# |---------+-----------+------+------+-------------+---------------------------------------+-------|
# | uga2009 | HHID      | 2975 | 2931 | no `ea` var | 1013000201       ->      418300230802 |    24 |
# |---------+-----------+------+------+-------------+---------------------------------------+-------|
# | uga2010 | HHID      | 2716 | 2685 | no `ea` var | 1013000201       ->   211530004040000 |    45 |
# |---------+-----------+------+------+-------------+---------------------------------------+-------|
# | uga2011 | HHID      | 2850 | 1074 | no `ea` var | 0                ->        2143000310 |    89 |
# | uga2011 | HHID_Text | 2850 | 2850 | no `ea` var | 1013000201       ->        4193003509 |    89 |


sapply(gps, names)
# Clean up and rename a few fields
# There's a `HHID_1` in `UGA_Y3_2` what's that?
summary(gps[[6]]$HHID)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 0.000e+00 0.000e+00 0.000e+00 5.410e+08 1.073e+09 2.143e+09
summary(gps[[6]]$HHID_1)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 0.000e+00 0.000e+00 0.000e+00 5.410e+08 1.073e+09 2.143e+09

tmp <- data.table(gps[[6]]@data)
tmp[HHID!=HHID_1]
# => no difference

names(gps[[1]])[c(1, 3, 4, 2)] <- c("hhid", "Y_mod", "X_mod", "ea")
names(gps[[2]])[c(1, 3, 4, 2)] <- c("hhid", "Y_mod", "X_mod", "ea")
names(gps[[3]])[c(1, 48, 49)] <- c("hhid", "Y_mod", "X_mod")
names(gps[[4]])[c(1, 2, 3, 7)] <- c("hhid", "Y_mod", "X_mod", "urban")
names(gps[[5]])[c(1, 2, 3, 8)] <- c("hhid", "Y_mod", "X_mod", "urban")
names(gps[[6]])[c(1, 3, 4, 8)] <- c("hhid", "Y_mod", "X_mod", "urban")

gps[[3]]$ea <- as.integer(NA)

gps[[1]]$svyCode <- "tza2008"
gps[[2]]$svyCode <- "tza2010"
gps[[3]]$svyCode <- "tza2012"
gps[[4]]$svyCode <- "uga2009"
gps[[5]]$svyCode <- "uga2010"
gps[[6]]$svyCode <- "uga2011"

gps[[1]]$wave <- "Y1"
gps[[2]]$wave <- "Y2"
gps[[3]]$wave <- "Y3"
gps[[4]]$wave <- "Y1"
gps[[5]]$wave <- "Y2"
gps[[6]]$wave <- "Y3"

gps[[1]]$ISO3 <- "TZA"
gps[[2]]$ISO3 <- "TZA"
gps[[3]]$ISO3 <- "TZA"
gps[[4]]$ISO3 <- "UGA"
gps[[5]]$ISO3 <- "UGA"
gps[[6]]$ISO3 <- "UGA"

gps[1:3] <- lapply(gps[1:3], function(x) x[, c("ISO3", "svyCode", "wave", "hhid", "ea", "Y_mod", "X_mod")])
gps[4:6] <- lapply(gps[4:6], function(x) x[, c("ISO3", "svyCode", "wave", "hhid", "urban", "Y_mod", "X_mod")])

sapply(gps, proj4string)

# Combine by country
tza <- rbind(gps[[1]], gps[[2]], gps[[3]])
uga <- rbind(gps[[4]], gps[[5]], gps[[6]])

# Plot to verify
tmap_mode("view")
tm_shape(tza[tza$wave=="Y1",]) + tm_dots()
tm_shape(tza[tza$wave=="Y2",]) + tm_dots()
tm_shape(tza[tza$wave=="Y3",]) + tm_dots()

# Verify UGA points, they don't plot right
tmp <- shapefile("./Admin/2016.09/UGA_Y1")
tm_shape(tmp) + tm_dots()
tmp <- data.table(coordinates(uga), uga$X_mod, uga$Y_mod, uga$svyCode, uga$wave)
setnames(tmp, c("X", "Y", "X-mod", "Y_mod", "svyCode", "wave"))
uga <- SpatialPointsDataFrame(tmp, uga@data, match.ID=F)
tmp <- data.table(uga@data)
tmp[X_mod==0, .N, keyby=.(svyCode, wave)]
#    svyCode wave  N
# 1: uga2009   Y1 24
# 2: uga2010   Y2 45
# 3: uga2011   Y3 89

# Share these bad records with Beliyou to check with FAO
tmp <- uga@data[uga$lat_mod==0,]
write.dta(tmp, "./out/2016.09/uga2011_missing.dta", version=12L)

# Drop all UGA bad records for now
uga <- uga[uga$X_mod!=0,]
# => 8383 records out of 8541

uniqueN(paste(uga$svyCode, uga$hhid))
# => 6690 unique hhids across waves

# Look for duplicated `hhid` in source files
dim(gps[[4]])
# 2975
uniqueN(gps[[4]]$hhid)
# 2931

dim(gps[[5]])
# 2716
uniqueN(gps[[5]]$hhid)
# 2685

dim(gps[[6]])
# 2850
uniqueN(gps[[6]]$hhid)
# 1074


# Plot each wave
tm_shape(uga[uga$wave=="Y1",]) + tm_dots()
tm_shape(uga[uga$wave=="Y2",]) + tm_dots()
tm_shape(uga[uga$wave=="Y3",]) + tm_dots()
# => looks ok

## SPEIbase
# Compare projections
proj4string(spei)
proj4string(tza)
tza <- spTransform(tza, proj4string(spei))
uga <- spTransform(uga, proj4string(spei))

speiMean <- function(x, y) {
  tm <- seq(as.Date("1901-01-01"), as.Date("2014-12-31"), "month")

  spei <- brick(nc[x])
  spei <- setZ(spei, tm, "month")
  spei <- subset(spei, 589:1368)

  # Crop but buffer by 2 degree too
  spei <- crop(spei, extent(y)+c(-2, 2, -2, 2))
  tmp <- extract(spei, y)
  names(dimnames(tmp)) <- c("rn", "month")

  # Monthly summaries
  tmp <- as.data.table(tmp)
  setnames(tmp, as.character(tm[589:1368]))
  tmp[, rn := row.names(y)]
  tmp <- melt(tmp, id.vars="rn", variable.name="month", value.name=nc.var[x])
  tmp[, month := as.Date(month)]
  return(tmp)
}

out.tza <- lapply(1:5, function(x) speiMean(x, tza))
sapply(out.tza, dim)
out.tza <- cbind(out.tza[[1]], out.tza[[2]]$spei06, out.tza[[3]]$spei12, out.tza[[4]]$spei24, out.tza[[5]]$spei48)
setnames(out.tza, 11:14, c("spei06", "spei12", "spei24", "spei48"))

out.uga <- lapply(1:5, function(x) speiMean(x, uga))
sapply(out.uga, dim)
out.uga <- cbind(out.uga[[1]], out.uga[[2]]$spei06, out.uga[[3]]$spei12, out.uga[[4]]$spei24, out.uga[[5]]$spei48)
setnames(out.uga, 11:14, c("spei06", "spei12", "spei24", "spei48"))

# Plot a sample month to check results
tza <- SpatialPointsDataFrame(tza, data.frame(out.tza[month=="2014-01-01"]), match.ID="rn")
uga <- SpatialPointsDataFrame(uga, data.frame(out.uga[month=="2014-01-01"]), match.ID="rn")

tm_shape(tza) + tm_dots("spei03")
tm_shape(tza) + tm_dots("spei06")
tm_shape(uga) + tm_dots("spei03")
tm_shape(uga) + tm_dots("spei06")
# => looks ok


## Impute any missing SPEI value with nearest point value available
out.tza[is.na(spei03), .N, by=hhid][N>500]
tm_shape(tza[is.na(tza$spei03),]) + tm_dots()

# Find nearest points
library(nabor)
# This library provides very fast knn() nearest neighbor algorithm, because
# rgeos::gDistance() is too slow with thousands of points

# TZA
rn.bad <- out.tza[is.na(spei03), .N, by=rn][N>500][, unique(rn)]
tm_shape(tza[tza$rn %in% rn.bad,]) + tm_dots()
tza.imp <- tza[!tza$rn %in% rn.bad,]
tza.bad <- tza[tza$rn %in% rn.bad,]
tmp <- knn(coordinates(tza.imp), coordinates(tza.bad), k=1)
tmp <- data.table(rn.bad=tza.bad$rn, rn.imp=tza.imp@data[tmp$nn.idx[,1], "rn"])

# Impute TZA
setkey(tmp, rn.imp)
setkey(out.tza, rn)
tmp <- out.tza[tmp]
out.tza[rn %in% rn.bad, .N]
out.tza <- out.tza[!rn %in% rn.bad]
tmp[, rn := NULL]
setnames(tmp, "rn.bad", "rn")
out.tza <- rbind(out.tza, tmp)
setkey(out.tza, svyCode, hhid, month)

# UGA
rn.bad <- out.uga[is.na(spei03), .N, by=rn][N>500][, unique(rn)]
tm_shape(spei[["X2014.01.16"]]) + tm_raster() +
  tm_shape(uga[uga$rn %in% rn.bad,], is.master=T) + tm_dots()
uga.imp <- uga[!uga$rn %in% rn.bad,]
uga.bad <- uga[uga$rn %in% rn.bad,]
tmp <- knn(coordinates(uga.imp), coordinates(uga.bad), k=1)
tmp <- data.table(rn.bad=uga.bad$rn, rn.imp=uga.imp@data[tmp$nn.idx[,1], "rn"])

# Impute UGA
setkey(tmp, rn.imp)
setkey(out.uga, rn)
tmp <- out.uga[tmp]
out.uga[rn %in% rn.bad, .N]
out.uga <- out.uga[!rn %in% rn.bad]
tmp[, rn := NULL]
setnames(tmp, "rn.bad", "rn")
out.uga <- rbind(out.uga, tmp)
setkey(out.uga, svyCode, hhid, month)

# Verify again after imputations
out.uga[is.na(spei03), .N, by=rn]
out.uga[, unique(urban)]
tza <- SpatialPointsDataFrame(tza, data.frame(out.tza[month=="2014-01-01"]), match.ID="rn")
uga <- SpatialPointsDataFrame(uga, data.frame(out.uga[month=="2014-01-01"]), match.ID="rn")

tm_shape(tza[tza$wave=="Y3",]) + tm_dots("spei12", title="Wave Y3<br/>spei12", n=8)
tm_shape(tza[tza$wave=="Y3",]) + tm_dots("spei06", title="Wave Y3<br/>spei06", n=8)
tm_shape(uga[uga$wave=="Y3",]) + tm_dots("spei12", title="Wave Y3<br/>spei12", n=8)
tm_shape(uga[uga$wave=="Y3",]) + tm_dots("spei06", title="Wave Y3<br/>spei06", n=8)
# => looks ok


# Export TZA and UGA to STATA
attr(out.tza, "var.labels") <- c(
  "ISO3 code", "survey code", "wave", "hhld ID (unique)", "enumeration area",
  "latitude", "longitude", "shape ID",
  "month",
  "SPEI 3-month scale (mean)",
  "SPEI 6-month scale (mean)",
  "SPEI 12-month scale (mean)",
  "SPEI 24-month scale (mean)",
  "SPEI 48-month scale (mean)")

write.dta(out.tza, "./out/2016.09/TZA-GPS-SPEIbase.2.4_1950-2014_monthly.dta",
  convert.factors="string", version=12L)

attr(out.uga, "var.labels") <- c(
  "ISO3 code", "survey code", "wave", "hhld ID (unique)", "urban/rural",
  "latitude", "longitude", "shape ID",
  "month",
  "SPEI 3-month scale (mean)",
  "SPEI 6-month scale (mean)",
  "SPEI 12-month scale (mean)",
  "SPEI 24-month scale (mean)",
  "SPEI 48-month scale (mean)")

write.dta(out.uga, "./out/2016.09/UGA-GPS-SPEIbase.2.4_1950-2014_monthly.dta",
  convert.factors="string", version=12L)


#####################################################################################
## 2016.09.26 Biophysical Vars for TZA and UGA LSMS-ISA panels
#####################################################################################
# Re-do above with Tim's STATA files. They seem to have full concordance with survey
# panel `hhid` codes.

library(data.table)
library(raster)
library(tmap)
library(foreign)
library(curl)

setwd("~/Projects/hc-data")
load("./out/2016.09/svyL2Maps_r16.09.RData")


gps <- c("Tanz_Shapefile_Y1", "Tanz_Shapefile_Y2", "Tanz_Shapefile_Y3",
  "UGA_Shapefile_Y1", "UGA_Shapefile_Y2", "UGA_Shapefile_Y3")
gps <- lapply(gps, function(x) read.dta(paste0("./Admin/2016.09/", x, ".dta")))

# Combine all 6 files, need to harmonize var names
sapply(gps, names)
gps <- lapply(gps, data.table)
setnames(gps[[1]], c(1,2,3,4,5,6), c("fid", "hhid", "hhid_str", "ea_id", "Y_mod", "X_mod"))
setnames(gps[[2]], c(1,2,3,4,5), c("fid", "hhid", "ea_id", "Y_mod", "X_mod"))
setnames(gps[[3]], c(1,2,49,50), c("fid", "hhid", "Y_mod", "X_mod"))
setnames(gps[[4]], c(1,2,3,4), c("fid", "hhid", "Y_mod", "X_mod"))
setnames(gps[[5]], c(1,2,3,4), c("fid", "hhid_str", "Y_mod", "X_mod"))
setnames(gps[[6]], c(1,61,2,3), c("fid", "hhid_str", "Y_mod", "X_mod"))

gps[[2]][, hhid_str := hhid]
gps[[3]][, hhid_str := hhid]
gps[[3]][, ea_id := as.character(NA)]
gps[[4]][, hhid_str := hhid]
gps[[4]][, ea_id := as.character(NA)]
gps[[5]][, hhid := hhid_str]
gps[[5]][, ea_id := as.character(NA)]
gps[[6]][, hhid := hhid_str]
gps[[6]][, ea_id := as.character(NA)]

gps <- lapply(gps, function(x) x[, .(fid, hhid, hhid_str, ea_id, Y_mod, X_mod)])

# Add survey codes and waves
svy <- c("tza2008", "tza2010", "tza2012", "uga2009", "uga2010", "uga2011")
svy.wave <- rep(c("Y1", "Y2", "Y3"), 2)
for (i in 1:6) gps[[i]][, svyCode := svy[i]]
for (i in 1:6) gps[[i]][, wave := svy.wave[i]]
gps <- rbindlist(gps)

# Check on unique `hhid` codes and share with Beliyou
gps[, .(.N,
  fid=uniqueN(fid),
  hhid=uniqueN(hhid),
  hhid_str=uniqueN(hhid_str),
  range=paste(range(hhid), collapse=" -> "),
  bad=sum(is.na(X_mod) | X_mod==0, na.rm=T)), keyby=svyCode]

# |svyCode |    N|  fid| hhid| hhid_str|range                                | bad|
# |:-------|----:|----:|----:|--------:|:------------------------------------|---:|
# |tza2008 | 2806| 2806| 2806|     2806|10010030040014 -> 9050123250059      |   0|
# |tza2010 | 3917| 3917| 3917|     3917|0101014002017101 -> 5502018021007801 |   0|
# |tza2012 | 4988| 4988| 4988|     4988|0001-001 -> 3924-001                 |   0|
# |uga2009 | 2975| 2975| 2975|     2975|1013000201 -> 4193003510             |  24|
# |uga2010 | 2716| 2716| 2716|     2716|1013000201 -> 4193003510             |  45|
# |uga2011 | 2850| 2850| 2850|     2850|1013000201 -> 4193003509             |  89|

# Drop all records with missing/bad GPS coords for now
gps <- gps[!(is.na(X_mod) | X_mod==0),]

# Verify count of unique `hhid`
gps[, .(uniqueN(hhid), uniqueN(hhid_str)), by=svyCode]
#    svyCode   V1   V2
# 1: tza2008 2806 2806
# 2: tza2010 3917 3917
# 3: tza2012 4988 4988
# 4: uga2009 2951 2951
# 5: uga2010 2671 2671
# 6: uga2011 2761 2761
# => OK, as expected

gps[, .(uniqueN(hhid), uniqueN(hhid_str)), by=svyCode][, .(sum(V1), sum(V2))]
#       V1    V2
# 1: 20094 20094
nrow(gps)
# [1] 20094 => 158 bad records

gps.pts <- SpatialPointsDataFrame(gps[, .(X_mod,Y_mod)], data.frame(gps),
  proj4string=CRS("+init=epsg:4326"), match.ID=F)

# Visually check each wave
tmap_mode("view")
tm_shape(gps.pts[gps.pts$svyCode==svy[1],]) + tm_dots()
tm_shape(gps.pts[gps.pts$svyCode==svy[2],]) + tm_dots()
tm_shape(gps.pts[gps.pts$svyCode==svy[6],]) + tm_dots()
# => looks ok

## Process SPEIbase
proj4string(spei)
proj4string(gps.pts)
gps.pts <- spTransform(gps.pts, proj4string(spei))
gps.pts$rn <- row.names(gps.pts)

# Extract SPEI values for all waves
# Note that speiMean() defined above adds a 2 degree buffer around the GPS coords
# extent, otherwise it seems we are missing values
out.spei <- lapply(1:5, function(x) speiMean(x, gps.pts))
sapply(out.spei, dim)
out.spei <- cbind(out.spei[[1]], out.spei[[2]]$spei06, out.spei[[3]]$spei12, out.spei[[4]]$spei24, out.spei[[5]]$spei48)
setnames(out.spei, c("rn", "month", "spei03", "spei06", "spei12", "spei24", "spei48"))

# Impute any missing SPEI values with nearest point
tmp <- out.spei[is.na(spei03), .N, by=rn]
tmp[, table(N)]
# N
#   1    2    3    4    8   11  780
# 300  214  154   92   27   29 1901
#
# Some hhlds are missing 1 to 11 months of data, others are missing the entire 780
# Let's impute first the ones missing the entire period (i.e. not covered by the rasters)

# Find nearest points
library(nabor)
# This library provides fast knn() nearest neighbor algorithm, because using
# rgeos::gDistance() is too slow with thousands of points

# Identify hhlds to impute
rn.bad <- out.spei[is.na(spei03), .N, by=rn][N==780][, unique(rn)]
# => 2717 hhlds (but some missing only a few months)
# Let's print them for Beliyou
tm_shape(spei[[100]]) + tm_raster(n=8) +
  tm_shape(gps.pts[gps.pts$rn %in% rn.bad,], is.master=T) + tm_dots()

imp <- gps.pts[!gps.pts$rn %in% rn.bad,]
bad <- gps.pts[gps.pts$rn %in% rn.bad,]
tmp <- knn(coordinates(imp), coordinates(bad), k=1)
tmp <- data.table(rn.bad=rn.bad, rn.imp=imp@data[tmp$nn.idx[,1], "rn"])

# Visual check on the neighbors
tm_shape(spei[["X2014.04.16"]]) + tm_raster(n=8) +
tm_shape(gps.pts[gps.pts$rn %in% rn.bad,]) + tm_dots() +
  tm_shape(gps.pts[gps.pts$rn %in% tmp$rn.imp,]) + tm_dots(col="red")

# Impute missing SPEI values
id.bad <- out.spei[is.na(spei03), .(rn.bad=rn, month)]
setkey(tmp, rn.bad)
setkey(id.bad, rn.bad)
id.bad <- tmp[id.bad]
setkey(id.bad, rn.imp, month)
setkey(out.spei, rn, month)
tmp <- out.spei[id.bad]

setkey(tmp, rn.bad, month)
setkey(out.spei, rn, month)
out.spei[tmp, spei03 := tmp$spei03]
out.spei[tmp, spei06 := tmp$spei06]
out.spei[tmp, spei12 := tmp$spei12]
out.spei[tmp, spei24 := tmp$spei24]
out.spei[tmp, spei48 := tmp$spei48]

# Merge in survey and hhld attributes
tmp <- data.table(gps.pts@data)
setkey(tmp, rn)
setkey(out.spei, rn)
out.spei <- tmp[out.spei]

# Verify
summary(out.spei$spei03)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# -5.08400 -0.69010  0.03662  0.04982  0.77580  3.44100

# Re-check unique `hhid`
out.spei[, .(uniqueN(month), uniqueN(hhid), uniqueN(hhid_str), uniqueN(rn)), by=svyCode]
#    svyCode  V1   V2   V3   V4
# 1: tza2008 780 2806 2806 2806
# 2: tza2010 780 3917 3917 3917
# 3: tza2012 780 4988 4988 4988
# 4: uga2009 780 2951 2951 2951
# 5: uga2010 780 2671 2671 2671
# 6: uga2011 780 2761 2761 2761

# What's the deal with the remaining missing months?
tmp <- out.spei[, is.na(spei03), keyby=.(svyCode, month)]
ggplot(tmp[svyCode %in% svy[1:3]], aes(month, weight=V1)) +
  geom_bar(fill="red") + ylim(0, 70) +
  scale_x_date(date_labels="%y %b") +
  facet_grid(svyCode~.)
tmp <- out.spei[is.na(spei03), .(missing=.N), keyby=.(svyCode, month)]
dcast(tmp, month~svyCode)
# => August, Sep, Oct are missing across a few years (don't know why, does not look
# like the rasters have missing pixel values)

# Let's impute these again as above using nearby values by month
rn.bad <- out.spei[is.na(spei03), .N, by=rn][, unique(rn)]
imp <- gps.pts[!gps.pts$rn %in% rn.bad,]
bad <- gps.pts[gps.pts$rn %in% rn.bad,]
tmp <- knn(coordinates(imp), coordinates(bad), k=1)
tmp <- data.table(rn.bad=rn.bad, rn.imp=imp@data[tmp$nn.idx[,1], "rn"])

# Visual check on the neighbors
tm_shape(spei[["X2014.04.16"]]) + tm_raster(n=8) +
  tm_shape(gps.pts[gps.pts$rn %in% rn.bad,]) + tm_dots() +
  tm_shape(gps.pts[gps.pts$rn %in% tmp$rn.imp,]) + tm_dots(col="red")

# Impute all by month (make sure to impute only the SPEI values)
id.bad <- out.spei[is.na(spei03), .(rn.bad=rn, month)]
setkey(tmp, rn.bad)
setkey(id.bad, rn.bad)
id.bad <- tmp[id.bad]
setkey(id.bad, rn.imp, month)
setkey(out.spei, rn, month)
tmp <- out.spei[id.bad]

setkey(tmp, rn.bad, month)
setkey(out.spei, rn, month)
out.spei[tmp, spei03 := tmp$spei03]
out.spei[tmp, spei06 := tmp$spei06]
out.spei[tmp, spei12 := tmp$spei12]
out.spei[tmp, spei24 := tmp$spei24]
out.spei[tmp, spei48 := tmp$spei48]

summary(out.spei.imp$spei03)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# -5.08400 -0.69130  0.03364  0.04739  0.77420  3.44100


# Plot a sample month to PDF to visually check results
pdf("./out/2016.09/TZA_UGA-GPS-SPEIbase.2.4_2014-01.pdf")
tmp <- out.spei[month=="2014-01-01"]
tmp <- SpatialPointsDataFrame(tmp[, .(X_mod, Y_mod)], data.frame(tmp),
  proj4string=CRS("+init=epsg:4326"), match.ID=F)
tmap_mode("plot")
data(World)

for (j in nc.var) for (i in svy) print(
  tm_shape(World) + tm_borders() +
    tm_shape(tmp[tmp$svyCode==i,], is.master=T) +
    tm_dots(j, size=0.3, title=paste(i, toupper(j), "2014-Jan", sep="\n")) +
    tm_style_grey()
)
dev.off()
# => looks ok


# Export to STATA, make 1 file per country
setkey(out.spei, svyCode, hhid, month)

lbl <- c(
  "ArcGIS shape ID", "hhld ID", "hhld ID - alternate", "enumeration area",
  "latitude", "longitude", "survey code", "wave", "shape ID", "month",
  "SPEI 3-month scale (mean)",
  "SPEI 6-month scale (mean)",
  "SPEI 12-month scale (mean)",
  "SPEI 24-month scale (mean)",
  "SPEI 48-month scale (mean)")

tmp <- out.spei[svyCode %in% svy[1:3]]
attr(tmp, "var.labels") <- lbl
write.dta(tmp, "./out/2016.09/TZA-GPS-SPEIbase.2.4_1950-2014_monthly.dta",
  convert.factors="string", version=12L)

tmp <- out.spei[svyCode %in% svy[4:6]]
attr(tmp, "var.labels") <- lbl
write.dta(tmp, "./out/2016.09/UGA-GPS-SPEIbase.2.4_1950-2014_monthly.dta",
  convert.factors="string", version=12L)


#####################################################################################
## UDEL precipitation, temperature
# - temp from UDEL v4.01
# - pre from UDEL v4.01

pre <- brick("./UDEL/precip.mon.total.v401.epsg4326.nc")
temp <- brick("./UDEL/air.mon.mean.v401.epsg4326.nc")

res(pre)
# [1] 0.5 0.5
res(spei)
# [1] 0.5 0.5

spplot(pre[["X1371"]])
spplot(temp[["X1371"]])

# Don't split GPS coords by country, process all at once to save code
gps.pts <- spTransform(gps.pts, proj4string(pre))
gps.pts$rn <- row.names(gps.pts)

# Don't crop rasters, that somehow creates missing values for points at the margin
#pre <- crop(pre, gps.pts)
#temp <- crop(temp, gps.pts)

# Doco says 1901/01 - 2014/12
names(pre)[1:10]
tail(names(pre), 10)
dim(pre)
# [1]  360  720 1380
tm <- seq(as.Date("1900-01-01"), as.Date("2014-12-31"), "month")
pre <- setZ(pre, tm, "month")
temp <- setZ(temp, tm, "month")

# Keep 1950/01-2014/12
pre <- subset(pre, 601:1380)
temp <- subset(temp, 601:1380)

# Extract monthly precipitation
tmp <- extract(pre, gps.pts)
dim(tmp)
# [1] 20094 780
tmp <- data.table(rn=row.names(gps.pts), tmp)
setnames(tmp, 2:781, format(tm[601:1380], "%Y-%m-%d"))
tmp <- melt(tmp, id.vars=c("rn"), variable.name="month", value.name="pre_udel", variable.factor=F)
tmp[, month := as.Date(month)]
out.udel <- tmp

summary(out.udel$month)
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max.
# "1950-01-01" "1966-03-24" "1982-06-16" "1982-06-16" "1998-09-08" "2014-12-01"
summary(out.udel$pre_udel)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#     0.0     2.8     8.1     9.7    14.2   261.7  375960
# => still quite a few missing values to be imputed

# Extract monthly temperature
tmp <- extract(temp, gps.pts)
tmp <- data.table(rn=row.names(gps.pts), tmp)
setnames(tmp, 2:781, format(tm[601:1380], "%Y-%m-%d"))
tmp <- melt(tmp, id.vars=c("rn"), variable.name="month", value.name="temp_udel", variable.factor=F)
tmp[, month := as.Date(month)]
setkey(tmp, rn, month)
setkey(out.udel, rn, month)
out.udel$temp_udel <- tmp[out.udel][, temp_udel]

summary(out.udel$temp_udel)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#     1.4    21.2    22.8    22.8    24.7    34.6  375960

# Where are the missing values?
rn.bad <- out.udel[is.na(pre_udel), .N, by=rn][N==780][, unique(rn)]
# => 482 hhlds
# Let's print them for Beliyou
tm_shape(pre[[100]]) + tm_raster(n=8) +
  tm_shape(gps.pts[gps.pts$rn %in% rn.bad,], is.master=T) + tm_dots()
# => only TZA islands

imp <- gps.pts[!gps.pts$rn %in% rn.bad,]
bad <- gps.pts[gps.pts$rn %in% rn.bad,]
tmp <- knn(coordinates(imp), coordinates(bad), k=1)
tmp <- data.table(rn.bad=rn.bad, rn.imp=imp@data[tmp$nn.idx[,1], "rn"])

# Impute all (make sure to impute only the SPEI values, not all attributes!)
setkey(tmp, rn.imp)
setkey(out.udel, rn)
tmp <- out.udel[tmp]
setkey(tmp, rn.bad, month)
setkey(out.udel, rn, month)
out.udel[tmp, pre_udel := tmp$pre_udel]
out.udel[tmp, temp_udel := tmp$temp_udel]


# Verify
summary(out.udel$pre_udel)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.000   2.750   8.100   9.723  14.260 261.700
summary(out.udel$temp_udel)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1.40   21.30   22.90   22.87   24.80   34.60

# Convert cm/month to mm/month
out.udel[, pre_udel := pre_udel*10]

# Merge in survey and hhlds attributes
tmp <- data.table(gps.pts@data)
setkey(tmp, rn)
setkey(out.udel, rn)
out.udel <- tmp[out.udel]

# Re-check unique `hhid`
out.udel[, .(uniqueN(month), uniqueN(hhid), uniqueN(hhid_str), uniqueN(rn)), by=svyCode]
#    svyCode  V1   V2   V3   V4
# 1: tza2008 780 2806 2806 2806
# 2: tza2010 780 3917 3917 3917
# 3: tza2012 780 4988 4988 4988
# 4: uga2009 780 2951 2951 2951
# 5: uga2010 780 2671 2671 2671
# 6: uga2011 780 2761 2761 2761
# => OK

# Plot a sample month to PDF to check results
pdf("./out/2016.09/TZA_UGA-GPS-UDEL_2014-01.pdf")
  tmp <- out.udel[month=="2014-01-01"]
  tmp <- SpatialPointsDataFrame(tmp[, .(X_mod,Y_mod)], data.frame(tmp),
  proj4string=CRS("+init=epsg:4326"), match.ID=F)
  tmap_mode("plot")

  for (j in c("pre_udel", "temp_udel")) for (i in svy) print(
    tm_shape(World) + tm_borders() +
      tm_shape(tmp[tmp$svyCode==i,], is.master=T) +
      tm_dots(j, n=8, size=0.3, title=paste(i, j, "2014-Jan", sep="\n")) +
      tm_style_grey())
dev.off()
# => looks ok

# Export to STATA (1 file per country)
setkey(out.udel, svyCode, hhid, month)

lbl <- c(
  "ArcGIS shape ID", "hhld ID", "hhld ID - alternate", "enumeration area",
  "latitude", "longitude", "survey code", "wave", "shape ID", "month",
  "precipitation (mm)",
  "temperature (C)")

tmp <- out.udel[svyCode %in% svy[1:3]]
attr(tmp, "var.labels") <- lbl
write.dta(tmp, "./out/2016.09/TZA-GPS-UDEL_1950-2014_monthly.dta",
  convert.factors="string", version=12L)

tmp <- out.udel[svyCode %in% svy[4:6]]
attr(tmp, "var.labels") <- lbl
write.dta(tmp, "./out/2016.09/UGA-GPS-UDEL_1950-2014_monthly.dta",
  convert.factors="string", version=12L)


#####################################################################################
## CHIRPS v2.0 Precipitation
# - extract `pre` from CHIRPS
# http://chg.geog.ucsb.edu/data/chirps/
#
# Climate Hazards Group InfraRed Precipitation with Station data (CHIRPS) is a 30+ year
# quasi-global rainfall dataset. Spanning 50°S-50°N (and all longitudes), starting in
# 1981 to near-present, CHIRPS incorporates 0.05° resolution satellite imagery with
# in-situ station data to create gridded rainfall time series for trend analysis and
# seasonal drought monitoring. As of February 12, 2015, version 2.0 of CHIRPS is
# complete and available to the public. For detailed information on CHIRPS, please
# refer to our paper in Scientific Data.

# Data for Africa available by month from FTP location
url <- "ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_monthly/bils/v2p0chirps"
tm <- seq(as.Date("1981-01-01"), as.Date("2016-08-01"), "month")
chirps <- lapply(tm, function(x) curl_download(
  paste0(url, format(x, "%Y%m"), ".tar.gz"),
  paste0("./CHIRPSv2/v2p0chirps", format(x, "%Y%m"), ".tar.gz")))

# Combine all into raster brick and save to netCDF for convenience
r <- lapply(chirps, untar)
chirps <- unlist(chirps)
file.remove(chirps)
r <- brick(as.list(gsub(".tar.gz", ".bil", chirps, fixed=T)))
# => note that loading these 428 rasters in memory is very slow, should use CDO instead:
for (i in chirps) {
  r <- raster(gsub(".tar.gz", ".bil", i, fixed=T))
  writeRaster(r, gsub(".tar.gz", ".nc", i, fixed=T), "CDF")
}
proj4string(r)
rm(r)

# CDO to combine all *.nc layers into one (takes less than 2 min)
system("cd /home/projects/hc-data/CHIRPSv2 && /opt/bin/cdo cat *.nc v2p0chirps.nc")

# Load CHIRPS from netCDF
chirps <- brick("./CHIRPSv2/v2p0chirps.nc")

# Verify
dim(chirps)
# [1] 1600 1500  428
res(chirps)
# [1] 0.05 0.05 => 5km
proj4string(chirps)
# +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0

# Values look a bit weird (negative), use setMinMax()
chirps <- setMinMax(chirps)
minValue(chirps[[1]])
# [1] -9999
maxValue(chirps[[1]])
# [1] 987

# Replace -9999 with NA
NAvalue(chirps) <- -9999
spplot(chirps[[1]])
# => OK looks good now

# Extract monthly precipitation
spplot(chirps[[100]])
tm_shape(chirps[[100]]) + tm_raster(n=8) +
  tm_shape(gps.pts, is.master=T) + tm_dots()

gps.pts <- spTransform(gps.pts, proj4string(chirps))
tmp <- extract(chirps, gps.pts)
tmp <- data.table(rn=row.names(gps.pts), tmp)
setnames(tmp, 2:429, format(tm, "%Y-%m-%d"))
tmp <- melt(tmp, id.vars=c("rn"), variable.name="month", value.name="pre_chirps", variable.factor=F)
tmp[, month := as.Date(month)]
out.chirps <- tmp

summary(out.chirps$pre_chirps)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.00   26.00   80.00   94.55  141.00 1185.00

# Where are the missing values?
rn.bad <- out.chirps[is.na(pre_chirps), .N, by=rn][N>200][, unique(rn)]
# => none

# Merge in survey and hhlds attributes
tmp <- data.table(gps.pts@data)
setkey(tmp, rn)
setkey(out.chirps, rn)
out.chirps <- tmp[out.chirps]

# Re-check unique `hhid`
out.chirps[, .(uniqueN(month), uniqueN(hhid), uniqueN(hhid_str), uniqueN(rn)), by=svyCode]
#    svyCode  V1   V2   V3   V4
# 1: tza2008 780 2806 2806 2806
# 2: tza2010 780 3917 3917 3917
# 3: tza2012 780 4988 4988 4988
# 4: uga2009 780 2951 2951 2951
# 5: uga2010 780 2671 2671 2671
# 6: uga2011 780 2761 2761 2761
# => OK


# Export to STATA (1 file per country)
setkey(out.chirps, svyCode, hhid, month)

lbl <- c(
  "ArcGIS shape ID", "hhld ID", "hhld ID - alternate", "enumeration area",
  "latitude", "longitude", "survey code", "wave", "shape ID", "month",
  "precipitation (mm) CHIRPS v2.0")

tmp <- out.chirps[svyCode %in% svy[1:3]]
attr(tmp, "var.labels") <- lbl
write.dta(tmp, "./out/2016.09/TZA-GPS-CHIRPS_1981-2016_monthly.dta",
  convert.factors="string", version=12L)

tmp <- out.chirps[svyCode %in% svy[4:6]]
attr(tmp, "var.labels") <- lbl
write.dta(tmp, "./out/2016.09/UGA-GPS-CHIRPS_1981-2016_monthly.dta",
  convert.factors="string", version=12L)



#####################################################################################
## SRTM Elevation
# simply use SRTM layers from `raster` library to extract elevation and slope
# maybe also add soil carbon content from CELL5M?

iso3 <- c("TZA", "UGA")
alt <- lapply(iso3, function(x) getData("alt", country=x, mask=F, path="./SRTM"))
tm_shape(alt[[1]][[1]]) + tm_raster()
names(alt) <- iso3

gps.pts <- spTransform(gps.pts, proj4string(alt[[1]]))
tmp <- list()
tmp[[1]] <- extract(alt[[1]], gps.pts[gps.pts$svyCode %in% svy[1:3],])
tmp[[2]] <- extract(alt[[2]], gps.pts[gps.pts$svyCode %in% svy[4:6],])
tmp[[1]] <- data.table(rn=gps.pts@data[gps.pts$svyCode %in% svy[1:3], "rn"], alt=tmp[[1]])
tmp[[2]] <- data.table(rn=gps.pts@data[gps.pts$svyCode %in% svy[4:6], "rn"], alt=tmp[[2]])
out.srtm <- rbindlist(tmp)
range(out.srtm$rn)

slope <- lapply(alt, terrain, opt=c("slope"), unit="degrees")
tmp[[1]] <- extract(slope[[1]], gps.pts[gps.pts$svyCode %in% svy[1:3],])
tmp[[2]] <- extract(slope[[2]], gps.pts[gps.pts$svyCode %in% svy[4:6],])
tmp[[1]] <- data.table(rn=gps.pts@data[gps.pts$svyCode %in% svy[1:3], "rn"], slope=tmp[[1]])
tmp[[2]] <- data.table(rn=gps.pts@data[gps.pts$svyCode %in% svy[4:6], "rn"], slope=tmp[[2]])
tmp <- rbindlist(tmp)
setkey(tmp, rn)
setkey(out.srtm, rn)
out.srtm$slope <- tmp[out.srtm][, slope]

# Verify
summary(out.srtm$alt)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#     3.0   486.0  1114.0   931.2  1244.0  2270.0      24

summary(out.srtm$slope)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#  0.0000  0.4350  0.8352  1.3300  1.5190 21.0500    1092


# Where are the missing values?
rn.bad <- out.srtm[is.na(alt), unique(rn)]
tm_shape(alt[[1]]) + tm_raster(n=8) +
  tm_shape(gps.pts[gps.pts$rn %in% rn.bad,], is.master=T) + tm_dots()
# => only TZA islands

imp <- gps.pts[!gps.pts$rn %in% rn.bad,]
bad <- gps.pts[gps.pts$rn %in% rn.bad,]
tmp <- knn(coordinates(imp), coordinates(bad), k=1)
tmp <- data.table(rn.bad=rn.bad, rn.imp=imp@data[tmp$nn.idx[,1], "rn"])

# Impute all
setkey(tmp, rn.imp)
setkey(out.srtm, rn)
tmp <- out.srtm[tmp]
setkey(tmp, rn.bad)
setkey(out.srtm, rn)
out.srtm[tmp, alt := tmp$alt]

# Also impute missing slope values
rn.bad <- out.srtm[is.na(slope), unique(rn)]
tm_shape(slope[[1]]) + tm_raster(n=8) +
  tm_shape(gps.pts[gps.pts$rn %in% rn.bad,], is.master=T) + tm_dots()
# => only TZA islands

imp <- gps.pts[!gps.pts$rn %in% rn.bad,]
bad <- gps.pts[gps.pts$rn %in% rn.bad,]
tmp <- knn(coordinates(imp), coordinates(bad), k=1)
tmp <- data.table(rn.bad=rn.bad, rn.imp=imp@data[tmp$nn.idx[,1], "rn"])

# Impute all
setkey(tmp, rn.imp)
setkey(out.srtm, rn)
tmp <- out.srtm[tmp]
setkey(tmp, rn.bad)
setkey(out.srtm, rn)
out.srtm[tmp, slope := tmp$slope]

# Merge in survey and hhld details
tmp <- data.table(gps.pts@data)
setkey(tmp, rn)
setkey(out.srtm, rn)
out.srtm <- tmp[out.srtm]

summary(out.srtm$alt)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#     3.0   486.0  1114.0   931.2  1244.0  2270.0

summary(out.srtm$slope)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.0000  0.4097  0.7606  1.2800  1.4990 21.0500

# Re-check unique `hhid`
out.srtm[, .(uniqueN(hhid), uniqueN(hhid_str), uniqueN(rn)), by=svyCode]
# s   vyCode   V1   V2   V3
# 1: tza2008 2806 2806 2806
# 2: tza2012 4988 4988 4988
# 3: uga2009 2951 2951 2951
# 4: uga2010 2671 2671 2671
# 5: uga2011 2761 2761 2761
# 6: tza2010 3917 3917 3917
# => OK

# Export
setkey(out.srtm, svyCode, hhid)

lbl <- c(
  "ArcGIS shape ID", "hhld ID", "hhld ID - alternate", "enumeration area",
  "latitude", "longitude", "survey code", "wave", "shape ID",
  "altitude (m)", "slope (degree)")

tmp <- out.srtm[svyCode %in% svy[1:3]]
attr(tmp, "var.labels") <- lbl
write.dta(tmp, "./out/2016.09/TZA-GPS-SRTM.dta",
  convert.factors="string", version=12L)

tmp <- out.srtm[svyCode %in% svy[4:6]]
attr(tmp, "var.labels") <- lbl
write.dta(tmp, "./out/2016.09/UGA-GPS-SRTM.dta",
  convert.factors="string", version=12L)


# ZIP all output files for GPS coords
system("cd /home/projects/hc-data/out/2016.09 && zip biovars_2016.09.28.zip TZA* UGA*")

# Save workspace
rm(i, j, x, r, tmp, id.bad, imp, bad, g2.dt, tza.imp, tza.bad, uga.imp, uga.bad,
  rn.bad, povmap, gps.ro, World, out.imp, out.bad, out.spei.imp)
save.image("./out/2016.09/svyL2Maps_r16.09.RData")


#####################################################################################
# Load Sara's TZA map to compare with Beliyou's

povmap <- shapefile("./Admin/2016.07/svyMaps_2016.06.24.shp")

tmap_mode("plot")

tm_shape(crop(pre[["X1371"]], tza)) +
  tm_raster(n=10, title="tza2012\nPrecipitation\n2014-03") +
  tm_shape(povmap[povmap$svyCode=="tza2012",]) + tm_borders() +
  tm_shape(povmap[povmap$svyCode=="tza2012",]) + tm_text("svyL2Nm", size=.6) +
  tm_layout(legend.outside=T)

tm_shape(g2[g2$svyCode=="tza2007",]) + tm_borders() +
  tm_shape(g2[g2$svyCode=="tza2007",]) + tm_text("svyL2Nm", size=.6) +
  tm_layout(legend.outside=T)

tm_shape(g2[g2$svyCode=="tza2011",]) + tm_borders() +
  tm_shape(g2[g2$svyCode=="tza2011",]) + tm_text("svyL2Nm", size=.5) +
  tm_layout(legend.outside=T)

tmp <- povmap@data[povmap$svyCode=="tza2012",]
tmp <- g2@data[g2$svyCode=="tza2007",]
tmp <- g2@data[g2$svyCode=="tza2011",]

