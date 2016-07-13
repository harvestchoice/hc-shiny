#####################################################################################
# Title:   Map Urban Population Hotspots 2000-2020 or 1990-2015
# Date:    March 2016
# Project: SDA for PIM
# Author:  Bacou, Melanie <mel@mbacou.com>
#####################################################################################

# I would like to use my one slide to argue that most urbanization in Africa is in areas
# on or near coasts. The geographic location of urbanization coupled with declining
# shipping costs creates a major problem for competitiveness of African producers (who
# are in the hinterland), but no real problem for food security in urban areas—imports
# are available at declining costs.
# Could you produce the map showing the location of change in population over the past
# 25 years in Africa south of the Sahara? Maybe show big bubbles where growth is fast
# and smaller ones where it is slow? You probably will be limited to population of
# cities, since I doubt that you have the population data in a grid that allows you to
# pick up non-urban areas.

# Load admin center points from GPWv4, downloaded from:
# http://beta.sedac.ciesin.columbia.edu/data/set/gpw-v4-admin-unit-center-points-population-estimates/data-download
# Use Joe's 2010 urban mask to classify urban/rural admin units, from:
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/RUNZJD
# Compute 20-year trend and map hotspots (bubble size=population, color
# intensity=change)

# GPWv4 consists of estimates of UN-adjusted population estimates and densities for the
# years 2000, 2005, 2010, 2015, and 2020 by administrative unit center point
# (centroid). Specifically, the population data were adjusted to the 2015 Revision of
# the UN World Population Prospects. Additionally, the administrative unit names, the
# unit area, and the data context of the unit are included. The data are stored in
# geographic coordinates of decimal degrees based on the World Geodetic System
# spheroid of 1984 (WGS84).

library(data.table)
library(rgdal)
library(rgeos)
library(tmap)
library(raster)
library(ggvis)
library(hcapi3)

setwd("~/Projects/hc-data")
load("./out/popTrends/popTrends.RData")

# SSA country boundaries for plotting
data(World)
World <- World[World$continent=="Africa",]

# Load GPWv4
gpw <- curl_download(
  "http://beta.sedac.ciesin.columbia.edu/downloads/data/gpw-v4/gpw-v4-admin-unit-center-points-population-estimates/gpw-v4-admin-unit-center-points-population-estimates-csv-global.zip",
  "./GPWv4/gpw-v4-admin-unit-center-points-population-estimates-csv-global.zip")
gpw <- unzip(gpw, exdir="./data")
gpw <- fread(gpw[1])

# Keep global layer for re-use
save(gpw, file="./out/popTrends/gpw-v4-admin-unit-center-points-population-estimates-csv-global.rds")

# Load Joe's 2010 urban mask
urb <- raster("./UrbanMask_SSA_2010/UrbanMask_SSA_2010/UrbanExtentAfrica2010_v1.tif")

# Load SSA from hcapi3, keep only SSA
gpw <- gpw[ISOALPHA %in% iso]
gpw[, unique(ISOALPHA)]
# [1] "AGO" "BDI" "BEN" "BFA" "BWA" "CAF" "CIV" "CMR" "COD" "COG" "COM" "CPV" "DJI" "ERI" "ESH"
# [16] "ETH" "GAB" "GHA" "GIN" "GMB" "GNB" "GNQ" "KEN" "LBR" "LSO" "MDG" "MLI" "MOZ" "MRT" "MUS"
# [31] "MWI" "MYT" "NAM" "NER" "NGA" "REU" "RWA" "SDN" "SEN" "SHN" "SLE" "SOM" "STP" "SWZ" "SYC"
# [46] "TCD" "TGO" "TZA" "UGA" "ZAF" "ZMB" "ZWE"


# Count admin units used by country in SSA
gpw[, .N, by=ISOALPHA][N < 100][order(-N)]
#     ISOALPHA  N
#  1:      ZWE 91
#  2:      LSO 80
#  3:      BEN 77
#  4:      SOM 74
#  5:      NER 66
#  6:      TCD 62
#  7:      CMR 58
#  8:      SWZ 55
#  9:      GAB 48
# 10:      SEN 45
# 11:      GMB 40
# 12:      TGO 40
# 13:      GNB 39
# 14:      ESH 31
# 15:      GNQ 30
# 16:      BWA 29
# 17:      SYC 26
# 18:      REU 24
# 19:      CPV 22
# 20:      MYT 17
# 21:      SHN 13
# 22:      COG 12
# 23:      STP  7
# 24:      DJI  6
# 25:      ERI  6
# 26:      COM  3

# => Looks acceptable

# Verify totals
summary(gpw$UN_2010_E)
gpw[, sum(UN_2010_E, na.rm=T), by=ISOALPHA][order(-V1)]
# => Ok, matches WB totals

# Classify GPW admin units (rural-0/urban-1)
gpw.dt <- gpw
gpw <- SpatialPointsDataFrame(gpw.dt[, .(INSIDE_X, INSIDE_Y)], data.frame(gpw.dt),
  proj4string=CRS("+init=epsg:4326"))
gpw.dt[, rn := row.names(gpw)]

# Map it to verify
tm_shape(gpw) + tm_bubbles(alpha=.2)



# Overlay with Joe's urban mask and assign predominantly urban units to GPW
minValue(urb)
# [1] -2147483648
maxValue(urb)
# [1] 2147483647
res(urb)
# [1] 0.008333333 0.008333333
proj4string(urb)
# [1] "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Clean up
urb <- calc(urb, function(x) ifelse(is.na(x) | x<0, 0, 1))

# Mask out of SSA boundaries
urb <- mask(urb, g2)

# Reclassify
levels(urb) <- data.frame(ID=c(0,1), class=c("rural", "urban"))

# Verify
tmap_mode("plot")
qtm(urb, raster="UrbanExtentAfrica2010_v1")

# Save for re-use
writeRaster(urb, "./out/popTrends/UrbanExtentAfrica2010_v1_class.tif",
  colorTables=list(c("green", "orange")),
  catNames=list(c("rural", "urban")))

#urb <- raster("./out/popTrends/UrbanExtentAfrica2010_v1_class.tif")


# Extract urban admin units
# Note that buffer() doesn't work with beginCluster()
gpw.15km <- buffer(gpw, width=15*1000, dissolve=F)
tmp.buff <- extract(urb, gpw.15km, fun=mean, na.rm=T)
tmp.pts <- extract(urb, gpw)

# Could also identify urban admin units based on density, using WB SSA urban rates
# Estimated at 37% in 2014
gpw.dt[, sum(ifelse(UN_2015_DS > 357, UN_2015_E, 0), na.rm=T)/sum(UN_2015_E, na.rm=T)]
# [1] 0.3703164


# Add growth rates
gpw.dt[, URBAN := UN_2015_DS > 357]
gpw.dt[, URBAN_PTS := tmp.pts > 0]
gpw.dt[, GROWTH_20 := UN_2020_E-UN_2000_E]
gpw.dt[, GROWRATE_20 := (UN_2020_E-UN_2000_E)/UN_2000_E]

summary(gpw.dt$URBAN)
#    Mode   FALSE    TRUE    NA's
# logical   41585   81462    2133

summary(gpw.dt$URBAN_PTS)
#    Mode   FALSE    TRUE    NA's
# logical   54590   70164     426

# What are the NA units?
gpw.dt[is.na(URBAN), .(ISOALPHA, NAME1, CONTEXT_NM)]
gpw.dt[is.na(URBAN), .N, keyby=CONTEXT_NM]
#                                                  CONTEXT_NM    N
# 1:                                                             1
# 2: Military district, airport zone, or other infrastructure    1
# 3:                 Not enumerated or not reported in census    5
# 4:                                   Park or protected area   27
# 5:                                   Population not gridded    1
# 6:                                              Uninhabited 2098

# Just convert all NAs to "rural"
gpw.dt[is.na(URBAN), URBAN := F]
gpw.dt[is.na(URBAN_PTS), URBAN_PTS := F]

# Also map urban units with missing data
miss <- gpw.dt[is.na(GROWTH_20) & URBAN_PTS==T,]
miss <- which(is.na(gpw.dt$GROWTH_20) & gpw.dt$URBAN_PTS==T)
tmap_mode("view")
qtm(gpw[miss,]) + tm_bubbles()

# Correct URBAN_PTS
gpw.dt[is.na(GROWTH_20) & URBAN_PTS==T, URBAN_PTS := F]

# If zero baseline then set 100% growth rate
gpw.dt[UN_2000_E==0, GROWRATE_20 := 1]

summary(gpw.dt[, GROWRATE_20])
# => what are these units with crazy growth rates?
gpw.dt[GROWRATE_20>10, .N, by=ISOALPHA]
#    ISOALPHA  N
# 1:      AGO 11
# 2:      COG  1
# 3:      MLI  4
# 4:      MRT  1
# 5:      MWI  6
# 6:      ZMB  1

summary(gpw.dt[URBAN==T, GROWRATE_20])
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# -0.6      0.1      0.3      7.2      0.6 553900.0 0

gpw.dt[GROWRATE_20>20, .(NAME2, GROWRATE_20)]
# NAME2  GROWRATE_20
# 1:             Cacuaco     25.29635
# 2:             Cazenga     25.29623
# 3:              Luanda     25.29634
# 4:               Belas     25.29626
# 5:               Viana     25.29626
# 6: Lake Malawi N. Park 553937.33333

# Fix Lake Malawi, NAME3==30111909 -- looks like an error (density is way too high)
# Correspond to Nkope on Lake Malawi
tmap_mode("view")
tm_shape(gpw[19474,]) + tm_dots()

gpw.dt[NAME3=="30111909", GROWRATE_20 := 1]
gpw.dt[ISOALPHA=="MWI", sum(UN_2020_E, na.rm=T)]
# [1] 20022316

# Correct empty strings
gpw.dt[COUNTRYNM=="", COUNTRYNM := NA]
gpw.dt[NAME1=="", NAME1 := NA]
gpw.dt[NAME2=="", NAME2 := NA]
gpw.dt[NAME3=="", NAME3 := NA]


#####################################################################################
# Share of rural/urban population within 150km of coastline
# Load Africa coastline
coast <- curl_download(
  "http://omap.africanmarineatlas.org/BASE/data/coast/vmap0/africa_coastline_vmap0.zip",
  "./GPWv4/africa_coastline_vmap0.zip")
coast <- unzip(coast, exdir="./GPWv4")
coast <- readOGR("./GPWv4", "africa_coastline_vmap0")
proj4string(coast)
# [1] "+proj=longlat +datum=NAD27 +no_defs +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat"

# Reproject to equidistant (for gDistance(), but not for dist2Line())
coast <- spTransform(coast, CRS("+init=epsg:3786"))
gpw <- spTransform(gpw, proj4string(coast))

# Map these layers to check
tmap_mode("plot")
tm_shape(gpw[gpw$ISOALPHA=="GHA",], is.master=T, projection="merc") + tm_bubbles() +
  tm_shape(coast) + tm_lines() + tm_scale_bar(width=0.8)


# Mark units within 150km of coastline
dist150k <- sapply(row.names(gpw), function(x) gWithinDistance(gpw[x,], coast, 150*1000))
gpw.dt[, DIST150km := dist150k]

# Within 80km of coastline
# Re-compute coastal distance (Karen suggests 50km?)
# Try to use geosphere::dist2Line instead
# dist <- dist2Line(gpw, coast)
# => way too slow

# Use 4 cores to speed up above
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

clusterExport(cl, c("gpw", "coast"))
clusterEvalQ(cl, library(rgeos))
rn.split <- clusterSplit(cl, row.names(gpw))

# gWithinDistance 80km
m <- clusterApply(cl, x=rn.split,
  fun=function(x) sapply(x, function(i) gWithinDistance(gpw[i,], coast, 80*1000)))

stopCluster(cl)
dist80k <- do.call(c, m)
rm(m)
gpw.dt[, DIST80k := dist80k]


# Correct integer overflow
gpw.dt[, `:=`(
  UN_2000_E=as.numeric(UN_2000_E),
  UN_2005_E=as.numeric(UN_2005_E),
  UN_2010_E=as.numeric(UN_2010_E),
  UN_2015_E=as.numeric(UN_2015_E),
  UN_2020_E=as.numeric(UN_2020_E))]

# Save into `gpw` as well
setkey(gpw.dt, rn)
gpw@data <- data.frame(gpw.dt[row.names(gpw)])



#####################################################################################
# Make ISO3 list for UI
iso <- hcapi3::iso
iso <- iso[order(names(iso))]
iso <- iso[iso %in% c("SSA", unique(gpw.dt$ISOALPHA))]
iso <- iso[iso != "SYC"]


#####################################################################################
# Summarize by country - version 1
gpw.sum1 <- gpw.dt[, lapply(.SD, sum, na.rm=T),
  .SDcols=names(gpw.dt) %like% "_E",
  keyby=.(ISOALPHA, URBAN, DIST150km)]
gpw.sum1[, GROWTH_20 := UN_2020_E-UN_2000_E]
gpw.sum1[, GROWRATE_20 := (UN_2020_E-UN_2000_E)/UN_2000_E]

# Add SSA row
tmp <- gpw.dt[, lapply(.SD, sum, na.rm=T),
  .SDcols=names(gpw.dt) %like% "_E",
  keyby=.(URBAN, DIST150km)]
tmp[, GROWTH_20 := UN_2020_E-UN_2000_E]
tmp[, GROWRATE_20 := (UN_2020_E-UN_2000_E)/UN_2000_E]
tmp[, ISOALPHA := "SSA"]
gpw.sum1 <- rbind(gpw.sum1, tmp)

# Summarize by country - version 2
gpw.sum2 <- gpw.dt[, lapply(.SD, sum, na.rm=T),
  .SDcols=names(gpw.dt) %like% "_E",
  keyby=.(ISOALPHA, URBAN_PTS, DIST150km)]
gpw.sum2[, GROWTH_20 := UN_2020_E-UN_2000_E]
gpw.sum2[, GROWRATE_20 := (UN_2020_E-UN_2000_E)/UN_2000_E]

# Add SSA row
tmp <- gpw.dt[, lapply(.SD, sum, na.rm=T),
  .SDcols=names(gpw.dt) %like% "_E",
  keyby=.(URBAN_PTS, DIST150km)]
tmp[, GROWTH_20 := UN_2020_E-UN_2000_E]
tmp[, GROWRATE_20 := (UN_2020_E-UN_2000_E)/UN_2000_E]
tmp[, ISOALPHA := "SSA"]
gpw.sum2 <- rbind(gpw.sum2, tmp)


# Urban rates - SSA
urb.rate1 <- gpw.sum1[, lapply(.SD,
  function(x) sum(URBAN*x, na.rm=T)/sum(x, na.rm=T)),
  .SDcols=c(2,4:8)]
urb.rate1[, URBAN := NULL]

urb.rate2 <- gpw.sum2[, lapply(.SD,
  function(x) sum(URBAN_PTS*x, na.rm=T)/sum(x, na.rm=T)),
  .SDcols=c(2,4:8)]
urb.rate2[, URBAN_PTS := NULL]

# Urban rates - coastal
urb.rate1.c <- gpw.sum1[, lapply(.SD,
  function(x) sum(URBAN*x, na.rm=T)/sum(x, na.rm=T)),
  by=.(ISOALPHA, DIST150km), .SDcols=c(2,4:8)]
urb.rate1.c[, URBAN := NULL]

urb.rate2.c <- gpw.sum2[, lapply(.SD,
  function(x) sum(URBAN_PTS*x, na.rm=T)/sum(x, na.rm=T)),
  by=.(ISOALPHA, DIST150km), .SDcols=c(2,4:8)]
urb.rate2.c[, URBAN_PTS := NULL]




#####################################################################################
# GPWv4 Population Rasters
#####################################################################################
setwd("~/Projects/hc-data")
load("./out/popTrends/popTrends.RData")

# Good enough to run the analysis at 10km resolution
# Load CELL5M-SSA grid
load("./CELL5M/cell5m.rda")
grid <- SpatialPixelsDataFrame(dt[, .(X,Y)], data.frame(dt[, .(CELL5M, ISO3, X, Y)]),
  proj4string=CRS("+init=epsg:4326"))
rm(dt)
plot(raster(grid, layer="ISO3"))

# Get summaries using original raster layers instead of admin centerpoints
gp <- c(
  "./GPWv4/gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals_2000.tif",
  "./GPWv4/gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals_2005.tif",
  "./GPWv4/gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals_2010.tif",
  "./GPWv4/gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals_2015.tif",
  "./GPWv4/gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals_2020.tif")

gp <- stack(gp)
res(gp)
# [1] 0.008333333 0.008333333
res(brick(grid))
# [1] 0.08333333 0.08333333
names(gp) <- c("gpw2000", "gpw2005", "gpw2010", "gpw2015", "gpw2020")

# We also need land and water areas to re-generate densities at 10km
gpl <- c(
  "./GPWv4/gpw-v4-land-water-area_land.tif",
  "./GPWv4/gpw-v4-land-water-area_water.tif")

gpl <- stack(gpl)
res(gpl)
names(gpl) <- c("land", "water")

gp <- crop(gp, grid)
extent(gp)
# class       : Extent
# xmin        : -25.5
# xmax        : 72.58333
# ymin        : -47.08333
# ymax        : 27.75

minValue(gp)
# [1] 0 0 0 0 0
maxValue(gp)
#[1]  97064.98  76170.12  90288.20 106393.51 940130.44
plot(raster(gp, "gpw2000"))

gpl <- crop(gpl, grid)
extent(gpl)

minValue(gpl)
# [1] 0 0
maxValue(gpl)
# [1] 0.8605588 0.8610033

plot(gpl)
# Note that the land raster is total area minus water bodies

# These layers are at 1km resolution, let's resample all to CELL5M grid to speed things up
gp <- aggregate(gp, fact=10, fun=sum, na.rm=T, filename="./CELL5M/cell5m_gpw-v4-SSA.tif")
res(gp)
gp.dt <- extract(gp, grid)
gp.dt <- as.data.table(gp.dt)
setnames(gp.dt, c("gpw2000", "gpw2005", "gpw2010", "gpw2015", "gpw2020"))
gp.dt <- cbind(as.data.table(grid@data), gp.dt)
gp.dt[, prettyNum(sum(gpw2015, na.rm=T)), keyby=ISO3]

gpl <- aggregate(gpl, fact=10, fun=sum, na.rm=T, filename="./CELL5M/cell5m_gpw-v4-land-SSA.tif")
res(gpl)
gpl.dt <- extract(gpl, grid)
gpl.dt <- as.data.table(gpl.dt)
setnames(gpl.dt, c("land", "water"))
gp.dt <- cbind(gp.dt, gpl.dt)
rm(gpl.dt)

summary(gp.dt$land)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#    0.00   81.01   83.60   81.43   85.26   86.06    5147

summary(gp.dt$gpw2000)
#   Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's
#    0.0      72.7     505.3    2254.0    1692.0 1881000.0      6058


# Note that Tom Hengl has a 5km distance to coastline raster ready made at
# http://spatial-analyst.net/worldmaps/dcoast.zip
# Units are in degrees not km
dcoast <- curl_download("http://spatial-analyst.net/worldmaps/dcoast.zip", "./Worldgrids/dcoast.zip")
dcoast <- unzip(dcoast, exdir="./Worldgrids")
dcoast <- raster(dcoast[2])
#dcoast <- raster("./Worldgrids/dcoast.tif")
dcoast <- crop(dcoast, grid)

extent(dcoast)
res(dcoast)
# [1] 0.05 0.05 => 5km
res(gp)
# [1] 0.008333333 0.008333333 => 1km
plot(dcoast)

dcoast <- aggregate(dcoast, fact=2, fun=mean, na.rm=T, filename="./CELL5M/cell5m_dcoast-SSA.tif", overwrite=T)
res(dcoast)
dcoast.dt <- extract(dcoast, grid)
dcoast.dt <- as.data.table(dcoast.dt)
setnames(dcoast.dt, "dcoast")

gp.dt <- cbind(gp.dt, dcoast.dt)
rm(dcoast.dt)
summary(gp.dt$dcoast)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# -4.098   2.345   5.785   6.230   9.735  16.620

# Use 8x 5-arc-minute pixels to approximate 80km, i.e. 0.08333333*8
km80 <- 0.08333333*8
gp.dt[, dcoast80km := ifelse(dcoast <= km80, "coastal", "inland")]

# Map it
tmp <- SpatialPixelsDataFrame(gp.dt[, .(X,Y)], data.frame(gp.dt),
  proj4string=CRS("+init=epsg:4326"))

tm_shape(tmp) + tm_raster("dcoast80km")
# => looks good


# Make densities, note that GPW4 land and water layers are in sq. km.
gp.dt[, `:=`(
  gpw2000dens=gpw2000/land,
  gpw2005dens=gpw2005/land,
  gpw2010dens=gpw2010/land,
  gpw2015dens=gpw2015/land,
  gpw2020dens=gpw2020/land)]


# Get World Bank urbanization rates for 2000
# Classify urban areas country by country
wb <- fread("./out/popTrends/wb_urbanization.csv")
setnames(wb, 5:9, c("Y1990", "Y2000", "Y2005", "Y2010", "Y2015"))
setnames(wb, "Country Code", "ISO3")
wb <- wb[`Series Code` != ""]
wb[, `:=`(
  Y1990=as.numeric(Y1990),
  Y2000=as.numeric(Y2000),
  Y2005=as.numeric(Y2005),
  Y2010=as.numeric(Y2010),
  Y2015=as.numeric(Y2015))]

gp.dt[!ISO3 %in% wb$ISO3, unique(ISO3)]
#  [1] "ESH" "XX0" "XX1" "COD" "XX2" "IOT" "SHN" "ATF" "MYT" "REU"
# British Indian Ocean Territory
# French Southern Territories
# Mayotte
# Saint Helena

# Let's merge and apply standard urbanization rates
setkey(wb, ISO3)
setkey(gp.dt, ISO3)
gp.dt$urbrate2000 <- wb[gp.dt][, Y2000]

# Recode missing
gp.dt[ISO3=="ESH", urbrate2000 := wb[ISO3=="MAR", Y2000]]
gp.dt[ISO3 %in% c("REU", "IOT", "ATF", "MYT", "SHN"),
  urbrate2000 := wb[ISO3=="MDG", Y2000]]
gp.dt[ISO3=="COD", urbrate2000 := wb[ISO3=="ZAR", Y2000]]

gp.dt[is.na(urbrate2000), .N, by=ISO3]
# ISO3   N
# 1:  XX0 239
# 2:  XX1  24
# 3:  XX2  43

tm_shape(tmp[tmp$ISO3 %like% "XX",]) + tm_raster("gpw2000")
gp.dt[ISO3 %like% "XX", urbrate2000 := wb[ISO3=="SSD", Y2000]]


# Classify urban areas based on 2000 urbanization rates
setorder(gp.dt, ISO3, -gpw2000dens)
gp.dt[, urban := cumsum(ifelse(is.na(gpw2000), 0, gpw2000))/sum(gpw2000, na.rm=T), by=ISO3]
gp.dt[, urban := ifelse(urban < urbrate2000/100, "urban", "rural")]
gp.dt[, sum(ifelse(urban=="urban", gpw2000, 0), na.rm=T)/sum(gpw2000, na.rm=T), keyby=ISO3]
# => looks good
gp.dt[urban=="urban", urbdens := min(gpw2000dens, na.rm=T), by=ISO3]
gp.dt[, urbdens := mean(urbdens, na.rm=T), by=ISO3]
gp.dt[, `:=`(
  urb2000=ifelse(gpw2000dens >= urbdens, "urban", "rural"),
  urb2005=ifelse(gpw2005dens >= urbdens, "urban", "rural"),
  urb2010=ifelse(gpw2010dens >= urbdens, "urban", "rural"),
  urb2015=ifelse(gpw2015dens >= urbdens, "urban", "rural"),
  urb2020=ifelse(gpw2020dens >= urbdens, "urban", "rural"))]

gp.dt[urb2000!=urban, .N, by=ISO3]
# Note that curiously using this method there are urban areas that move into rural areas

gp.dt[, sum(urb2000=="urban", na.rm=T)/.N, by=ISO3]
gp.dt[, sum(urb2020=="urban", na.rm=T)/.N, by=ISO3]

# Add abs. pop gain/loss and growth rate
gp.dt[, growrate_20 := ifelse(gpw2000==0 & gpw2020>0, 1, (gpw2020-gpw2000)/gpw2000)]
gp.dt[, growth_20 := gpw2020-gpw2000]

gp.dt[, mean(growrate_20*100, na.rm=T), by=.(dcoast80km, urban)]

# Export for reuse
saveRDS(gp.dt, file="./out/popTrends/gpw-v4-dt.rds")


# Summarize across countries for charting
tmp <- gp.dt[, .(gpw2000=sum(gpw2000, na.rm=T)), keyby=.(ISO3, dcoast80km, urb2000)]
tmp2 <- gp.dt[, .(gpw2005=sum(gpw2005, na.rm=T)), keyby=.(ISO3, dcoast80km, urb2005)]
tmp3 <- gp.dt[, .(gpw2010=sum(gpw2010, na.rm=T)), keyby=.(ISO3, dcoast80km, urb2010)]
tmp4 <- gp.dt[, .(gpw2015=sum(gpw2015, na.rm=T)), keyby=.(ISO3, dcoast80km, urb2015)]
tmp5 <- gp.dt[, .(gpw2020=sum(gpw2020, na.rm=T)), keyby=.(ISO3, dcoast80km, urb2020)]

setnames(tmp, 3:4, c("urban", "PN"))
setnames(tmp2, 3:4, c("urban", "PN"))
setnames(tmp3, 3:4, c("urban", "PN"))
setnames(tmp4, 3:4, c("urban", "PN"))
setnames(tmp5, 3:4, c("urban", "PN"))

tmp[, year := 2000]
tmp2[, year := 2005]
tmp3[, year := 2010]
tmp4[, year := 2015]
tmp5[, year := 2020]

gp.dt.iso3 <- rbind(tmp, tmp2, tmp3, tmp4, tmp5)
rm(tmp, tmp2, tmp3, tmp4, tmp5)

# Add total SSA
tmp <- gp.dt.iso3[, .(PN=sum(PN, na.rm=T)), by=.(dcoast80km, urban, year)]
tmp[, ISO3 := "SSA"]
gp.dt.iso3 <- rbind(gp.dt.iso3, tmp)

# Graphs
data.frame(gp.dt.iso3[!is.na(urban) & ISO3=="SSA"]) %>%
  ggvis(~factor(year), ~PN) %>%
  add_axis("x", title="", properties=ap) %>%
  add_axis("y", title="", format=".2s", properties=ap) %>%
  group_by(urban, dcoast80km) %>%
  layer_points(shape=~urban, fill=~dcoast80km, stroke:="#fff") %>%
  layer_lines(stroke=~dcoast80km) %>%
  scale_ordinal("stroke", range=pal) %>%
  scale_ordinal("fill", range=pal) %>%
  hide_legend("stroke") %>%
  add_legend("shape", title="Urban/Rural") %>%
  add_legend("fill", title="Location", properties=legend_props(legend=list(y=60))) %>%
  set_options(height=320, width="auto", resizable=F, duration=0)




#####################################################################################
# Save 1km GPWv4 pop counts for SSA for re-use
gp <- mask(gp, World, filename="./out/popTrends/gpw-v4-SSA.tif")
#gp <- raster("./out/popTrends/gpw-v4-SSA.tif")

# Rasterize coastline
coast <- spTransform(coast, proj4string(gp))
gp.coast <- rasterize(coast, gp[[1]], 1, fun="first", datatype="INT2S",
  filename="./out/popTrends/coastline-SSA.tif", overwrite=T)

minValue(gp.coast)
# [1] 1
maxValue(gp.coast)
# [1] 1
extent(gp.coast)
plot(gp.coast)

# Distance to nearest cell that's not NA
gp.dist <- distance(gp.coast, filename="./out/popTrends/coastline-distance-SSA.tif")
# => way too slow, quit


#####################################################################################
# Adjust South Africa and Namibia granularity
#####################################################################################
# In these countries the data comes at admin-3 or admin-4, so much smaller admin
# unit sizes that would crowd the print map too much. We need to cluster them up.

# Are there other countries with admin-3 level estimates?
gpw.dt[, length(unique(NAME3)), by=ISOALPHA][order(-V1)]
#    ISOALPHA    V1
# 1:      MWI 12645
# 2:      NAM  5475
# 3:      TZA  3387

gpw.dt[, .N, by=ISOALPHA][order(-N)]



#####################################################################################
# Maps (using urban def. #2)
#####################################################################################

# Map - Version 1
gpw.urb1 <- gpw.dt[URBAN==T]
setorder(gpw.urb1, -UN_2015_E)
gpw.urb1 <- SpatialPointsDataFrame(gpw.urb1[, .(INSIDE_X, INSIDE_Y)],
  data.frame(gpw.urb1), proj4string=CRS("+init=epsg:4326"))
gpw.urb1 <- gpw.urb1[, c(4:8,33,36,37,12:28,34:35)]

# Map - Version 2
gpw.urb2 <- gpw.dt[URBAN_PTS==T]
setorder(gpw.urb2, -UN_2015_E)
gpw.urb2 <- SpatialPointsDataFrame(gpw.urb2[, .(INSIDE_X, INSIDE_Y)],
  data.frame(gpw.urb2), proj4string=CRS("+init=epsg:4326"))
gpw.urb2 <- gpw.urb2[, c(4:8,33,36,37,12:28,34:35)]


# Print map
tmap_mode("plot")

pmap <- tm_shape(World, projection="robin") +
  tm_polygons(col="#463144", border.col="#997DB4") +
  tm_text("name", size="AREA", col="#FFFFFF") +
  tm_shape(gpw.urb2, is.master=T) +
  tm_bubbles(col="GROWRATE_20", size="UN_2015_E", scale=1.2,
    breaks=c(-20,0,.15,.3,.45,.6,.75,.9,1,26),
    labels=c("pop. loss", " 0 - 15", "15 - 30", "30 - 45", "45 - 60", "60 - 75", "75 - 90", "90 - 100", "above 100%"),
    palette="-Spectral", border.col="#463144", border.lwd=0.2,
    title.col="Pop. Gain/Loss \n2000-2020 Projected \n(percent)",
    title.size="Urban Population Hotspots, 2015") +
  tm_credits("Source: GPWv4. \nIFPRI/HarvestChoice, 2016.", col="#463144") +
  tm_layout(bg.color="#3F448E",
    legend.position=c("LEFT", "BOTTOM"),
    legend.bg.color="white", legend.frame="#463144")

save_tmap(pmap, "./www/popTrends_SSA_2000-2020.png", width=6, pointsize=10)


# Leaflet map
tmap_mode("view")
gpw.urb2.m <- gpw.urb2[gpw.urb2$UN_2015_E > 2000,] # | gpw.urb2$UN_2015_DS >= 50000

m <- tm_shape(gpw.urb2.m) +
  tm_bubbles(col="GROWRATE_20", size="UN_2015_E", scale=1,
    palette="-Spectral", border.col="white", border.lwd=0.4,
    breaks=c(-20,0,.15,.3,.45,.6,.75,.9,1,26),
    labels=c("pop. loss", " 0 - 15", "15 - 30", "30 - 45", "45 - 60", "60 - 75", "75 - 90", "90 - 100", "above 100% gain"),
    title.col="Urban Population Gain/Loss <br/><small>2000-2020 Projected (percent)</small>",
    title.size="Urban Population Hotspots, 2015") +
  tm_credits("Source: GPWv4. \nIFPRI/HarvestChoice, 2016.") +
  tm_layout(legend.position=c("LEFT", "BOTTOM"),
    basemaps=c("Thunderforest.TransportDark", "Esri.WorldGrayCanvas")) +
  tm_view(alpha=0.9, popup.all.data=T, bubble.size.fixed=T)

m <- tmap_leaflet(m)


#####################################################################################
# Data Downloads
#####################################################################################

attr(gpw.urb2.m@data, "var.labels") <- c(
  "3-letter ISO country code",
  "country name",
  "admin level-1 name",
  "admin level-2 name",
  "admin level-3 name",
  "urban area - density below 357 pp/sq. km.",
  "urban area - per 2010 urban mask",
  "is coastal area?",
  "admin long.",
  "admin lat.",
  "admin long. - inside area",
  "admin lat. - inside area",
  "area sq. km.",
  "water area, sq. km.",
  "land area, sq. km.",
  "2000 pop. headcount",
  "2005 pop. headcount",
  "2010 pop. headcount",
  "2015 projected pop. headcount",
  "2020 projected pop. headcount",
  "2000 pop. density, sq. km.",
  "2005 pop. density, sq. km.",
  "2010 pop. density, sq. km.",
  "2015 projected pop. density, sq. km.",
  "2020 projected pop. density, sq. km.",
  "2000-2020 pop. gain/loss",
  "2000-2020 pop. growth rate")

write.csv(gpw.urb2.m@data, "./www/popTrends_SSA_2000-2020.csv", na="", row.names=F)
write.dta(gpw.urb2.m@data, "./www/popTrends_SSA_2000-2020.dta", version=12L)
writeOGR(gpw.urb2.m, "./www", "popTrends_SSA_2000-2020", "ESRI Shapefile", overwrite=T)



#####################################################################################
# Charts (using urban def. #2 and 80km cutoff)
#####################################################################################
# Theme
ap <- axis_props(
  axis=list(stroke="transparent"),
  title=list(fill="#444", font="Pt Sans", fontSize=14),
  labels=list(fill="#444", font="Pt Sans", fontSize=12),
  ticks=list(stroke="transparent"),
  grid=list(stroke="#e3e3e3", strokeWidth=1))

pal <- c("#6EB89A", "#E05D58")

# Tooltips
tt <- function(x) {
  if(is.null(x)) return()
  paste(sapply(x, format), collapse="<br />")
}


# p1 - Pop trends x 4
p1 <- function(iso3="SSA") {

  d <- melt(gpw.sum2, id.vars=1:3, measure.vars=4:8)
  levels(d$variable)[1:5] <- c(2000, 2005, 2010, 2015, 2020)
  if(iso3 != "SSA") d <- d[ISOALPHA==iso3]
  d <- d[, .(value=sum(value, na.rm=T)), by=.(URBAN_PTS, DIST150km, variable)]
  d[, URBAN_PTS := factor(URBAN_PTS, levels=c(F,T), labels=c("rural", "urban"))]
  d[, DIST150km := factor(DIST150km, levels=c(F,T), labels=c("hinterland", "coast"))]

  p <- data.frame(d) %>%
    ggvis(~variable, ~value) %>%
    add_axis("x", title="", properties=ap) %>%
    add_axis("y", title="", format=".2s", properties=ap) %>%
    group_by(URBAN_PTS, DIST150km) %>%
    layer_points(shape=~URBAN_PTS, fill=~DIST150km, stroke:="#fff") %>%
    layer_lines(stroke=~DIST150km) %>%
    scale_ordinal("stroke", range=pal) %>%
    scale_ordinal("fill", range=pal) %>%
    hide_legend("stroke") %>%
    add_legend("shape", title="Urban/Rural") %>%
    add_legend("fill", title="Location", properties=legend_props(legend=list(y=60))) %>%
    set_options(height=320, width="auto", resizable=F, duration=0)
  #add_tooltip(tt, on="hover")

  return(p)
}


# p2 - Urbanization rates x 2
p2 <- function(iso3="SSA") {

  d <- melt(urb.rate2.c, id.vars=1:2)
  d <- d[ISOALPHA==iso3]
  levels(d$variable) <- c(2000, 2005, 2010, 2015, 2020)
  d[, DIST150km := factor(DIST150km, levels=c(F,T), labels=c("hinterland", "coast"))]

  p <- data.frame(d) %>%
    ggvis(~variable, ~value) %>%
    add_axis("x", title="", properties=ap) %>%
    add_axis("y", title="", format="%", properties=ap) %>%
    group_by(DIST150km) %>%
    layer_points(fill=~DIST150km, stroke:="#fff") %>%
    layer_lines(stroke=~DIST150km) %>%
    scale_ordinal("stroke", range=pal) %>%
    scale_ordinal("fill", range=pal) %>%
    hide_legend("stroke") %>%
    add_legend("fill", title="Location") %>%
    set_options(height=320, width="auto", resizable=F) %>%
    add_tooltip(tt, on="hover")

  return(p)

}

# p3 - Top urban areas and growth rate in/outside coastal areas
p3 <- function(iso3="SSA") {

  d <- gpw.dt[, .SD, .SDcols=c(4:7, 19:28, 34:37)]
  d <- d[URBAN_PTS==T]
  if(iso3 != "SSA") d <- d[ISOALPHA==iso3] else d <- d[UN_2015_E >= 100000]
  d[, DIST150km := factor(DIST150km, levels=c(F,T), labels=c("hinterland", "coast"))]
  d[order(-GROWRATE_20), rank := 1:.N, by=DIST150km]
  d <- d[rank <= 10][order(-rank)]
  d[COUNTRYNM=="Democratic Republic of the Congo", COUNTRYNM := "Congo, Dem. Rep."]
  d[, NAME := ordered(tools::toTitleCase(tolower(paste(COUNTRYNM, NAME2, sep=" - "))))]

  p <- data.frame(d) %>%
    ggvis(y=~NAME) %>%
    add_axis("x", title="", format=".2s", properties=ap) %>%
    add_axis("y", title="", properties=ap) %>%
    layer_rects(x=0, x2=~GROWTH_20, fill=~DIST150km, height=band(),
      stroke:="#FFF", fillOpacity:=0.8) %>%
    scale_ordinal("fill", range=pal) %>%
    add_legend("fill", title="Location") %>%
    set_options(height=400, width="auto", resizable=F) %>%
    add_tooltip(tt, on="hover")

  return(p)
}

# p4 - Urbanization trends across countries and locations
p4 <- function()  {


  return(p)
}




rm(tmp, d, iso3)
save.image("./out/popTrends/popTrends.RData")
save.image("/home/projects/hc-shiny/popTrends/tmp/popTrends.RData")
