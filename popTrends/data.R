#####################################################################################
# Title:   Map Population Hotspots 2000-2020 or 1990-2015
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
library(hcapi3)
library(raster)
library(ggvis)

setwd("~/Projects/shiny/popTrends")
load("./tmp/popTrends.RData")

# SSA country boundaries for plotting
data(World)
World <- World[World$continent=="Africa",]

# Load GPWv4
gpw <- curl_download(
  "http://beta.sedac.ciesin.columbia.edu/downloads/data/gpw-v4/gpw-v4-admin-unit-center-points-population-estimates/gpw-v4-admin-unit-center-points-population-estimates-csv-global.zip",
  "./data/gpw-v4-admin-unit-center-points-population-estimates-csv-global.zip")
gpw <- unzip(gpw, exdir="./data")
gpw <- fread(gpw[1])

# Keep global layer for re-use
save(gpw, file="./data/gpw-v4-admin-unit-center-points-population-estimates-csv-global.rds")

# Load Joe's 2010 urban mask
urb <- raster("./data/UrbanExtentAfrica2010_v1.tif")

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
writeRaster(urb, "./tmp/UrbanExtentAfrica2010_v1.tif",
  colorTables=list(c("green", "orange")),
  catNames=list(c("rural", "urban")))


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

# Save into `gpw` as well
gpw@data <- data.frame(gpw.dt)


# Share of rural/urban population within 150km of coastline
# Load Africa coastline
coast <- curl_download(
  "http://omap.africanmarineatlas.org/BASE/data/coast/vmap0/africa_coastline_vmap0.zip",
  "./data/africa_coastline_vmap0.zip")
coast <- unzip(coast, exdir="./data")
coast <- readOGR("./data", "africa_coastline_vmap0")
proj4string(coast)
# [1] "+proj=longlat +datum=NAD27 +no_defs +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat"

# Reproject to equidistant
coast <- spTransform(coast, CRS("+init=epsg:3786"))
gpw <- spTransform(gpw, proj4string(coast))

# Map these layers to check
tm_shape(gpw) + tm_bubbles(alpha=.3) + tm_shape(coast) + tm_lines()

# Mark units within 150km of coastline
dist150k <- rep(0, nrow(gpw))
dist150k <- sapply(row.names(gpw), function(x) gWithinDistance(gpw[x,], coast, 150*1000))
gpw.dt[, DIST150km := dist150k]

# Correct possible interger overflow
gpw.dt[, `:=`(
  UN_2000_E=as.numeric(UN_2000_E),
  UN_2005_E=as.numeric(UN_2005_E),
  UN_2010_E=as.numeric(UN_2010_E),
  UN_2015_E=as.numeric(UN_2015_E),
  UN_2020_E=as.numeric(UN_2020_E))]

# Summarize - version 1
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

# Summarize - version 2
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

m <- tm_shape(gpw.urb2[gpw.urb2$UN_2015_E >= 80000,]) +
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
# Charts (using urban def. #2)
#####################################################################################
# Theme
ap <- axis_props(
  axis=list(stroke="transparent"),
  title=list(fill="#444", font="Pt Sans", fontSize=14),
  labels=list(fill="#444", font="Pt Sans", fontSize=12),
  ticks=list(stroke="transparent"),
  grid=list(stroke="#e3e3e3", strokeWidth=1))

pal <- c("#6EB89A", "#E05D5A")

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
    add_axis("x", title="", title_offset=40, properties=ap) %>%
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
    add_axis("x", title="", title_offset=40, properties=ap) %>%
    add_axis("y", title="", format="%", properties=ap) %>%
    group_by(DIST150km) %>%
    layer_points(fill=~DIST150km, stroke:="#fff") %>%
    layer_lines(stroke=~DIST150km) %>%
    scale_ordinal("stroke", range=pal) %>%
    scale_ordinal("fill", range=pal) %>%
    hide_legend("stroke") %>%
    add_legend("fill", title="Urban/Rural") %>%
    set_options(height=320, width="auto", resizable=F) %>%
    add_tooltip(tt, on="hover")

  return(p)

}

# p3 - Top urban areas and growth rate in/outside coastal areas
p3 <- function(iso3="SSA") {

  d <- gpw.dt[, .SD, .SDcols=c(4:8, 19:28, 34:37)]
  d <- d[URBAN_PTS==T]
  d[, DIST150km := factor(DIST150km, levels=c(F,T), labels=c("hinterland", "coast"))]
  if(iso3 != "SSA") d <- d[ISOALPHA==iso3]
  d <- d[order(-UN_2015_E)][1:10]

  p <- data.frame(d) %>%
    ggvis(~DIST150km, ~factor(paste(NAME2, NAME3, sep=" - "))) %>%
    add_axis("x", title="", title_offset=40, properties=ap) %>%
    add_axis("y", title="", properties=ap) %>%
    layer_points(fillOpacity=~GROWRATE_20, size=~UN_2015_E, stroke:="#fff") %>%
    add_legend("size", title="Urban Population") %>%
    add_legend("fillOpacity", title="20-year Growth Rate", properties=legend_props(legend=list(y=60))) %>%
    set_options(height=320, width="auto", resizable=F, duration=0)
    # add_tooltip(tt, on="hover")

  return(p)
}


rm(tmp, d, iso3)
save.image("./tmp/popTrends.RData")
