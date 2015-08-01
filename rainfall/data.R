#####################################################################################
# Title: Prepare CRU-TS 3.22 Persistent Datasets
# Date: December 2014
# Project: HarvestChoice
# Author: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

# Note that all downloaded time-series are used as-is, so we can use drop-in replacements
# without any further transformation, but we need to pre-process district summaries for
# extra speed using stats.cntr() below.

library(stringr)
library(data.table)
library(reshape2)
library(raster)
library(rgdal)

setwd("/home/projects/shiny/rainfall")

## CRU variables are in directory tree
# Note that each 1901-2013 time series is 2.6GB uncrompressed
d <- c("cld", "dtr", "frs", "pet", "pre", "tmn", "tmp", "tmx", "vap", "wet")
f <- "cru_ts3.22.1901.2013.cld.dat.nc.gz"
f <- str_replace(f, "cld", d)
f <- str_replace(f, "cru_ts", paste0(d, "/cru_ts"))

# Retrieve netCDF files one variable at a time (pre, tmp, tmn, tmx)
baseurl <- "http://www.cru.uea.ac.uk/cru/data/hrg/cru_ts_3.22/cruts.1406251334.v3.22/"

for (i in 8) {
  download.file(paste0(baseurl, f[i]), paste0("./data/", basename(f[i])), mode="wb")
  system(paste0("gzip -d ./data/", basename(f[i])))
  assign(d[i], brick(paste0("./data/", str_replace(basename(f[i]), ".gz", ""))))
}


## CRU temperature anomalies
url <- "http://www.cru.uea.ac.uk/cru/data/temperature/CRUTEM.4.3.0.0.anomalies.nc"
download.file(url, "./data/CRUTEM.4.3.0.0.anomalies.nc", mode="wb")

# Explore
ano <- brick("./data/CRUTEM.4.3.0.0.anomalies.nc")
extent(ano)
names(ano)
tm <- seq(as.Date("1850-01-16"), as.Date("2014-10-16"), "month")
ano <- setZ(ano, tm, "month")
r <- crop(ano, g2[g2$ADM0_NAME=="Ghana",])
hist(r)


## Palmer Drought Severity Index (PDSI)
url <- "http://www.cgd.ucar.edu/cas/catalog/climind/pdsisc.monthly.maps.1850-2012.fawc=1.r2.5x2.5.ipe=2.nc.gz"
download.file(url, "./data/pdsisc.monthly.maps.1850-2012.nc.gz", mode="wb")
system("gzip -d ./data/pdsisc.monthly.maps.1850-2012.nc.gz")

# Explore
pdsi <- brick("./data/pdsisc.monthly.maps.1850-2012.nc")
extent(pdsi)
# class       : Extent
# xmin        : -180
# xmax        : 180
# ymin        : -60
# ymax        : 77.5

summary(raster(pdsi,1))
#         X1850.04162597656
# Min.               -4.997
# 1st Qu.            -1.809
# Median             -0.525
# 3rd Qu.             1.838
# Max.                4.891
# NA's             7718.000

head(names(pdsi), 13)
#  [1] "X1850.04162597656" "X1850.125"         "X1850.20837402344"
#  [4] "X1850.29162597656" "X1850.375"         "X1850.45837402344"
#  [7] "X1850.54162597656" "X1850.625"         "X1850.70837402344"
# [10] "X1850.79162597656" "X1850.875"         "X1850.95837402344"
# [13] "X1851.04162597656"

tail(names(pdsi), 13)
#  [1] "X2011.95837402344" "X2012.04162597656" "X2012.125"
#  [4] "X2012.20837402344" "X2012.29162597656" "X2012.375"
#  [7] "X2012.45837402344" "X2012.54162597656" "X2012.625"
# [10] "X2012.70837402344" "X2012.79162597656" "X2012.875"
# [13] "X2012.95837402344"

nlayers(pdsi)
# [1] 1956

tm <- seq(as.Date("1850-01-01"), as.Date("2012-12-31"), "month")
pdsi <- setZ(pdsi, tm, "month")

g <- g2.web[g2.web$ADM0_NAME=="Ghana",]
spplot(crop(pdsi, g), 700)

dt <- extract(pdsi, g, fun=mean, na.rm=T, df=T, small=T)
dt <- cbind(g@data, dt)
dt <- data.table(dt)
setnames(dt, 8:dim(dt)[2], format(tm, "%Y-%m-%d"))
dt <- melt(dt, id.vars=c(names(g), "ID"), variable.name="month", variable.factor=F)
dt[, month := as.Date(month)]
# Limit to 1960 onwards
dt <- dt[month>=as.Date("1960-01-01")]
summary(dt$value)

# Test time serie decomposition
library(TTR)
library(zoo)
dt.ts <- dt[ID==1][order(month)]
dt.ts <- ts(dt.ts, start=c(dt[, min(year(month))], 1), frequency=12)
plot.ts(dt.ts)
dt.ts <- decompose(na.StructTS(dt.xts))
plot(dt.ts)
dt.ts <- dt.ts$trend
dt.ts <- data.table(trend=dt.ts)
dt <- cbind(dt, dt.ts)



#####################################################################################
# Pre-process District Summaries (faster to map)
#####################################################################################

setwd("/home/projects/shiny/rainfall")

library(data.table)
library(reshape2)
library(raster)
library(rgdal)

load("../../cell5m/rdb/g2.rda")
# Use webified boundaries (updated with GAUL 2013 v14 version)
g2.web <- readRDS("../../cell5m/rdb/g2_2013v14.web.rds")

# Helper - summarize rasters over districts
stats.cntr <- function(x, y) {
  g <- g2[g2$ADM0_CODE==x,]
  dt <- extract(get(y), g, fun=mean, na.rm=T, df=T, small=T)
  dt <- cbind(g@data, dt)
  dt <- data.table(dt)
  setnames(dt, 8:dim(dt)[2], format(tm, "%Y-%m-%d"))
  dt <- melt(dt, id.vars=c(names(g), "ID"), variable.name="month", variable.factor=F)
  dt[, month := as.Date(month)]
  # Limit to 1960 onwards
  dt <- dt[month>=as.Date("1960-01-01")]
  saveRDS(dt, file=paste0("./data/rds/", y, x, ".rds"), compress=T)
}


# Helper - symbolize GeoJSON
json.cntr <- function(x, y, col) {
  # Load pre-processed district X month records
  dt <- readRDS(paste0("./data/rds/", y, x, ".rds"))
  # Summarize each district over entire period for mapping (mm/month)
  dt <- dt[, list(
    mean=mean(value, na.rm=T),
    min=min(value, na.rm=T),
    max=max(value, na.rm=T),
    sd=sd(value, na.rm=T)), keyby=ADM2_CODE]

  # Load country GeoJSON as list
  f <- paste0("./data/json/g2web", x)
  m <- jsonlite::fromJSON(f, simplifyVector=F)

  # Construct symbology from district means (note that PDSI has fewer values)
  dt <- dt[J(sapply(m$features, function(x) x$properties$ADM2_CODE))]
  rg <- range(dt$mean, na.rm=T)
  cv <- try(classInt::classIntervals(dt$mean, n=min(c(5, dt[,length(unique(mean))])))$brks)

  if (class(cv)=="try-error") {
    # classInt fails apply only 1 color (white)
    dt[, cl := "white"]
  } else {
    # Symbolize normally
    dt[, cl := cut(mean, unique(c(rg[1]-1, cv, rg[2]+1)), cutlabels=F, ordered_result=T)]
    dt[, cl := colorRampPalette(col)(length(cv)+1)[cl]]
  }

  # Add symbology to GeoJSON. Note that the districts do not necessarily match here
  # since we used the original g2 to generate stats, and g2.web to generate GeoJSON
  for (i in 1:length(m$features)) {
    m$features[[i]]$properties$mean <- dt[i, round(mean, 2)]
    m$features[[i]]$properties$min <- dt[i, round(min, 2)]
    m$features[[i]]$properties$max <- dt[i, round(max, 2)]
    m$features[[i]]$properties$sd <- dt[i, round(sd, 2)]
    m$features[[i]]$style <- list(fillColor=dt[i, cl], weight=.6, color="white", fillOpacity=0.7)
  }

  # Write to RDS file
  saveRDS(m, file=paste0("./data/rds/", y, x, ".json.rds"), compress=T)
}

# Country code list
cntr <- unique(g2.web@data$ADM0_CODE)

# Pre-process `pre`
pre <- brick("./data/cru_ts3.22.1901.2013.pre.dat.nc")
tm <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
pre <- setZ(pre, tm, "month")
col <- rev(c("#2F6FBF", "#69DB4D", "#F9EF58", "#DC5207", "#830000"))
for (i in cntr) stats.cntr(i, "pre")
for (i in cntr) json.cntr(i, "pre", col)
# 3 countries 6, 102, 74 failed to overlay (31, 42, 48)

# Pre-process `pdsi`
pdsi <- brick("./data/pdsisc.monthly.maps.1850-2012.nc")
tm <- seq(as.Date("1850-01-01"), as.Date("2012-12-31"), "month")
pdsi <- setZ(pdsi, tm, "month")
col <- c("#FF9900", "#FFFF66", "#FFFFFF", "#99FF99", "#009900")
for (i in cntr) stats.cntr(i, "pdsi")
for (i in cntr) json.cntr(i, "pdsi", col)
# countries 6, 102, 74 failed to overlay


#####################################################################################
# Simplify GAUL 2008 District Boundaries
#####################################################################################

rm(list=ls())
setwd("/home/projects/shiny/rainfall")

library(rgeos)
library(rgdal)
library(data.table)

# Load GAUL 2008 (CELL5M version)
load("../../cell5m/rdb/g2.rda")

# I believe gSimplify uses the same libraries as GRASS
g2.web <- gSimplify(g2, 0.05, topologyPreserve=T)
g2.web <- SpatialPolygonsDataFrame(g2.web, g2@data)
plot(g2.web[g2.web$ADM0_NAME=="Ghana", ])

# Export to GRASS v.in.ogr with a 0.06 snapping threshold, then reload
writeOGR(g2.web, "../../cell5m/rdb", "g2.web", "ESRI Shapefile")
g2.web <- readOGR("../../cell5m/rdb", "g2.web")
#load("../../cell5m/rdb/g2_2008v09.web.rda")
g2.web@data <- g2.web@data[, -c(3:6,11:12)]

# Add X,Y centroids
dt <- data.table(g2.web@data)
setcolorder(dt, c(3:6,1,2))
dt[, X := coordinates(g2.web)[, 1]]
dt[, Y := coordinates(g2.web)[, 2]]
g2.web@data <- dt

# Save webified version for re-use
saveRDS(g2.web, "../../cell5m/rdb/g2_2008v09.web.rds", compress=T)

# Create as many geojson files as SSA countries
for (i in unique(g2.web@data$ADM0_CODE)) {
  writeOGR(g2.web[g2.web$ADM0_CODE==i,], paste0("./data/json/g2web", i), paste0(i), "GeoJSON",
    overwrite_layer=T)
}


# Create well-formatted country, province, district list for re-use in input controls
d2 <- data.table(g2.web@data)
d2 <- d2[, .N, by=list(ADM0_NAME, ADM1_NAME, ADM2_NAME)]
d2 <- d2[, lapply(.SD, as.character), .SDcols=1:3]
setkey(d2, ADM0_NAME, ADM1_NAME, ADM2_NAME)
d2 <- split(d2, d2$ADM0_NAME)
d2 <- lapply(d2, function(x) x[, list(ADM1_NAME, ADM2_NAME)])
d2 <- lapply(d2, function(x) split(x, x$ADM1_NAME))
d2 <- lapply(d2, function(x) lapply(x, function(y) y$ADM2_NAME))

saveRDS(d2, "../../cell5m/rdb/g2_2008v09.list.rds", compress=T)


# Compare original and webified versions
dt <- data.table(g2@data)
dtw <- data.table(g2.web@data)
tmp <- dt[, length(unique(ADM2_CODE)), keyby=ADM0_NAME]
tmpw <- dtw[, length(unique(ADM2_CODE)), keyby=ADM0_NAME]
# Which countries are missing?
tmp[!tmpw][, ADM0_NAME]
# [1] British Indian Ocean Territory Cape Verde                     Glorioso Island
# [4] Juan de Nova Island            Mauritius                      Réunion
# [7] Saint Helena                   Sao Tome and Principe          Seychelles
tmp <- tmpw[tmp]
setnames(tmp, 2:3, c("g2.web", "g2"))
tmp <- tmp[g2!=g2.web]
tmp[, diff := g2-g2.web]
tmp[, sum(diff)]
# [1] 398 missing districts



#####################################################################################
# 2015.01.29 Update: GAUL 2015
#####################################################################################
# Switch to latest FAO GAUL 2015 boundaries
# http://www.fao.org/geonetwork/srv/en/resources.get?id=12691&fname=g2015_2014_2.zip
# Switch to rstudio/leaflet R package allowing binding leaflet to sp objects (no need
# to pre-process json lists)
# We only need to pre-process the raster summaries across all districts `dt2`
# Also add ERA monthly estimates
# Then update the shiny app to use rstudio/leaflet and the new input datasets

library(rgeos)
library(rgdal)
library(raster)
library(maptools)
library(data.table)

setwd("/home/projects/shiny")

## ECMWF Re-analysis Dataset (ERA)
## Synoptic monthly means, Surface, Total precipitation, Volumetric soil water layer 1,
## interim_full_month, 1979-01-01...2014-12-01, Forecast, ERA Interim
# Documented at http://www.hydrol-earth-syst-sci.net/19/389/2015/hess-19-389-2015.pdf
url <- "http://download.ecmwf.org/data/web219/netcdf-web219-20150202010350-10683-25927.nc"
download.file(url, "./data/era-interim.monthly.pre.water.1979-2014.nc", mode="wb")

era <- brick("./data/era-interim.monthly.pre.water.1979-2014.nc", varname="tp")
extent(era)
head(names(era))
tail(names(era))
tm <- seq(as.Date("1979-01-01"), as.Date("2014-12-01"), "month")
era <- setZ(era, tm, "month")
projection(era) <- CRS("+init=epsg:")

tmp <- g2.web[g2.web$ADM0_NAME=="Nigeria",]
tmp <- crop(era, tmp)
spplot(tmp, 50, add=T)
plot(tmp, col="red")
# Looks ok, note that unit is meter, not millimeter


## GAUL 2015
# List of SSA countries
ssa <- fread("../../cell5m/rdb/ssa.csv")
paste0(ssa, collapse="', '")

# GAUL 2014 (2015 eds) was converted and simplified on local using QGIS, load here
# Used QGIS simplify with tolerance=
g2 <- readOGR("../../cell5m/rdb", "g2015_2014_2_SSA")
plot(g2[g2$ADM0_NAME=="Ghana",])

# Merge all features by admin codes
g2.dt <- data.table(g2@data)
g2.dt[, rn := row.names(g2)]
setkey(g2.dt, ADM0_CODE, ADM1_CODE, ADM2_CODE)
tmp <- unique(g2.dt)
nrow(g2.dt)
nrow(tmp)
g2.dt[duplicated(g2.dt)]
g2.dt[ADM2_CODE==22602]
plot(g2[g2$ADM2_CODE==22602,])
# Nigeria Abia Member State has duplicated ADM2_CODE, correct here
g2.dt[ADM2_NAME=="Ukwa West", ADM2_CODE := 22603]
setkey(g2.dt, rn)
g2@data <- g2.dt[row.names(g2)]
writeOGR(g2, "../../cell5m/rdb", "g2015_2014_2_SSA_web", "ESRI Shapefile")


# Simplify features (used service at http://mapshaper.org/)
g2.web <- readOGR("../../cell5m/rdb", "g2015_2014_2_SSA_web")
proj4string(g2.web) <- CRS("+init=epsg:4326")
# Merge attributes using `FID`
setkey(g2.dt, rn)
g2.web.dt <- g2.dt[as.character(g2.web$FID)]
g2.web@data <- g2.web.dt
plot(g2.web[g2.web$ADM0_NAME=="Ghana",])
plot(g2[g2$ADM0_NAME=="Togo",], add=T)


# Create well-formatted country, province, district list for re-use in input controls
d2 <- data.table(g2.web@data)
d2 <- d2[, .N, by=list(ADM0_NAME, ADM1_NAME, ADM2_NAME)]
d2 <- d2[, lapply(.SD, as.character), .SDcols=1:3]
setkey(d2, ADM0_NAME, ADM1_NAME, ADM2_NAME)
d2 <- split(d2, d2$ADM0_NAME)
d2 <- lapply(d2, function(x) x[, list(ADM1_NAME, ADM2_NAME)])
d2 <- lapply(d2, function(x) split(x, x$ADM1_NAME))
d2 <- lapply(d2, function(x) lapply(x, function(y) y$ADM2_NAME))


# Save
f <- c("ADM0_CODE", "ADM0_NAME", "ADM1_CODE", "ADM1_NAME", "ADM2_CODE", "ADM2_NAME")
g2 <- g2[, f]
g2.web <- g2.web[, f]
saveRDS(g2, "../../cell5m/rdb/g2_2014v15.rds", compress=T)
saveRDS(g2.web, "../../cell5m/rdb/g2_2014v15.web.rds", compress=T)
saveRDS(d2, "../../cell5m/rdb/g2_2014v15.list.rds", compress=T)


# Helper - summarize raster over districts
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

# Helper - add symbology (period mean)
genSymbol <- function(x, col) {
  # Summarize each district over entire period for mapping (mm/month)
  dt <- x[, list(
    mean=mean(value, na.rm=T),
    min=min(value, na.rm=T),
    `85th`=quantile(value, 0.85, na.rm=T),
    max=max(value, na.rm=T),
    sd=sd(value, na.rm=T)), keyby=list(ADM0_CODE, ADM1_CODE, ADM2_CODE)]

  # Construct symbology from district means (note that PDSI has fewer values)
  rg <- range(dt$mean, na.rm=T)
  cv <- try(classInt::classIntervals(dt$mean, n=min(c(5, dt[,length(unique(mean))])))$brks)

  if (class(cv)=="try-error") {
    # classInt fails apply only 1 color (white)
    dt[, cl := "white"]
  } else {
    # Symbolize normally
    dt[, cl := cut(mean, unique(c(rg[1]-1, cv, rg[2]+1)), cutlabels=F, ordered_result=T)]
    dt[, cl := colorRampPalette(col)(length(cv)+1)[cl]]
  }
  return(dt)
}


##  Pre-process district summaries for speed
# Country code list
cntr <- unique(g2.web@data$ADM0_CODE)

# Pre-process `pre`
pre <- brick("./data/cru_ts3.22.1901.2013.pre.dat.nc")
tm <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
pre <- setZ(pre, tm, "month")
col <- rev(c("#2F6FBF", "#69DB4D", "#F9EF58", "#DC5207", "#830000"))
dt2.pre <- genStats("pre")
dt2.pre.web <- genSymbol(dt2.pre, col)
saveRDS(dt2.pre, "./data/dt2.pre.rds")


# Pre-process `pdsi`
pdsi <- brick("./data/pdsisc.monthly.maps.1850-2012.nc")
tm <- seq(as.Date("1850-01-01"), as.Date("2012-12-31"), "month")
pdsi <- setZ(pdsi, tm, "month")
col <- c("#FF9900", "#FFFF66", "#FFFFFF", "#99FF99", "#009900")
dt2.pdsi <- genStats("pdsi")
dt2.pdsi.web <- genSymbol(dt2.pdsi, col)
saveRDS(dt2.pdsi, "./data/dt2.pdsi.rds")


# Pre-process `eratp` total precipitation (tp)
eratp <- brick("./data/era-interim.monthly.pre.water.1979-2014.nc", varname="tp")
tm <- seq(as.Date("1979-01-01"), as.Date("2014-12-01"), "month")
eratp <- setZ(eratp, tm, "month")
col <- brewer.pal(9, "YlGnBu")
dt2.eratp <- genStats("eratp")
dt2.eratp[, value := value*1000]
dt2.eratp.web <- genSymbol(dt2.eratp, col)
saveRDS(dt2.eratp, "./data/dt2.eratp.rds")


# Save all
save(d2, g2, g2.web,
  dt2.pre, dt2.pdsi, dt2.eratp,
  dt2.pre.web, dt2.pdsi.web, dt2.eratp.web,
  file="./data/rainfall_2014v15.RData", compress=T)


## Create country .rds files (until rstudio/leaflet package is fixed)
var <- c("pre", "pdsi", "eratp")
cntr <- unique(g2.web@data$ADM0_CODE)

for(j in var) {
  for (i in cntr) {
    dt <- get(paste0("dt2.", j, ".web"))[ADM0_CODE==i]
    dt[is.nan(mean) | is.infinite(mean), mean := NA]
    dt[is.nan(min) | is.infinite(min), min := NA]
    dt[is.nan(max) | is.infinite(max), max := NA]
    dt[is.nan(`85th`) | is.infinite(`85th`), `85th` := NA]
    dt[is.nan(sd) | is.infinite(sd), sd := NA]

    dt[, mean := round(mean, 1)]
    dt[, min := round(min, 1)]
    dt[, max := round(max, 1)]
    dt[, `85th` := round(`85th`, 1)]
    dt[, sd := round(sd, 1)]

    # Add admin names
    t <- get(paste0("dt2.", j))[ADM0_CODE==i]
    setkey(dt, ADM1_CODE, ADM2_CODE)
    setkey(t, ADM1_CODE, ADM2_CODE)
    tmp <- unique(t)
    dt <- tmp[, .SD, .SDcols=1:6][dt]

    g <- g2.web[g2.web$ADM0_CODE==i,]
    g@data <- dt[J(g$ADM1_CODE, g$ADM2_CODE)]
    f <- paste0("./data/json/", j, i)
    writeOGR(g, f, g$ADM2_CODE, "GeoJSON", overwrite_layer=T)

    m <- jsonlite::fromJSON(f, simplifyVector=F)
    for (x in 1:length(m$features)) {
      m$features[[x]]$style <- list(fillColor=g@data[x, cl], weight=.6, color="white", fillOpacity=0.7)
    }
    saveRDS(m, paste0("./data/rds/", j, i, ".json.rds"), compress=T)
    saveRDS(t, file=paste0("./data/rds/", j, i, ".rds"), compress=T)
  }
}


#####################################################################################
# 2015.03.18 Update: Africa-wide yearly stats for Julia Collins
#####################################################################################

library(maptools)
setwd("/home/projects/shiny/rainfall")

load("../../cell5m/rdb/g0.rda")
pre <- brick("./data/cru_ts3.22.1901.2013.pre.dat.nc")
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
write.csv(dt.year, "./data/CRU.SSA.1901-2013.csv", na="", row.names=F)


#####################################################################################
# 2015.03.18 Update: Add CRU monthly temperatures
#####################################################################################

rm(list=ls())
load("./data/rainfall_2014v15.RData")

##  Pre-process district summaries for speed
# Country code list
cntr <- unique(g2.web@data$ADM0_CODE)

# Pre-process `tmp`
tmp <- brick("./data/cru_ts3.22.1901.2013.tmp.dat.nc")
tm <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
tmp <- setZ(tmp, tm, "month")
col <- c("#801FEF", "#0000FF", "#4169E1", "#1C90FF", "#00BFFF", "#8CCDEF", "#FFFFC8",
  "#FFE131", "#FFAA00", "#FF6E00", "#FF0000", "#C80000", "#FFB1B1")
dt2.tmp <- genStats("tmp")
dt2.tmp.web <- genSymbol(dt2.tmp, col)
saveRDS(dt2.tmp, "./data/dt2.tmp.rds")

# Save all
save(d2, g2, g2.web,
  dt2.pre, dt2.pdsi, dt2.eratp, dt2.tmp,
  dt2.pre.web, dt2.pdsi.web, dt2.eratp.web, dt2.tmp.web,
  file="./data/rainfall_2014v15.RData", compress=T)

# Save country .rds
for (i in cntr) {
  dt <- dt2.tmp.web[ADM0_CODE==i]
  dt[is.nan(mean) | is.infinite(mean), mean := NA]
  dt[is.nan(min) | is.infinite(min), min := NA]
  dt[is.nan(max) | is.infinite(max), max := NA]
  dt[is.nan(`85th`) | is.infinite(`85th`), `85th` := NA]
  dt[is.nan(sd) | is.infinite(sd), sd := NA]

  dt[, mean := round(mean, 1)]
  dt[, min := round(min, 1)]
  dt[, max := round(max, 1)]
  dt[, `85th` := round(`85th`, 1)]
  dt[, sd := round(sd, 1)]

  # Add admin names
  t <- dt2.tmp[ADM0_CODE==i]
  setkey(dt, ADM1_CODE, ADM2_CODE)
  setkey(t, ADM1_CODE, ADM2_CODE)
  temp <- unique(t)
  dt <- temp[, .SD, .SDcols=1:6][dt]

  g <- g2.web[g2.web$ADM0_CODE==i,]
  g@data <- dt[J(g$ADM1_CODE, g$ADM2_CODE)]
  f <- paste0("./data/json/tmp", i)
  writeOGR(g, f, g$ADM2_CODE, "GeoJSON", overwrite_layer=T)

  m <- jsonlite::fromJSON(f, simplifyVector=F)
  for (x in 1:length(m$features)) {
    m$features[[x]]$style <- list(fillColor=g@data[x, cl], weight=.6, color="white", fillOpacity=0.7)
  }
  saveRDS(m, paste0("./data/rds/tmp", i, ".json.rds"), compress=T)
  saveRDS(t, file=paste0("./data/rds/tmp", i, ".rds"), compress=T)
}


#####################################################################################
# TODO 2015.03.20 Migrate to rstudio/leaflet
#####################################################################################




#####################################################################################
# TODO 2015.03.18 Update: Use SEAS package for normal and departure statistics
#####################################################################################

library(seas)


#####################################################################################
# TODO 2015.03.24 Update: Add new indicators
#####################################################################################
# total annual precipitation
# precipitation on the days of heavy rain
# maximum number of consecutive dry days in the year
# annual mean maximum and mean minimum temperatures
# Suggested in http://file.scirp.org/Html/7-2360181_50081.htm


#####################################################################################
# 2015.04.09 Generate Climate Stats for Costa Rica
#####################################################################################
# For Eduardo WB

setwd("/home/projects/shiny/rainfall")
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

cri <- readOGR("./data/", "district471")

# Process `pre`
pre <- brick("./data/cru_ts3.22.1901.2013.pre.dat.nc")
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
pdsi <- brick("./data/pdsisc.monthly.maps.1850-2012.nc")
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
tmp <- brick("./data/cru_ts3.22.1901.2013.tmp.dat.nc")
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
writeOGR(cri, "./data/", "cri_adm3_climate", "ESRI Shapefile")

# Let's reprocess but keep all years so we can then generate the bio18 variables
# We need `tmn` an `tmx` instead of `tmp`
tmn <- brick("./data/cru_ts3.22.1901.2013.tmn.dat.nc")
tm <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
tmn <- setZ(tmn, tm, "month")

tmx <- brick("./data/cru_ts3.22.1901.2013.tmx.dat.nc")
tm <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
tmx <- setZ(tmx, tm, "month")

cri <- spTransform(cri, proj4string(pre))
pre <- crop(pre, cri)
tmp <- crop(tmp, cri)
tmn <- crop(tmn, cri)
tmx <- crop(tmx, cri)

# Save
save(pre, tmp, tmn, tmx, file="./data/cri_bioclim.RData")


#####################################################################################
# 2015.04.13 Generate Annual Mean and CV for CELL5M
#####################################################################################
# Need to extract and summarize `pre` values over CELL5M SSA grid
library(hcapi3)

grid <- getLayer("ADM0_NAME")
grid <- SpatialPointsDataFrame(grid[, list(X, Y)], data.frame(CELL5M=grid[["CELL5M"]]),
  proj4string=CRS("+init=epsg:4326"))

pre <- brick("./data/cru_ts3.22.1901.2013.pre.dat.nc")
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
saveRDS(pre, "./data/cell5m_pre_cru3.22.rds", compress=T)



#####################################################################################
# 2015.06.07 Update: Add CRU monthly temperatures min/max
#####################################################################################
library(dismo)

rm(list=ls())
load("./data/rainfall_2014v15.RData")

# These are needed to generate bioclimatic variables
tm <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
pre <- brick("./data/cru_ts3.22.1901.2013.pre.dat.nc")
pre <- setZ(pre, tm, "month")
tmn <- brick("./data/cru_ts3.22.1901.2013.tmn.dat.nc")
tmn <- setZ(tmn, tm, "month")
tmx <- brick("./data/cru_ts3.22.1901.2013.tmx.dat.nc")
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

# Let's also process country-level biovars
bio.g0 <- dt2.bio[, list(), by=list(ADM0_CODE, month)]


# Save for re-use
saveRDS(bio, file="./data/ts_yearly_biovars_g2.rds")



#####################################################################################
# 2015.07.28 Update: Process biovars across Ghana districts
#####################################################################################

library(dismo)
library(raster)
library(data.table)
library(rgdal)
library(hcapi3)

setwd("/home/projects/shiny/rainfall")
load("./data/rainfall_2014v15.RData")

gha <- getData(country="GHA", level=2)
# => 137 districts, try GAUL 2008 instead

g2.dt <- data.table(g2.web@data)
g2.dt[, rn := row.names(g2.web)]
g2.dt[ADM0_NAME=="Ghana", length(unique(ADM2_CODE))]
# => 216 districts (since 2008), was 170 in 2008
# Found one with 173 district at GeoCommons
# http://geocommons.com/overlays/201941.zip
download.file("http://geocommons.com/overlays/201941.zip", "./data/GHA_adm2_170.zip")
unzip("./data/GHA_adm2_170.zip", exdir="./data")


# Eduardo provided the GLSS6 district codelist in `./data/GLSS6_codelist.csv`
gha.lbl <- fread("./data/GLSS6_codelist.csv")
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


gha <- readOGR("./data", "ghana_districts")
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

write.csv(gha.lbl, "./data/GLSS6_district.csv", row.names=F, na="")

# Reload with matching here
gha.lbl <- fread("./data/GLSS6_district.csv")

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
writeOGR(gha, "./data", "GHA_adm2_170", "ESRI Shapefile", overwrite=T)
gha <- readOGR("./data", "GHA_adm2_170")
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
pre <- brick("./data/cru_ts3.22.1901.2013.pre.dat.nc")
pre <- setZ(pre, tm, "month")
tmn <- brick("./data/cru_ts3.22.1901.2013.tmn.dat.nc")
tmn <- setZ(tmn, tm, "month")
tmx <- brick("./data/cru_ts3.22.1901.2013.tmx.dat.nc")
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
write.dta(gha.dt, "./data/gha-glss6-svyMap.dta", version=11L)

# Export to shapefile
setkey(gha.dt, rn)
gha@data <- gha.dt[row.names(gha)]

writeOGR(gha, "./data", "gha-glss6-svyMap", "ESRI Shapefile")



#####################################################################################
# 2015.07.28 Update: Process biovars across AR villages
#####################################################################################

library(dismo)
library(raster)
library(data.table)
library(rgdal)
library(hcapi3)

setwd("/home/projects/shiny/rainfall")
load("./data/rainfall_2014v15.RData")

# Import AR village locations
ar <- readRDS("./data/ARPointsZoI_2015.07.28.rds")


# Generate biovars from CRU 3.22
# These are needed to generate bioclimatic variables
tm <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
pre <- brick("./data/cru_ts3.22.1901.2013.pre.dat.nc")
pre <- setZ(pre, tm, "month")
tmn <- brick("./data/cru_ts3.22.1901.2013.tmn.dat.nc")
tmn <- setZ(tmn, tm, "month")
tmx <- brick("./data/cru_ts3.22.1901.2013.tmx.dat.nc")
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
saveRDS(ar, "./data/ARPointsZoI_2015.07.28.rds")


# Save all
rm(dt2.eratp, dt2.pdsi, dt2.tmp)
rm(g2, dt2.tmn, dt2.tmx, genStats, cntr, baseurl, tm, tmn, tmx, pre)
save.image("./data/rainfall_2014v15.RData")




