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


# Save for re-use
saveRDS(bio, file="./data/ts_yearly_biovars_g2.rds")



#####################################################################################
# 2015.07.28 Update: Process biovars across GLSS6 districts
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


#####################################################################################
# 2015.08.02 Update: Update leaflet, merge all data files
#####################################################################################

# Get all rds files in ./data/rds, not *json*
# Combine them all and compute mean stats by districts

pre <- readRDS("./data/dt2.pre.rds")
tmp <- readRDS("./data/dt2.tmp.rds")
pdsi <- readRDS("./data/dt2.pdsi.rds")

pre[, var := "pre"]
tmp[, var := "tmp"]
pdsi[, var := "pdsi"]

dt <- rbind(pre, tmp, pdsi)
saveRDS(dt, file="./data/dt2.rds")



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

setwd("/home/projects/shiny/rainfall")

# Load GLSS5 survey map (110 districts)
gha <- readOGR("./data/DSG", "gha-glss5-map_L2")
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
pre <- brick("./data/cru_ts3.22.1901.2013.pre.dat.nc")
pre <- setZ(pre, tm, "month")
tmn <- brick("./data/cru_ts3.22.1901.2013.tmn.dat.nc")
tmn <- setZ(tmn, tm, "month")
tmx <- brick("./data/cru_ts3.22.1901.2013.tmx.dat.nc")
tmx <- setZ(tmx, tm, "month")
tmp <- brick("./data/cru_ts3.22.1901.2013.tmp.dat.nc")
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

write.dta(bio, "./data/DSG/gha-glss5_L2_bio_month.dta", version=12L)


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
writeOGR(gha, "./data/DSG", "gha-glss5_L2_bio", "ESRI Shapefile", overwrite=T)

# Export to STATA
attr(gha.dt, "var.labels") <- c("ISO3 code",
  "GLSS5 district code", "GLSS5 district", "Capital city", "Region",
  vi[names(gha.dt)[6:21]][, varLabel],
  "area (sq. km.)", bio.lbl)
write.dta(gha.dt, "./data/DSG/gha-glss5_L2_bio.dta", version=10L)



#####################################################################################
# 2015.07.28 GlobeLand30 land cover classes across GLSS6 survey districts
#####################################################################################

memory.limit(7000)
library(tmap)

gha <- readOGR("./maps", "gha-glss6-svyMap")

# Load forest cover (from GlobeLand30)
lc <- raster("~/Maps/GLC30/CRLANDUSE/crlu30m")

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
pre <- brick("./rainfall/data/cru_ts3.22.1901.2013.pre.dat.nc")
pre <- setZ(pre, tm, "month")
tmn <- brick("./rainfall/data/cru_ts3.22.1901.2013.tmn.dat.nc")
tmn <- setZ(tmn, tm, "month")
tmx <- brick("./rainfall/data/cru_ts3.22.1901.2013.tmx.dat.nc")
tmx <- setZ(tmx, tm, "month")

# Load SSA map and check
g0 <- readRDS("../cell5m/rdb/g0.rds")
plot(g0)
plot(g0[g0$ADM0_NAME=="Ethiopia",])

# Keep only SSA countries
ssa <- fread("../cell5m/rdb/ssa.csv")
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
write.dta(tmp, "./rainfall/data/SB/ssa_L0_1960-2013_yearly_bio.dta", version=10L)

# Save all
save.image(file="./rainfall/data/SB/ssa_L0_1960-2013_yearly_bio.RData")


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



#####################################################################################
# 2015.09.19 Process biovars across AR Malawi households
#####################################################################################

setwd("/home/projects/shiny/rainfall")

# Import AR village locations
ar <- readRDS("./data/MWI-HH-Coordinates.rds")


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
# [1] 1131   19   54

names(dimnames(bio)) <- c("hhid", "biovars", "year")

# Finaly compute long-term mean for each district
bio <- apply(bio, c("hhid", "biovars"), mean, na.rm=T)
bio <- data.frame(bio)
ar@data <- cbind(ar@data, bio)

# Save
saveRDS(ar, "./data/MWI-HH-Coordinates_bio.rds")



#####################################################################################
# 2015.10.25 Process biovars across district survey maps
#####################################################################################
setwd("/home/projects/shiny/rainfall")

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

# Intersect with districts map
l2.map <- readOGR("./r15.10", "svyL2Maps_r1")
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

write.dta(bio, "./r15.10/svyL2Maps-CRU.3.22_1960-2013_r1.dta", version=12L)
write.dta(bio.svy, "./r15.10/svyL2Maps-CRU.3.22_2000-2010_year_r1.dta", version=12L)
write.dta(bio.mth, "./r15.10/svyL2Maps-CRU.3.22.dta_1960-2013_month_r1.dta", version=12L)


# Save all
save.image(file="./data/r15.10/svyL2Maps.RData")



#####################################################################################
# 2015.10.30 Simplify watershed boundaries for Carleen
#####################################################################################

library(rgdal)
library(rgeos)
library(leaflet)

setwd("/home/projects/shiny/rainfall")

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
setwd("/home/projects/shiny/rainfall")
load("./data/r15.10/svyL2Maps.RData")

library(data.table)
library(reshape2)
library(dismo)
library(foreign)

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
write.dta(bio.svy, "./data/r15.10/svyL2Maps-CRU.3.22_1990-2013_year_r1.dta", version=12L)


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
write.dta(bio.wc.l2, "./data/r15.10/svyL2Maps-WorldClim-1km_1950-2000_r1.dta", version=12L)


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
write.dta(bio.wc.mth, "./data/r15.10/svyL2Maps-WorldClim-1km_month_1950-2000_r1.dta", version=12L)


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
write.dta(pdsi.svy, "./data/r15.10/svyL2Maps-PDSI-50km_1990-2012_r1.dta", version=12L)




# Save all
save.image(file="./data/r15.10/svyL2Maps.RData")

