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

setwd("/home/projects/shiny/rainfall")

## CRU variables are in directory tree
# Note that each 1901-2013 time series is 2.6GB uncrompressed
d <- c("cld", "dtr", "frs", "pet", "pre", "tmn", "tmp", "tmx", "vap", "wet")
f <- "cru_ts3.22.1901.2013.cld.dat.nc.gz"
f <- str_replace(f, "cld", d)
f <- str_replace(f, "cru_ts", paste0(d, "/cru_ts"))

# Retrieve netCDF files one variable at a time
baseurl <- "http://www.cru.uea.ac.uk/cru/data/hrg/cru_ts_3.22/cruts.1406251334.v3.22/"

for (i in 5) {
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
dt <- melt(dt, id.vars=c(names(g) "ID"), variable.name="month", variable.factor=F)
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
# 2015.01.29 Update
#####################################################################################

# Switch to latest FAO GAUL 2015 boundaries
# Switch to rstudio/leaflet R package allowing binding leaflet to sp objects (no need
# to pre-process json lists)
# We only need to pre-process the raster summaries across all districts `d2`





