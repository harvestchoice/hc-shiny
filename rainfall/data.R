#####################################################################################
# Title: Prepare CRU-TS 3.22
# Date: December 2014
# Project: HarvestChoice
# Author: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

setwd("/home/projects/www/cru322")
library(stringr)
library(raster)

# CRU files are in directory tree
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

# CRU temperature anomalies
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



#####################################################################################
# Simplify GAUL District Boundaries
#####################################################################################

setwd("~/Projects/www")

library(data.table)
library(rgeos)
library(rgdal)

load("../cell5m/rdb/g2.rda")
gha <- g2[g2$ADM0_NAME=="Ghana",]
writeOGR(gha, "./cru322/data", "g2_gha", "ESRI Shapefile")

# Then in the console try 
# $ ogr2ogr -f GeoJSON g2_gha.geojson g2_gha.shp -simplify 0.001

# Load the generated geojson and check
tmp <- readOGR("./cru322/data/g2_gha.geojson", "OGRGeoJSON")
plot(tmp)

# I believe gSimplify uses the same OGR libraries
g2.web <- gSimplify(g2, 0.01, topologyPreserve=T)
g2.web <- SpatialPolygonsDataFrame(g2.web, g2@data)
writeOGR(g2.web, "./cru322/data/g2_web.geojson", "g2008", "GeoJSON")

# Save webified version for re-use
save(g2.web, file="./cru322/data/g2.web.rda", compress=T)

# Create as many geojson files as SSA countries
for (i in unique(g2.web@data$ADM0_CODE)) {
  writeOGR(g2.web[g2.web$ADM0_CODE==i,], paste0("./cru322/data/g2web", i), paste0(i), "GeoJSON")
}

# Create well-formatted country, province, district list for re-use in web apps
d2 <- data.table(g2.web@data)
d2 <- d2[, .N, by=list(ADM0_NAME, ADM1_NAME, ADM2_NAME)]
d2 <- d2[, lapply(.SD, as.character), .SDcols=1:3]
setkey(d2, ADM0_NAME, ADM1_NAME, ADM2_NAME)
d2 <- split(d2, d2$ADM0_NAME)
d2 <- lapply(d2, function(x) x[, list(ADM1_NAME, ADM2_NAME)])
d2 <- lapply(d2, function(x) split(x, x$ADM1_NAME))
d2 <- lapply(d2, function(x) lapply(x, function(y) y$ADM2_NAME))

g2.list <- d2
save(g2.list, file="./cru322/data/g2.list.rda", compress=T)


