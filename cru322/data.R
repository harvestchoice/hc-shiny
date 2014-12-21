#####################################################################################
# Title: Prepare CRU-TS 3.22 
# Date: December 2014
# Project: HarvestChoice
# Author: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

setwd("~/Projects/www/cru322")
library(data.table)
library(raster)
library(stringr)
library(leaflet)

memory.size(6000)

# CRU files are in directory tree

d <- c("cld", "dtr", "frs", "pet", "pre", "tmn", "tmp", "tmx", "vap", "wet")
f <- "cru_ts3.22.1901.2013.cld.dat.nc.gz"
f <- str_replace(f, "cld", d)
f <- str_replace(f, "cru_ts", paste0(d, "/cru_ts"))

# Retrieve netCDF files one variable at a time
baseurl <- "http://www.cru.uea.ac.uk/cru/data/hrg/cru_ts_3.22/cruts.1406251334.v3.22/"

for (i in 1:10) {
  download.file(paste0(baseurl, f[i]), paste0("./data/", basename(f[i])), mode="wb")
  system(paste0("gzip -d ./data/", basename(f[i])))
  assign(d[i], brick(paste0("./data/", str_replace(basename(f[i]), ".gz", ""))))
}

# Crop to country
load("../../cell5m/rdb/g2.rda")
plot(raster(crop(cld, g2[g2$ADM0_NAME=="Ghana",]), layer=1))
plot(g2[g2$ADM0_NAME=="Ghana",], add=T)

# Uses entire extent to crop
cld.gha <- crop(cld, g2[g2$ADM0_NAME=="Ghana",])
tm <- seq(as.Date('1901-01-15'), as.Date('2013-12-16'), 'month')
cld.gha <- setZ(cld.gha, tm, "month")

# We can summarize over time (e.g. quarters)
library(zoo)
cld.gha.mean <- zApply(cld.gha, by=as.yearqtr, fun=mean, name="years", na.rm=T)

# We can further summarize/extract over districts
cld.gha.mean <- extract(cld.gha.mean, g2[g2$ADM0_NAME=="Ghana",], fun=mean, na.rm=T, df=T)

# And plot the results
tmp <- g2[g2$ADM0_NAME=="Ghana",]
tmp@data <- cbind(tmp@data, cld.gha.mean)
spplot(tmp, "X1901.Q1") 
spplot(tmp, "X2012.Q1") 





