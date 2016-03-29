#####################################################################################
# Title:   Bioclimatic Estimations
# Date:    Jan. 2016 - Dec. 2016
# Project: HarvestChoice for various
# Author:  Bacou, Melanie <mel@mbacou.com>
#####################################################################################

library(stringr)
library(data.table)
library(reshape2)
library(raster)
library(rgdal)
library(curl)


setwd("/home/projects/hc-data")

# Download latest revision to PDSI
pdsi <- curl_download("http://www.cgd.ucar.edu/cas/catalog/climind/pdsisc.monthly.maps.1850-2014.fawc=1.r2.5x2.5.ipe=2.nc.gz",
  destfile="./PDSI/pdsisc.monthly.maps.1850-2014.nc.gz")
pdsi <- brick(pdsi)
