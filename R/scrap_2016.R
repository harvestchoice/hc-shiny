#####################################################################################
# 2016.07.02 Reporjecting rasters across the antemeridian
#####################################################################################

library(raster)
library(rgdal)

setwd("~/")

# Download geoTIFF
BS.terra <- curl::curl_download("https://lance.modaps.eosdis.nasa.gov/imagery/subsets/?subset=BeringSea.2016182.terra.721.2km.tif",
  "BeringSea.2016182.terra.721.2km.tif")
BS.terra <- brick("BeringSea.2016182.terra.721.2km.tif")

# Reproject using `gdalwarp`
system("gdalwarp -overwrite -s_srs EPSG:4326 -t_srs EPSG:3035 -multi -of GTiff BeringSea.20e16182.terra.721.2km.tif BeringSea.2016182.terra.721.2km.azi.tif")

# Load reprojected raster
BS.terra.sa1 <- brick("BeringSea.2016182.terra.721.2km.azi.tif")

# Reproject with projectRaster()
BS.terra.sa2 <- projectRaster(from=BS.terra, crs=CRS("+init=epsg:3035"))

# Plot
par(mfrow=c(2,1))
plotRGB(BS.terra)
plotRGB(BS.terra.sa1)

# This one errors out
plotRGB(BS.terra.sa2)
# Error in grDevices::rgb(RGB[, 1], RGB[, 2], RGB[, 3], alpha = alpha, max = scale) :
#  color intensity -0.00130533, not in [0,1]

plot(BS.terra.sa2[[1]])
