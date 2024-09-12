
# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Date: 01 abr 2021


# Cropping variables from Worldclim ---------------------------------------


# Required packages 

library(raster)
library(dplyr)
library(rgeos)
library(reshape)


## Current variables ------------------------------------------------------


# Reading rasters
# use pattern = '.tif$' or something else if you have multiple files in this folder
raster_files <- list.files("./ENM/env/current/worldclim/", 
                           full.names = T, 'tif')
head(raster_files)

envi <- stack(raster_files)

# Cropping rasters

# Choose your extention
# All America
envi.cut<-crop(envi, c(-130, -30, -55, 55))
plot(envi.cut[[1]])

# Saving rasters
dir.create(paste0("./ENM/env_cropped/present/", "."))
writeRaster(envi.cut, filename='./ENM/env_cropped/present/', format="GTiff", 
            bylayer=TRUE, suffix="names", overwrite=TRUE)


## Future variables ------------------------------------------------------


# Reading future rasters

# For CMIP6, MIROC6 is one of the best GCM for Americas 
# (see Cannon et al. 2020; https://doi.org/10.1088/1748-9326/ab7e4f)

future_var_stk <- stack("./ENM/env/future/wc2.1_10m_bioc_MIROC6_ssp585_2041-2060.tif")

# Cropping future variables 

envi_fut.cut<- crop(future_var_stk, envi.cut[[1]])
envi_fut.mask<- mask(envi_fut.cut, envi.cut[[1]])

dir.create(paste0("./ENM/env_cropped/future/"))
writeRaster(envi_fut.mask, filename='./ENM/env_cropped/future/', format="GTiff", 
            bylayer=TRUE, suffix="names", overwrite=TRUE)

