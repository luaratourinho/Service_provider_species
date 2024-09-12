
# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Last update: 22 Feb 2022


# Required packages

library(tidyverse)
library(dplyr)
library(raster)


# Reading file with species names

sp.names <- read_csv("./data/eco_relationship.csv")

# Dispensers

disp <- sp.names %>%
  group_by(eco_relationship) %>%
  filter(eco_relationship == 'dispersor') %>%
  pull(species)

n <-length(disp)


# Predators

pred <- sp.names %>%
  group_by(eco_relationship) %>%
  filter(eco_relationship == 'predator') %>%
  pull(species)

pred <- pred[-12]

n_p <-length(pred)



# Reading Euterpe maps to crop other species continuous maps
cu_eut <- raster(paste0("./outputs/", "Euterpe_edulis", "/results", 
                        "/Diff_raster/", "Euterpe_edulis_cur_bin.tif"))
fu_eut <- raster(paste0("./outputs/", "Euterpe_edulis", "/results",
                        "/Diff_raster/", "Euterpe_edulis_fut_bin.tif"))


# Creating file empty lists

cu <- list()
fu <- list()
cu_cont <- list()
fu_cont <- list()
cu_crop_cont <- list()
fu_crop_cont <- list()


# Reading rasters of dispensers -------------------------------------------

sp.names = disp

for(i in 1:n){
  
cu[[i]] <- raster(paste0("./outputs/", sp.names[i], "/results", "/Diff_raster/", 
                         sp.names[i], "_cu_by_Eut.tif"))
cu[[i]][(cu[[i]])<1] <- 0
cu[[i]][is.na(cu[[i]])] <- 0

fu[[i]] <- raster(paste0("./outputs/", sp.names[i], "/results", "/Diff_raster/", 
                         sp.names[i], "_fu_by_Eut.tif"))
fu[[i]][(fu[[i]])<1] <- 0
fu[[i]][is.na(fu[[i]])] <- 0

cu_cont[[i]] <- raster(paste0("./outputs/", sp.names[i], "/results","/CUR.cont_", sp.names[i], ".asc"))
cu_crop_cont[[i]] <- resample(cu_cont[[i]], cu_eut, method='bilinear')
cu_crop_cont[[i]] <- mask(cu_crop_cont[[i]], cu_eut)
cu_crop_cont[[i]][is.na(cu_crop_cont[[i]])] <- 0

fu_cont[[i]] <- raster(paste0("./outputs/", sp.names[i], "/results","/Fut_all.cont_", sp.names[i], ".asc"))
fu_crop_cont[[i]] <- resample(fu_cont[[i]], fu_eut, method='bilinear')
fu_crop_cont[[i]] <- mask(fu_crop_cont[[i]], fu_eut)
fu_crop_cont[[i]][is.na(fu_crop_cont[[i]])] <- 0

}


# Richness maps of disperser species ------------------------------------------

cu_rich_disp <- Reduce('+', cu)
cu_rich_disp <- mask(cu_rich_disp, cu_eut)
# OR
# rs <- stack(cu)
# cu_rich2 <- calc(rs, sum)
fu_rich_disp <- Reduce('+', fu)
fu_rich_disp <- mask(fu_rich_disp, fu_eut)
cu_cont_rich_disp <- Reduce('+', cu_crop_cont)
cu_cont_rich_disp <- mask(cu_cont_rich_disp, cu_eut)
fu_cont_rich_disp <- Reduce('+', fu_crop_cont)
fu_cont_rich_disp <- mask(fu_cont_rich_disp, fu_eut)

writeRaster(cu_rich_disp, filename = paste0("./Richness/cu_rich_disp.tif"),
            format="GTiff", overwrite=T)
writeRaster(fu_rich_disp, filename = paste0("./Richness/fu_rich_disp.tif"),
            format="GTiff", overwrite=T)
writeRaster(cu_cont_rich_disp, filename = paste0("./Richness/cu_cont_rich_disp.tif"),
            format="GTiff", overwrite=T)
writeRaster(fu_cont_rich_disp, filename = paste0("./Richness/fu_cont_rich_disp.tif"),
            format="GTiff", overwrite=T)



# Reading rasters of predators -------------------------------------------

sp.names = pred

for(i in 1:n_p){
  
  cu[[i]] <- raster(paste0("./outputs/", sp.names[i], "/results", "/Diff_raster/", 
                           sp.names[i], "_cu_by_Eut.tif"))
  cu[[i]][(cu[[i]])<1] <- 0
  cu[[i]][is.na(cu[[i]])] <- 0
  
  fu[[i]] <- raster(paste0("./outputs/", sp.names[i], "/results", "/Diff_raster/", 
                           sp.names[i], "_fu_by_Eut.tif"))
  fu[[i]][(fu[[i]])<1] <- 0
  fu[[i]][is.na(fu[[i]])] <- 0
  
  cu_cont[[i]] <- raster(paste0("./outputs/", sp.names[i], "/results","/CUR.cont_", sp.names[i], ".asc"))
  cu_crop_cont[[i]] <- resample(cu_cont[[i]], cu_eut, method='bilinear')
  cu_crop_cont[[i]] <- mask(cu_crop_cont[[i]], cu_eut)
  cu_crop_cont[[i]][is.na(cu_crop_cont[[i]])] <- 0
  
  fu_cont[[i]] <- raster(paste0("./outputs/", sp.names[i], "/results","/Fut_all.cont_", sp.names[i], ".asc"))
  fu_crop_cont[[i]] <- resample(fu_cont[[i]], fu_eut, method='bilinear')
  fu_crop_cont[[i]] <- mask(fu_crop_cont[[i]], fu_eut)
  fu_crop_cont[[i]][is.na(fu_crop_cont[[i]])] <- 0
  
}


# Richness maps of predators ----------------------------------------------

cu_rich_pred <- Reduce('+', cu)
cu_rich_pred <- mask(cu_rich_pred, cu_eut)
fu_rich_pred <- Reduce('+', fu)
fu_rich_pred <- mask(fu_rich_pred, fu_eut)
cu_cont_rich_pred <- Reduce('+', cu_crop_cont)
cu_cont_rich_pred <- mask(cu_cont_rich_pred, cu_eut)
fu_cont_rich_pred <- Reduce('+', fu_crop_cont)
fu_cont_rich_pred <- mask(fu_cont_rich_pred, fu_eut)

writeRaster(cu_rich_pred, filename = paste0("./Richness/cu_rich_pred.tif"),
            format="GTiff", overwrite=T)
writeRaster(fu_rich_pred, filename = paste0("./Richness/fu_rich_pred.tif"),
            format="GTiff", overwrite=T)
writeRaster(cu_cont_rich_pred, filename = paste0("./Richness/cu_cont_rich_pred.tif"),
            format="GTiff", overwrite=T)
writeRaster(fu_cont_rich_pred, filename = paste0("./Richness/fu_cont_rich_pred.tif"),
            format="GTiff", overwrite=T)


# Min and Max -------------------------------------------------------------

cu_rich_disp # 0, 15  (min, max)
fu_rich_disp # 0, 15  (min, max)
cu_cont_rich_disp # 1.69809, 10.67971  (min, max)
fu_cont_rich_disp # 1.77198, 10.52896  (min, max)

cu_rich_pred # 0, 18  (min, max)
fu_rich_pred # 0, 18  (min, max)
cu_cont_rich_pred # 2.358126, 13.80434  (min, max)
fu_cont_rich_pred # 2.726632, 13.2801  (min, max)


