# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Last update: 18 Jul 2022

# Creating a richness map for one services

# Required packages

library(maptools)
library(rgdal)
library(sp)
library(raster)
library(reshape)
library(rgeos)
library(maptools)
library(dplyr)
library(tidyverse)



# Open some objects to use later ------------------------------------------

Ams <- raster('./ENM/env_cropped/present/_wc2.1_10m_bio_1.tif')
BR <- shapefile("./Shp/Brasil_wgs84_diss.shp")
SP <- shapefile("./Shp/SP.shp")

# Each species for rodent control services --------------------------------

# Reading species names of ecosystem services 

sp_Es  <- read.csv("./tables/eco_services_binary.csv", stringsAsFactors = FALSE)

sp_Es$genus_epithet_IUCN <-
  gsub(x = sp_Es$genus_epithet_IUCN,
       pattern = " ",
       replacement = "_")

sp_Es_sub  <- sp_Es %>%
  subset(PESTS_AND_DISEASES_REGULATION == 1)

splist  <- read.csv("./Table_results/12_sp_table_S1_less15onlycharis.csv", 
                    stringsAsFactors = FALSE) %>%
  pull(Species)

sp_Es_sub_occ  <- sp_Es_sub %>%
  subset(sp_Es_sub$genus_epithet_IUCN %in% splist)

# Outliers

splist2  <- read.csv("./Table_results/12_sp_table_S1_less15onlycharis.csv",
                     stringsAsFactors = FALSE)%>%
  subset(outliers == 0) %>%
  pull(Species)

sp_Es_sub_occ2  <- sp_Es_sub_occ %>%
  subset(sp_Es_sub_occ$genus_epithet_IUCN %in% splist2)

# Remaining species

sp_names  <- sp_Es_sub_occ2 %>%
  pull(genus_epithet_IUCN)

# Rasters of current and future -------------------------------------------

cu_cont <- list()
cu_bin <- list()
fu_cont <- list()
fu_bin <- list()

for(i in 1:length(sp_names)){
  
  cu_cont[[i]] <- raster(paste0("./ENM/outputs/", sp_names[i], "/results/", 
                                "CUR.cont_", sp_names[i], ".asc"))
  
  cu_cont[[i]] <- resample(cu_cont[[i]], Ams)
  cu_cont[[i]][is.na(cu_cont[[i]])] <- 0
  
  cu_bin[[i]] <- raster(paste0("./ENM/outputs/", sp_names[i], "/results/", 
                               "CUR.bin_", sp_names[i], '.asc'))
  
  cu_bin[[i]] <- resample(cu_bin[[i]], Ams)
  cu_bin[[i]][cu_bin[[i]] >= 1] <- 1 
  cu_bin[[i]][is.na(cu_bin[[i]])] <- 0
  
  fu_cont[[i]] <- raster(paste0("./ENM/outputs/", sp_names[i], "/results/", 
                                '/Fut_bcc.cont_', sp_names[i], '.asc'))
  
  fu_cont[[i]] <- resample(fu_cont[[i]], Ams)
  fu_cont[[i]][is.na(fu_cont[[i]])] <- 0
  
  
  fu_bin[[i]] <- raster(paste0("./ENM/outputs/", sp_names[i], "/results/", 
                               '/Fut_bcc.bin_', sp_names[i], '.asc'))
  
  fu_bin[[i]] <- resample(fu_bin[[i]], Ams)
  fu_bin[[i]][fu_bin[[i]] >= 1] <- 1
  fu_bin[[i]][is.na(fu_bin[[i]])] <- 0
  
}

cu_cont_ecoser_rich <- Reduce('+', cu_cont)
cu_cont_ecoser_rich_BR <- crop(cu_cont_ecoser_rich, BR)
cu_cont_ecoser_rich_BR <- mask(cu_cont_ecoser_rich_BR, BR)
cu_cont_ecoser_rich_SP <- crop(cu_cont_ecoser_rich, SP)
cu_cont_ecoser_rich_SP <- mask(cu_cont_ecoser_rich_SP, SP)

cu_bin_ecoser_rich <- Reduce('+', cu_bin)
cu_bin_ecoser_rich_BR <- crop(cu_bin_ecoser_rich, BR)
cu_bin_ecoser_rich_BR <- mask(cu_bin_ecoser_rich_BR, BR)
cu_bin_ecoser_rich_SP <- crop(cu_bin_ecoser_rich, SP)
cu_bin_ecoser_rich_SP <- mask(cu_bin_ecoser_rich_SP, SP)

fu_cont_ecoser_rich <- Reduce('+', fu_cont)
fu_cont_ecoser_rich_BR <- crop(fu_cont_ecoser_rich, BR)
fu_cont_ecoser_rich_BR <- mask(fu_cont_ecoser_rich_BR, BR)
fu_cont_ecoser_rich_SP <- crop(fu_cont_ecoser_rich, SP)
fu_cont_ecoser_rich_SP <- mask(fu_cont_ecoser_rich_SP, SP)

fu_bin_ecoser_rich <- Reduce('+', fu_bin)
fu_bin_ecoser_rich_BR <- crop(fu_bin_ecoser_rich, BR)
fu_bin_ecoser_rich_BR <- mask(fu_bin_ecoser_rich_BR, BR)
fu_bin_ecoser_rich_SP <- crop(fu_bin_ecoser_rich, SP)
fu_bin_ecoser_rich_SP <- mask(fu_bin_ecoser_rich_SP, SP)


# Difference

fu_cu_cont <- fu_cont_ecoser_rich_BR - cu_cont_ecoser_rich_BR
fu_cu_bin <- fu_bin_ecoser_rich_BR - cu_bin_ecoser_rich_BR
fu_cu_cont_SP <- fu_cont_ecoser_rich_SP - cu_cont_ecoser_rich_SP
fu_cu_bin_SP <- fu_bin_ecoser_rich_SP - cu_bin_ecoser_rich_SP

# Saving

writeRaster(cu_cont_ecoser_rich, 
            paste0("./Diversity/Richness/Ams/cu_cont_noout_pest_Ams.tif"),
            format="GTiff", overwrite=T)

writeRaster(cu_cont_ecoser_rich_BR, 
            paste0("./Diversity/Richness/BR_noout/cu_cont_noout_pest.tif"),
            format="GTiff", overwrite=T)

writeRaster(cu_cont_ecoser_rich_SP, 
            paste0("./Diversity/Richness/SP_noout/cu_cont_noout_pest.tif"),
            format="GTiff", overwrite=T)

writeRaster(cu_bin_ecoser_rich, 
            paste0("./Diversity/Richness/Ams/cu_bin_noout_pest_Ams.tif"),
            format="GTiff", overwrite=T)

writeRaster(cu_bin_ecoser_rich_BR, 
            paste0("./Diversity/Richness/BR_noout/cu_bin_noout_pest.tif"),
            format="GTiff", overwrite=T)

writeRaster(cu_bin_ecoser_rich_SP, 
            paste0("./Diversity/Richness/SP_noout/cu_bin_noout_pest.tif"),
            format="GTiff", overwrite=T)

writeRaster(fu_cont_ecoser_rich, 
            paste0("./Diversity/Richness/Ams/fu_cont_noout_pest_Ams.tif"),
            format="GTiff", overwrite=T)

writeRaster(fu_cont_ecoser_rich_BR, 
            paste0("./Diversity/Richness/BR_noout/fu_cont_noout_pest.tif"),
            format="GTiff", overwrite=T)

writeRaster(fu_cont_ecoser_rich_SP, 
            paste0("./Diversity/Richness/SP_noout/fu_cont_noout_pest.tif"),
            format="GTiff", overwrite=T)

writeRaster(fu_bin_ecoser_rich, 
            paste0("./Diversity/Richness/Ams/fu_bin_noout_pest_Ams.tif"),
            format="GTiff", overwrite=T)

writeRaster(fu_bin_ecoser_rich_BR, 
            paste0("./Diversity/Richness/BR_noout/fu_bin_noout_pest.tif"),
            format="GTiff", overwrite=T)

writeRaster(fu_bin_ecoser_rich_SP, 
            paste0("./Diversity/Richness/SP_noout/fu_bin_noout_pest.tif"),
            format="GTiff", overwrite=T)

writeRaster(fu_cu_cont, 
            paste0("./Diversity/Richness/Diff_BR_noout/fu_cu_cont_noout_pest.tif"),
            format="GTiff", overwrite=T)

writeRaster(fu_cu_bin, 
            paste0("./Diversity/Richness/Diff_BR_noout/fu_cu_bin_noout_pest.tif"),
            format="GTiff", overwrite=T)

writeRaster(fu_cu_cont_SP, 
            paste0("./Diversity/Richness/Diff_SP_noout/fu_cu_cont_noout_pest.tif"),
            format="GTiff", overwrite=T)

writeRaster(fu_cu_bin_SP, 
            paste0("./Diversity/Richness/Diff_SP_noout/fu_cu_bin_noout_pest.tif"),
            format="GTiff", overwrite=T)