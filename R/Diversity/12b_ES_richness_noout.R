# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Last update: 18 Jul 2022

# Creating a richness map for all services

# Required packages

library(dplyr)
library(tidyverse)
library(raster)
library(stringr)

list.var <-
  list.files(paste0("./Diversity/Richness/BR_noout"),
             recursive = TRUE,
             pattern = "tif",
             full.names = TRUE
  )


# CONTINUOUS

# Current

cu_cont_list <- str_subset(list.var, "^./Diversity/Richness/BR_noout/cu_cont_noout_")

cu_cont_rst <- stack(cu_cont_list)

cu_cont_norm <- list()

for(i in 1:11) {
  minimo <- min(cu_cont_rst[[i]][], na.rm = T)
  maximo <- max(cu_cont_rst[[i]][], na.rm = T)
  
  normalizar <- function(x) {
    (x - minimo) / (maximo - minimo)
  }
  
  cu_cont_norm[[i]] <- normalizar(cu_cont_rst[[i]])
}

# cu_cont_ES <- stack(cu_cont_norm[])
# cu_cont_ES <- calc(cu_cont_rst, sum) # to sum a stack
cu_cont_ES <- Reduce('+', cu_cont_norm) # to sum a list

writeRaster(cu_cont_ES, 
            paste0("./Diversity/Richness/BR_noout/cu_cont_noout_ES.tif"),
            format="GTiff", overwrite=T)


# Future

fu_cont_list <- str_subset(list.var, "^./Diversity/Richness/BR_noout/fu_cont_noout_")

fu_cont_rst <- stack(fu_cont_list)

fu_cont_norm <- list()

for(i in 1:11) {
  minimo <- min(fu_cont_rst[[i]][], na.rm = T)
  maximo <- max(fu_cont_rst[[i]][], na.rm = T)
  
  normalizar <- function(x) {
    (x - minimo) / (maximo - minimo)
  }
  
  fu_cont_norm[[i]] <- normalizar(fu_cont_rst[[i]])
}

fu_cont_ES <- Reduce('+', fu_cont_norm) # to sum a list

writeRaster(fu_cont_ES, 
            paste0("./Diversity/Richness/BR_noout/fu_cont_noout_ES.tif"),
            format="GTiff", overwrite=T)

# BINARY

# Current

cu_bin_list <- str_subset(list.var, "^./Diversity/Richness/BR_noout/cu_bin_noout_")

cu_bin_rst <- stack(cu_bin_list)

cu_bin_norm <- list()

for(i in 1:11) {
  minimo <- min(cu_bin_rst[[i]][], na.rm = T)
  maximo <- max(cu_bin_rst[[i]][], na.rm = T)
  
  normalizar <- function(x) {
    (x - minimo) / (maximo - minimo)
  }
  
  cu_bin_norm[[i]] <- normalizar(cu_bin_rst[[i]])
}

cu_bin_ES <- Reduce('+', cu_bin_norm) # to sum a list
cu_bin_ES <- round(cu_bin_ES)

writeRaster(cu_bin_ES, 
            paste0("./Diversity/Richness/BR_noout/cu_bin_noout_ES.tif"),
            format="GTiff", overwrite=T)



# Future

fu_bin_list <- str_subset(list.var, "^./Diversity/Richness/BR_noout/fu_bin_noout_")

fu_bin_rst <- stack(fu_bin_list)

fu_bin_norm <- list()

for(i in 1:11) {
  minimo <- min(fu_bin_rst[[i]][], na.rm = T)
  maximo <- max(fu_bin_rst[[i]][], na.rm = T)
  
  normalizar <- function(x) {
    (x - minimo) / (maximo - minimo)
  }
  
  fu_bin_norm[[i]] <- normalizar(fu_bin_rst[[i]])
}

fu_bin_ES <- Reduce('+', fu_bin_norm) # to sum a list
fu_bin_ES <- round(fu_bin_ES)

writeRaster(fu_bin_ES, 
            paste0("./Diversity/Richness/BR_noout/fu_bin_noout_ES.tif"),
            format="GTiff", overwrite=T)


# Differences -------------------------------------------------------------


fu_cu_bin_ES <- fu_bin_ES - cu_bin_ES

fu_cu_cont_ES <- fu_cont_ES - cu_cont_ES

writeRaster(fu_cu_bin_ES, 
            paste0("./Diversity/Richness/Diff_BR_noout/fu_cu_bin_noout_ES.tif"),
            format="GTiff", overwrite=T)

writeRaster(fu_cu_cont_ES, 
            paste0("./Diversity/Richness/Diff_BR_noout/fu_cu_cont_noout_ES.tif"),
            format="GTiff", overwrite=T)

