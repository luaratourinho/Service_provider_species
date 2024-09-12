# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Last update: 19 Jul 2022

# Creating a richness map for all services

# Required packages

library(tidyverse)
library(dplyr)
library(raster)
library(rgdal)
library(rgeos)

# Read your ES list

list.var <-
  list.files(paste0("./Diversity/Richness/Diff_BR"),
             recursive = TRUE,
             pattern = "tif",
             full.names = TRUE
  )

list.var_2 <-
  list.files(paste0("./Diversity/Richness/Diff_BR_noout"),
             recursive = TRUE,
             pattern = "tif",
             full.names = TRUE
  )

ES <- c("charism","ecotour","carrion","pollin","top_pred","dispers",
        "pest","nutrient","engineers","rodent_ctrl","sentinel", "ES")


n <-length(ES)

# Table

tab.mud <- matrix(nrow = n, ncol = 8)
colnames(tab.mud) <- c("ES", "stability", "loss", "gain", "total", 
                       "prop_stability", "prop_loss", "prop_gain")

# With outliers

for(i in 1:n){
  
fu_cu_bin_list <- str_subset(list.var, 
                             paste0("./Diversity/Richness/Diff_BR/fu_cu_bin_",
                                    ES[i],".tif"))

fu_cu_bin_rst<- raster(fu_cu_bin_list)


delta.serv.vals <- na.omit(values(fu_cu_bin_rst))
summary(delta.serv.vals)
table(delta.serv.vals)

# Loss, gain and stability values for delta

loss <- length(which(delta.serv.vals < 0))
stability <-  length(which(delta.serv.vals == 0))
gain <- length(which(delta.serv.vals > 0))
total <- length(delta.serv.vals)
prop_loss <- (loss/total)*100
prop_stability <- (stability/total)*100
prop_gain <- (gain/total)*100


tab.mud[i, ] <- cbind(ES[i],stability, loss, gain, total, 
                      prop_stability, prop_loss, prop_gain)


# Loss/Gain/Stability map - "trinary"

fu_cu_tri <- fu_cu_bin_rst
fu_cu_tri
fu_cu_tri[fu_cu_tri < 0] <- -1
fu_cu_tri[fu_cu_tri > 0] <- 1

writeRaster(fu_cu_tri, paste0("./Diversity/delta/", "diff_tri_", ES[i],
                                         ".tif"),
            format="GTiff", overwrite=T)

}

write.csv(tab.mud, "./Table_results/13c_ES_tab.mud.csv", row.names = F)


# Without outliers

# Table

tab.mud2 <- matrix(nrow = n, ncol = 8)
colnames(tab.mud2) <- c("ES", "stability", "loss", "gain", "total", 
                       "prop_stability", "prop_loss", "prop_gain")

# With outliers

for(i in 1:n){
  
  fu_cu_bin_list <- str_subset(list.var_2, 
                               paste0("./Diversity/Richness/Diff_BR_noout/fu_cu_bin_noout_",
                                      ES[i],".tif"))
  
  fu_cu_bin_rst<- raster(fu_cu_bin_list)
  
  
  delta.serv.vals <- na.omit(values(fu_cu_bin_rst))
  summary(delta.serv.vals)
  table(delta.serv.vals)
  
  # Loss, gain and stability values for delta
  
  loss <- length(which(delta.serv.vals < 0))
  stability <-  length(which(delta.serv.vals == 0))
  gain <- length(which(delta.serv.vals > 0))
  total <- length(delta.serv.vals)
  prop_loss <- (loss/total)*100
  prop_stability <- (stability/total)*100
  prop_gain <- (gain/total)*100
  
  
  tab.mud2[i, ] <- cbind(ES[i],stability, loss, gain, total, 
                        prop_stability, prop_loss, prop_gain)
  
  
  # Loss/Gain/Stability map - "trinary"
  
  fu_cu_tri <- fu_cu_bin_rst
  fu_cu_tri
  fu_cu_tri[fu_cu_tri < 0] <- -1
  fu_cu_tri[fu_cu_tri > 0] <- 1
  
  writeRaster(fu_cu_tri, paste0("./Diversity/delta_noout/", "diff_tri_noout_", ES[i],
                                ".tif"),
              format="GTiff", overwrite=T)
  
  
}

write.csv(tab.mud2, "./Table_results/13c_ES_tab.mud_noout.csv", row.names = F)

