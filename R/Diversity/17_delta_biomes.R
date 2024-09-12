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

# Shapefile of the Brazilian phytogeographic domains (a.k.a. biomes)

domains <- shapefile("./Shp/BR_BIOMAS_IBGE.shp")

amaz <- subset(domains, CD_LEGENDA == "AMAZÔNIA")
amaz <- aggregate(amaz, dissolve = T)
mata <- subset(domains, CD_LEGENDA == "MATA ATLÂNTICA")
mata <- aggregate(mata, dissolve = T)
pamp <- subset(domains, CD_LEGENDA == "PAMPA")
pamp <- aggregate(pamp, dissolve = T)
cerr <- subset(domains, CD_LEGENDA == "CERRADO")
cerr <- aggregate(cerr, dissolve = T)
pant <- subset(domains, CD_LEGENDA == "PANTANAL")
pant <- aggregate(pant, dissolve = T)
caat <- subset(domains, CD_LEGENDA == "CAATINGA")
caat <- aggregate(caat, dissolve = T)



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

tab.mud <- matrix(nrow = n, ncol = 19)
colnames(tab.mud) <- c("ES", 
                       "prop_loss_amaz", "prop_stability_amaz", "prop_gain_amaz", 
                       "prop_loss_mata", "prop_stability_mata", "prop_gain_mata",
                       "prop_loss_pamp", "prop_stability_pamp", "prop_gain_pamp",
                       "prop_loss_cerr", "prop_stability_cerr", "prop_gain_cerr",
                       "prop_loss_pant", "prop_stability_pant", "prop_gain_pant",
                       "prop_loss_caat", "prop_stability_caat", "prop_gain_caat")

# With outliers

for(i in 1:n){
  
  fu_cu_bin_list <- str_subset(list.var, 
                               paste0("./Diversity/Richness/Diff_BR/fu_cu_bin_",
                                      ES[i],".tif"))
  
  fu_cu_bin_rst<- raster(fu_cu_bin_list)
  
  
  # Crop and mask for domain extension 
  
  amaz_rst <- crop(fu_cu_bin_rst, amaz)
  amaz_rst <- mask(amaz_rst, amaz)
  mata_rst <- crop(fu_cu_bin_rst, mata)
  mata_rst <- mask(mata_rst, mata)
  pamp_rst <- crop(fu_cu_bin_rst, pamp)
  pamp_rst <- mask(pamp_rst, pamp)
  cerr_rst <- crop(fu_cu_bin_rst, cerr)
  cerr_rst <- mask(cerr_rst, cerr)
  pant_rst <- crop(fu_cu_bin_rst, pant)
  pant_rst <- mask(pant_rst, pant)
  caat_rst <- crop(fu_cu_bin_rst, caat)
  caat_rst <- mask(caat_rst, caat)
  
  # Loss, gain and stability values for delta
  
  # Amazonia
  delta <- na.omit(values(amaz_rst))
  loss <- length(which(delta < 0))
  stability <-  length(which(delta == 0))
  gain <- length(which(delta > 0))
  total <- length(delta)
  
  prop_loss_amaz <- (loss/total)*100
  prop_stability_amaz <- (stability/total)*100
  prop_gain_amaz <- (gain/total)*100
  
  # Mata Atlantica
  delta <- na.omit(values(mata_rst))
  loss <- length(which(delta < 0))
  stability <-  length(which(delta == 0))
  gain <- length(which(delta > 0))
  total <- length(delta)
  
  prop_loss_mata <- (loss/total)*100
  prop_stability_mata <- (stability/total)*100
  prop_gain_mata <- (gain/total)*100
  
  # Pampas
  delta <- na.omit(values(pamp_rst))
  loss <- length(which(delta < 0))
  stability <-  length(which(delta == 0))
  gain <- length(which(delta > 0))
  total <- length(delta)
  
  prop_loss_pamp <- (loss/total)*100
  prop_stability_pamp <- (stability/total)*100
  prop_gain_pamp <- (gain/total)*100
  
  # Cerrado
  delta <- na.omit(values(cerr_rst))
  loss <- length(which(delta < 0))
  stability <-  length(which(delta == 0))
  gain <- length(which(delta > 0))
  total <- length(delta)
  
  prop_loss_cerr <- (loss/total)*100
  prop_stability_cerr <- (stability/total)*100
  prop_gain_cerr <- (gain/total)*100
  
  # Pantanal
  delta <- na.omit(values(pant_rst))
  loss <- length(which(delta < 0))
  stability <-  length(which(delta == 0))
  gain <- length(which(delta > 0))
  total <- length(delta)
  
  prop_loss_pant <- (loss/total)*100
  prop_stability_pant <- (stability/total)*100
  prop_gain_pant <- (gain/total)*100
  
  # Caatinga
  delta <- na.omit(values(caat_rst))
  loss <- length(which(delta < 0))
  stability <-  length(which(delta == 0))
  gain <- length(which(delta > 0))
  total <- length(delta)
  
  prop_loss_caat <- (loss/total)*100
  prop_stability_caat <- (stability/total)*100
  prop_gain_caat <- (gain/total)*100
  
  
  tab.mud[i, ] <- cbind(ES[i],
                        prop_loss_amaz, prop_stability_amaz, prop_gain_amaz, 
                        prop_loss_mata, prop_stability_mata, prop_gain_mata,
                        prop_loss_pamp, prop_stability_pamp, prop_gain_pamp,
                        prop_loss_cerr, prop_stability_cerr, prop_gain_cerr,
                        prop_loss_pant, prop_stability_pant, prop_gain_pant,
                        prop_loss_caat, prop_stability_caat, prop_gain_caat)
  
  

  
}

write.csv(tab.mud, "./Table_results/17_ES_tab.mud_domains.csv", row.names = F)


# Without outliers

# Table

tab.mud_2 <- matrix(nrow = n, ncol = 19)
colnames(tab.mud_2) <- c("ES", 
                       "prop_loss_amaz", "prop_stability_amaz", "prop_gain_amaz", 
                       "prop_loss_mata", "prop_stability_mata", "prop_gain_mata",
                       "prop_loss_pamp", "prop_stability_pamp", "prop_gain_pamp",
                       "prop_loss_cerr", "prop_stability_cerr", "prop_gain_cerr",
                       "prop_loss_pant", "prop_stability_pant", "prop_gain_pant",
                       "prop_loss_caat", "prop_stability_caat", "prop_gain_caat")

# With outliers

for(i in 1:n){
  
  fu_cu_bin_list <- str_subset(list.var_2, 
                               paste0("./Diversity/Richness/Diff_BR_noout/fu_cu_bin_noout_",
                                      ES[i],".tif"))
  
  fu_cu_bin_rst<- raster(fu_cu_bin_list)
  
  # Crop and mask for domain extension 
  
  amaz_rst <- crop(fu_cu_bin_rst, amaz)
  amaz_rst <- mask(amaz_rst, amaz)
  mata_rst <- crop(fu_cu_bin_rst, mata)
  mata_rst <- mask(mata_rst, mata)
  pamp_rst <- crop(fu_cu_bin_rst, pamp)
  pamp_rst <- mask(pamp_rst, pamp)
  cerr_rst <- crop(fu_cu_bin_rst, cerr)
  cerr_rst <- mask(cerr_rst, cerr)
  pant_rst <- crop(fu_cu_bin_rst, pant)
  pant_rst <- mask(pant_rst, pant)
  caat_rst <- crop(fu_cu_bin_rst, caat)
  caat_rst <- mask(caat_rst, caat)
  
  # Loss, gain and stability values for delta
  
  # Amazonia
  delta <- na.omit(values(amaz_rst))
  loss <- length(which(delta < 0))
  stability <-  length(which(delta == 0))
  gain <- length(which(delta > 0))
  total <- length(delta)
  
  prop_loss_amaz <- (loss/total)*100
  prop_stability_amaz <- (stability/total)*100
  prop_gain_amaz <- (gain/total)*100
  
  # Mata Atlantica
  delta <- na.omit(values(mata_rst))
  loss <- length(which(delta < 0))
  stability <-  length(which(delta == 0))
  gain <- length(which(delta > 0))
  total <- length(delta)
  
  prop_loss_mata <- (loss/total)*100
  prop_stability_mata <- (stability/total)*100
  prop_gain_mata <- (gain/total)*100
  
  # Pampas
  delta <- na.omit(values(pamp_rst))
  loss <- length(which(delta < 0))
  stability <-  length(which(delta == 0))
  gain <- length(which(delta > 0))
  total <- length(delta)
  
  prop_loss_pamp <- (loss/total)*100
  prop_stability_pamp <- (stability/total)*100
  prop_gain_pamp <- (gain/total)*100
  
  # Cerrado
  delta <- na.omit(values(cerr_rst))
  loss <- length(which(delta < 0))
  stability <-  length(which(delta == 0))
  gain <- length(which(delta > 0))
  total <- length(delta)
  
  prop_loss_cerr <- (loss/total)*100
  prop_stability_cerr <- (stability/total)*100
  prop_gain_cerr <- (gain/total)*100
  
  # Pantanal
  delta <- na.omit(values(pant_rst))
  loss <- length(which(delta < 0))
  stability <-  length(which(delta == 0))
  gain <- length(which(delta > 0))
  total <- length(delta)
  
  prop_loss_pant <- (loss/total)*100
  prop_stability_pant <- (stability/total)*100
  prop_gain_pant <- (gain/total)*100
  
  # Caatinga
  delta <- na.omit(values(caat_rst))
  loss <- length(which(delta < 0))
  stability <-  length(which(delta == 0))
  gain <- length(which(delta > 0))
  total <- length(delta)
  
  prop_loss_caat <- (loss/total)*100
  prop_stability_caat <- (stability/total)*100
  prop_gain_caat <- (gain/total)*100
  
  
  tab.mud_2[i, ] <- cbind(ES[i],
                        prop_loss_amaz, prop_stability_amaz, prop_gain_amaz, 
                        prop_loss_mata, prop_stability_mata, prop_gain_mata,
                        prop_loss_pamp, prop_stability_pamp, prop_gain_pamp,
                        prop_loss_cerr, prop_stability_cerr, prop_gain_cerr,
                        prop_loss_pant, prop_stability_pant, prop_gain_pant,
                        prop_loss_caat, prop_stability_caat, prop_gain_caat)
  
  
}

write.csv(tab.mud_2, "./Table_results/17_ES_tab.mud_domains_noout.csv", row.names = F)
