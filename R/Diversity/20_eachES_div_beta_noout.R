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
BR <- shapefile("./ENM/shp/Brasil_wgs84_diss.shp")

# Each species for rodent control services --------------------------------

# Reading species names of ecosystem services 

sp_Es  <- read.csv("./tables/eco_services_binary.csv", stringsAsFactors = FALSE)

sp_Es$genus_epithet_IUCN <-
  gsub(x = sp_Es$genus_epithet_IUCN,
       pattern = " ",
       replacement = "_")

# CHARISMATIC_AND_OR_CULTURAL_IMPORTANT_SPECIES 
# ECOTOURISM  CARRION_ELIMINATION  POLINIZATION
# TOP_PREDATOR  DISPERSERS  PESTS_AND_DISEASES_REGULATION
# NUTRIENT_TRANSPORT_horizontal_and_vertical
# ENGINEERS  RODENT_REGULATION  SENTINEL_SPECIES_DISEASES 

sp_Es_sub  <- sp_Es %>%
  subset(RODENT_REGULATION == 1)

splist  <- read.csv("./ENM/12_restricted_range_no_outliers.csv", 
                    stringsAsFactors = FALSE) %>%
  pull(Species)

sp_Es_sub_occ  <- sp_Es_sub %>%
  subset(sp_Es_sub$genus_epithet_IUCN %in% splist)

sp_names  <- sp_Es_sub_occ %>%
  pull(genus_epithet_IUCN)

# Rasters of current and future -------------------------------------------

cu_bin <- list()
fu_bin <- list()
cu_bin_BR <- list()
fu_bin_BR <- list()


for(i in 1:length(sp_names)){
  
  cu_bin[[i]] <- raster(paste0("./ENM/outputs/", sp_names[i], "/results/", 
                               "CUR.bin_", sp_names[i], '.asc'))
  
  cu_bin[[i]] <- resample(cu_bin[[i]], Ams)
  cu_bin[[i]][cu_bin[[i]] >= 1] <- 1 
  cu_bin[[i]][is.na(cu_bin[[i]])] <- 0
  
  cu_bin_BR[[i]] <- crop(cu_bin[[i]], BR)
  cu_bin_BR[[i]] <- mask(cu_bin_BR[[i]], BR)
  
  
  fu_bin[[i]] <- raster(paste0("./ENM/outputs/", sp_names[i], "/results/", 
                               '/Fut_bcc.bin_', sp_names[i], '.asc'))
  
  fu_bin[[i]] <- resample(fu_bin[[i]], Ams)
  fu_bin[[i]][fu_bin[[i]] >= 1] <- 1
  fu_bin[[i]][is.na(fu_bin[[i]])] <- 0
  
  fu_bin_BR[[i]] <- crop(fu_bin[[i]], BR)
  fu_bin_BR[[i]] <- mask(fu_bin_BR[[i]], BR)
  
}

servicos.p.cer <- stack(cu_bin_BR)
riq.serv.p.cer <- sum(servicos.p.cer > 0)

servicos.f.cer <- stack(fu_bin_BR)
riq.serv.f.cer <- sum(servicos.f.cer > 0)


# Turnover, Nestedness, Sorense -------------------------------------------

# Calculating turnover, nestedness and sorense of ES between current and future

pres01 <- na.omit(values(as.integer(servicos.p.cer > 0)))
fut01 <- na.omit(values(as.integer(servicos.f.cer > 0)))

serv.vals <-
  values(servicos.p.cer[[1]])
xy <- xyFromCell(servicos.p.cer, 1:ncell(servicos.p.cer))
xy.sem.na <- na.omit(cbind(xy, serv.vals))[,1:2]

a <-
  b <-
  c <-
  a.p <-
  b.p <-
  c.p <-
  min.bc <-
  max.bc <- sum.bc <- beta.sim <- beta.nes <- beta.sor <- NULL

for(k in 1:nrow(pres01)){
  for(l in 1:ncol(pres01)){
    if(pres01[k,l] + fut01[k,l] == 2){
      a.p[l] <- 1
    } else {
      a.p[l] <- 0
    }
    if(pres01[k,l] - fut01[k,l] == 1){
      b.p[l] <- 1
    } else {
      b.p[l] <- 0
    }
    if(pres01[k,l] - fut01[k,l] == -1){
      c.p[l] <- 1
    } else {
      c.p[l] <- 0
    }
  }
  a[k] <- sum(a.p)
  b[k] <- sum(b.p)
  c[k] <- sum(c.p)
  
  min.bc[k] <- min(b[k], c[k])
  max.bc[k] <- max(b[k], c[k])
  sum.bc[k] <- sum(b[k], c[k])
  
  beta.sim[k] <- min.bc[k]/(min.bc[k] + a[k])
  beta.nes[k] <- ((max.bc[k] - min.bc[k]) / (2 * a[k] + sum.bc[k])) * (a[k] / (a[k] + min.bc[k]))
  beta.sor[k] <- sum.bc[k] / (2 * a[k] + sum.bc[k])
} # k (beta)

beta.sim.r <- rasterFromXYZ(cbind(xy.sem.na, beta.sim))
beta.nes.r <- rasterFromXYZ(cbind(xy.sem.na, beta.nes))
beta.sor.r <- rasterFromXYZ(cbind(xy.sem.na, beta.sor))


# Plot beta (nes e turn) and delta

# plot(beta.sim.r, main = paste0('Turnover'))
# plot(beta.nes.r, main = paste0('Nestedness'))
# plot(beta.sor.r, main = paste0('Beta total'))

# Proportion

# beta.sim.r_2 <- beta.sim.r
# beta.sim.r_2[beta.sim.r_2 != 0] <- 1
# high_values <- ncell(which(beta.sim.r_2[] == 1))
# low_values <- ncell(which(beta.sim.r_2[] == 0))
# propor_high <- ((high_values/(high_values + low_values))*100)
# 
# beta.sor.r_2 <- beta.sor.r
# beta.sor.r_2[beta.sor.r_2 != 0] <- 1
# high_values <- ncell(which(beta.sor.r_2[] == 1))
# low_values <- ncell(which(beta.sor.r_2[] == 0))
# propor_high_2 <- ((high_values/(high_values + low_values))*100)
# 
# beta.nes.r_2 <- beta.nes.r
# beta.nes.r_2[beta.nes.r_2 != 0] <- 1
# high_values <- ncell(which(beta.nes.r_2[] == 1))
# low_values <- ncell(which(beta.nes.r_2[] == 0))
# propor_high_3 <- ((high_values/(high_values + low_values))*100)


# Save results

writeRaster(beta.sim.r, 
            "./Diversity/beta/each_ES/noout/beta_sim_rodent_ctrl.tif",
            format="GTiff", overwrite=T)

writeRaster(beta.nes.r, 
            "./Diversity/beta/each_ES/noout/beta_nes_rodent_ctrl.tif",
            format="GTiff", overwrite=T)

writeRaster(beta.sor.r, 
            "./Diversity/beta/each_ES/noout/beta_sor_rodent_ctrl.tif",
            format="GTiff", overwrite=T)



# charism,ecotour,carrion,pollin,top_pred,dispers,
# pest,nutrient,engineers,rodent_ctrl,sentinel
