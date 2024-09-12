
# Credits ---------------------------

# Script created by

# Luara Tourinho (https://github.com/luaratourinho)

# Last update: 03 Feb 2024

# Removing zero x zero and calculating percentage of change -----------------

# PS.: 
# I ran everything until "13b_", then I jumped to this script
# The next script will be "xxx"
# The maps I ran in QGis


# Required packages
library(dplyr)
library(tidyverse)
library(raster)
library(stringr)

# File requered -------------------------------------------------------------

# Shp of São Paulo State
SP <- shapefile("./Shp_sp/SP.shp")

# Projection WGS84
crs.wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(SP) <- crs.wgs84

# List of Ecosystem Services
ES <- c("carrion","charism","dispers","ecotour","engineers","ES",
        "nutrient","pest","pollin","rodent_ctrl","sentinel","top_pred")

n <-length(ES)

# Creating directories to save results
target_dir <- (paste0("./","Richness/","BR_noout_out0x0_less15char"))
#dir.create(target_dir)
target_dir_2 <- paste0("./","Richness/","SP_noout_out0x0_less15char")
#dir.create(target_dir_2)

# Target directory 
list.var <-
  list.files(paste0("./Diversity/Richness/BR_noout"),
             recursive = TRUE, pattern = "tif", full.names = TRUE)

# List of files (richness in the current and future conditions)
cu_bin_list <- str_subset(list.var, 
                          "./Diversity/Richness/BR_noout/cu_bin_noout_")
fu_bin_list <- str_subset(list.var, 
                          "./Diversity/Richness/BR_noout/fu_bin_noout_")


# Table for results -------------------------------------------------------

Classic_results <- matrix(nrow = n, ncol = 31)
colnames(Classic_results) <- c("ES", "max_spp_cu", "max_spp_fu",
                            "ncell_total", "ncell_gain", "ncell_gain_perc",
                            "ncell_loss", "ncell_loss_perc", "net_value",
                            "ncell_diff_perc","ncell_ter_stable_redun",
                            "ncell_ter_stable_redun_perc", "ncell_ter_gain_redun",
                            "ncell_ter_gain_redun_perc", "ncell_ter_loss_redun",
                            "ncell_ter_loss_redun_perc", "net_value_redun",
                            "ncell_total_SP", "ncell_gain_SP", "ncell_gain_perc_SP",
                            "ncell_loss_SP", "ncell_loss_perc_SP", "net_value_SP",
                            "ncell_diff_perc_SP", "ncell_ter_stable_redun_SP",
                            "ncell_ter_stable_redun_perc_SP", "ncell_ter_gain_redun_SP",
                            "ncell_ter_gain_redun_perc_SP", "ncell_ter_loss_redun_SP",
                            "ncell_ter_loss_redun_perc_SP", "net_value_redun_SP",
                            "increasing_general", "decreasing_general",
                            "increasing_general_SP", "decreasing_general_SP")

# Loop --------------------------------------------------------------------

for(i in 1:n){

  # Reading rst from the list
  cu_bin_rst <- raster(cu_bin_list[[i]])
  
  max_spp_cu <- max(cu_bin_rst[], na.rm = T)
     
  fu_bin_rst <- raster(fu_bin_list[[i]])
  
  max_spp_fu <- max(fu_bin_rst[], na.rm = T)
  
  # Cleaning noises
  cu_bin_rst[cu_bin_rst < 0.9] <- 0
  fu_bin_rst[fu_bin_rst < 0.9] <- 0
  
  # Creating a mask for when current and future are zero
  cu_bin_rst2 = cu_bin_rst
  cu_bin_rst2[cu_bin_rst2 == 0] <- 999
  fu_bin_rst2 = fu_bin_rst
  fu_bin_rst2[fu_bin_rst2 == 0] <- 999
  erase_zerozero <- fu_bin_rst2 + cu_bin_rst2
  erase_zerozero[erase_zerozero < 1998] <- 0
  
  # Future - current without 0 x 0
  diff_bin_rst <- fu_bin_rst - cu_bin_rst
  diff <- diff_bin_rst + erase_zerozero
  diff[diff > 1000] <- NA
  
  diff_SP <- crop(diff, SP)
  diff_SP <- mask(diff_SP, SP)
  
  # Saving
  writeRaster(diff, 
              paste0(target_dir, "/diff_", ES[i],".tif"),
              format="GTiff", overwrite=T)
  
  writeRaster(diff_SP, 
              paste0(target_dir_2, "/diff_", ES[i],".tif"),
              format="GTiff", overwrite=T)

# Calculating percentage of change and highlighting loss and gain ---------

  fu_over_cu <- fu_bin_rst/cu_bin_rst
  
  # There is in future, but not in current
  fu_exist_cu_0 <- fu_over_cu
  fu_exist_cu_0[fu_exist_cu_0 < 1000] <- 0
  fu_exist_cu_0[fu_exist_cu_0 > 1000] <- 1 
  
  # Saving
  writeRaster(fu_exist_cu_0, 
              paste0(target_dir, "/fu_exist_cu_0_", ES[i],".tif"),
              format="GTiff", overwrite=T)
  
  # SP
  fu_exist_cu_0_SP <- crop(fu_exist_cu_0, SP)
  fu_exist_cu_0_SP <- mask(fu_exist_cu_0_SP, SP)
  
  writeRaster(fu_exist_cu_0_SP, 
              paste0(target_dir_2, "/fu_exist_cu_0_", ES[i],".tif"),
              format="GTiff", overwrite=T)
  
  # There is in current, but not in future
  cu_exist_fu_0 <- fu_over_cu
  cu_exist_fu_0[cu_exist_fu_0 != 0] <- 2 
  cu_exist_fu_0[cu_exist_fu_0 == 0] <- 1
  cu_exist_fu_0[cu_exist_fu_0 == 2] <- 0 
  
  # Saving
  writeRaster(cu_exist_fu_0, 
              paste0(target_dir, "/cu_exist_fu_0_", ES[i],".tif"),
              format="GTiff", overwrite=T)
  
  # SP
  cu_exist_fu_0_SP <- crop(cu_exist_fu_0, SP)
  cu_exist_fu_0_SP <- mask(cu_exist_fu_0_SP, SP)
  
  writeRaster(cu_exist_fu_0_SP, 
              paste0(target_dir_2, "/cu_exist_fu_0_", ES[i],".tif"),
              format="GTiff", overwrite=T)
  
  # Changed area only loss and gain truly:
   changed_area <- fu_exist_cu_0 + cu_exist_fu_0
  # Changed area in general is: diff raster

  # Calculating net value
  # Total number of cells with value and for the cells equal to 1
  ncell_total <- ncell(diff[!is.na(diff)]) 
  ncell_gain <- ncell(fu_exist_cu_0[fu_exist_cu_0 == 1])
  ncell_gain_perc <- (ncell_gain/ncell_total)*100
  ncell_loss <- ncell(cu_exist_fu_0[cu_exist_fu_0 == 1])
  ncell_loss_perc <- (ncell_loss/ncell_total)*100
  net_value <- ncell_gain_perc - ncell_loss_perc
  
  # SP
  ncell_total_SP <- ncell(diff_SP[!is.na(diff_SP)]) 
  ncell_gain_SP <- ncell(fu_exist_cu_0_SP[fu_exist_cu_0_SP == 1])
  ncell_gain_perc_SP <- (ncell_gain_SP/ncell_total_SP)*100
  ncell_loss_SP <- ncell(cu_exist_fu_0_SP[cu_exist_fu_0_SP == 1])
  ncell_loss_perc_SP <- (ncell_loss_SP/ncell_total_SP)*100
  net_value_SP <- ncell_gain_perc_SP - ncell_loss_perc_SP
  

# Percentage of percentage of loss/gain of species (redundancy) -----------

  
  changed_area_NA <- changed_area
  changed_area_NA[changed_area_NA >= 1] <- NA
  changed_area_NA[changed_area_NA <= 0] <- 1

  fu_out_changed_area <- mask(fu_bin_rst, changed_area_NA)
  cu_out_changed_area <- mask(cu_bin_rst, changed_area_NA)
  diff_perc <- ((fu_out_changed_area/cu_out_changed_area) -1)*100
 
  writeRaster(diff_perc, 
              paste0(target_dir, "/diff_perc_out_0over0_someover0_", 
                     ES[i],".tif"), format="GTiff", overwrite=T)
  
  ncell_diff_perc <- (ncell(diff_perc[!is.na(diff_perc)])/ncell_total)*100
  
  # SP
  diff_perc_SP <- crop(diff_perc, SP)
  diff_perc_SP <- mask(diff_perc_SP, SP)
  
  writeRaster(diff_perc_SP, 
              paste0(target_dir_2, "/diff_perc_out_0over0_someover0_", 
                     ES[i],".tif"), format="GTiff", overwrite=T)
  
  ncell_diff_perc_SP <- (ncell(diff_perc_SP[!is.na(diff_perc_SP)])/ncell_total_SP)*100
  

# Ternary map for percentage ----------------------------------------------

  
  diff_perc_ter <- diff_perc
  diff_perc_ter[diff_perc_ter > 1] <- 1
  diff_perc_ter[diff_perc_ter < -1] <- -1
  diff_perc_ter[diff_perc_ter <= 0 & diff_perc_ter > -1] <- 0
  diff_perc_ter[diff_perc_ter >= 0 & diff_perc_ter < 1] <- 0
  
  ncell_ter_stable_redun <- ncell(diff_perc_ter[diff_perc_ter == 0])
  ncell_ter_stable_redun_perc <- (ncell_ter_stable_redun/ncell_total)*100
  ncell_ter_gain_redun <- ncell(diff_perc_ter[diff_perc_ter == 1])
  ncell_ter_gain_redun_perc <- (ncell_ter_gain_redun/ncell_total)*100
  ncell_ter_loss_redun <- ncell(diff_perc_ter[diff_perc_ter == -1])
  ncell_ter_loss_redun_perc <- (ncell_ter_loss_redun/ncell_total)*100
  net_value_redun <- ncell_ter_gain_redun_perc - ncell_ter_loss_redun_perc
  
  writeRaster(diff_perc_ter, 
              paste0(target_dir, "/redundancy_", 
                     ES[i],".tif"), format="GTiff", overwrite=T)
  
  # SP
  diff_perc_ter_SP <- crop(diff_perc_ter, SP)
  diff_perc_ter_SP <- mask(diff_perc_ter_SP, SP)
  
  ncell_ter_stable_redun_SP <- ncell(diff_perc_ter_SP[diff_perc_ter_SP == 0])
  ncell_ter_stable_redun_perc_SP <- (ncell_ter_stable_redun_SP/ncell_total_SP)*100
  ncell_ter_gain_redun_SP <- ncell(diff_perc_ter_SP[diff_perc_ter_SP == 1])
  ncell_ter_gain_redun_perc_SP <- (ncell_ter_gain_redun_SP/ncell_total_SP)*100
  ncell_ter_loss_redun_SP <- ncell(diff_perc_ter_SP[diff_perc_ter_SP == -1])
  ncell_ter_loss_redun_perc_SP <- (ncell_ter_loss_redun_SP/ncell_total_SP)*100
  net_value_redun_SP <- ncell_ter_gain_redun_perc_SP - ncell_ter_loss_redun_perc_SP
  
  writeRaster(diff_perc_ter_SP, 
              paste0(target_dir_2, "/redundancy_ter_", 
                     ES[i],".tif"), format="GTiff", overwrite=T)
  
  
  increasing_general <-	ncell_gain_perc + ncell_ter_gain_redun_perc
  decreasing_general <-	ncell_loss_perc + ncell_ter_loss_redun_perc
  increasing_general_SP <-	ncell_gain_perc + ncell_ter_gain_redun_perc_SP
  decreasing_general_SP <-	ncell_loss_perc + ncell_ter_loss_redun_perc_SP
  
  # Saving values
  
  Classic_results[i, ] <- c(ES[i], max_spp_cu, max_spp_fu,
                            ncell_total, ncell_gain, ncell_gain_perc,
                            ncell_loss, ncell_loss_perc, net_value,
                            ncell_diff_perc, ncell_ter_stable_redun,
                            ncell_ter_stable_redun_perc, ncell_ter_gain_redun,
                            ncell_ter_gain_redun_perc, ncell_ter_loss_redun,
                            ncell_ter_loss_redun_perc, net_value_redun,
                            ncell_total_SP, ncell_gain_SP, ncell_gain_perc_SP,
                            ncell_loss_SP, ncell_loss_perc_SP, net_value_SP,
                            ncell_diff_perc_SP, ncell_ter_stable_redun_SP,
                            ncell_ter_stable_redun_perc_SP, ncell_ter_gain_redun_SP,
                            ncell_ter_gain_redun_perc_SP, ncell_ter_loss_redun_SP,
                            ncell_ter_loss_redun_perc_SP, net_value_redun_SP,
                            increasing_general, decreasing_general,
                            increasing_general_SP, decreasing_general_SP)
  
  
# Easier thought, however, too demanding
 # # Creating an empty raster  
 # rst_result <- raster(cu_bin_rst)
 # # Logic for calculating percentages
 # for (i in 1:ncell(cu_bin_rst)) {
 #   if (!is.na(cu_bin_rst[i])) {
 #     rst_result[i] <- NA
 #   } else if (cu_bin_rst[i] == 0 & fu_bin_rst[i] == 0) {
 #     rst_result[i] <- NA
 #   } else if (cu_bin_rst[i] == 0 & fu_bin_rst[i] > 0) {
 #     rst_result[i] <- 9999
 #   } else {
 #     rst_result[i] <- ((fu_bin_rst[i] - cu_bin_rst[i]) / cu_bin_rst[i]) * 100
 #   }
 # }
 # # Check the results
 # print(rst_result)

   }


# Saving table ------------------------------------------------------------

write.csv(Classic_results, 
          "./Table_results/13e_extra.csv", 
          row.names = F)

# END ---------------------------------------------------------------------