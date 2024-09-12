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

# List of rasters

list.var <-
  list.files(paste0("./Diversity/Richness/BR_noout"),
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

# Reading species names of ecosystem services 

# Outliers

splist3  <- read.csv("./Table_results/12_sp_table_S1_less15onlycharis.csv",
                     stringsAsFactors = FALSE)%>%
  subset(outliers == 0)  %>%
  pull(Species)


# Data tables

splist2 <- read.csv("./Table_results/12_sp_table_S1_less15onlycharis.csv", 
                    stringsAsFactors = FALSE) %>%
  filter(Species %in% splist3) %>%
  pull(Species)

sp_Es_0  <-
  read.csv("./tables/eco_services_binary.csv", stringsAsFactors = FALSE) 

sp_Es_0$genus_epithet_IUCN <-
  gsub(x = sp_Es_0$genus_epithet_IUCN, pattern = " ", replacement = "_")

sp_Es  <- sp_Es_0 %>%
  filter(genus_epithet_IUCN %in% splist2)

sp_Es <- sp_Es[,c(3:6,8,10,12,14,16,18,20,22)]

colnames(sp_Es) <- c("species","charism","ecotour","carrion","pollin",
                     "top_pred","dispers","pest","nutrient","engineers",
                     "rodent_ctrl","sentinel")


ES <- c("charism","ecotour","carrion","pollin","top_pred","dispers",
        "pest","nutrient","engineers","rodent_ctrl","sentinel")

n <-length(ES)


# Building table by a loop ---------------------------------------------------

Classic_results <- matrix(nrow = n, ncol = 25)
colnames(Classic_results) <- c("ES", "n_sp_occs", 
                               "min_cu_cont","max_cu_cont",
                               "min_fu_cont","max_fu_cont",
                               "min_cu_bin", "max_cu_bin",
                               "min_fu_bin", "max_fu_bin",
                               "min_dif_cont", "max_dif_cont",
                               "min_dif_bin","max_dif_bin", 
                               "area_cu","area_fu","area_net", 
                               "area_dif_loss", "area_dif_gain",
                               "percentage_change", "quantile_fu_cu_0",
                               "quantile_fu_cu_25","quantile_fu_cu_50",
                               "quantile_fu_cu_75","quantile_fu_cu_100")

# loop 

for(i in 1:n){
  
  # Occurrence number
  
  sp_Es_sub  <- sp_Es %>%
    subset(sp_Es[i+1] == 1)
  
  sp_Es_sub_occ  <- sp_Es_sub %>%
    subset(species %in% splist2)
  
  n_sp_occs <- dim(sp_Es_sub_occ)[1]
  
  # Minimum and maximum values of raster
  
  cu_cont_list <- str_subset(list.var, 
                             paste0("./Diversity/Richness/BR_noout/cu_cont_noout_",
                                    ES[i],".tif"))
  
  cu_cont_rst <- raster(cu_cont_list)
  min_cu_cont <- min(cu_cont_rst[], na.rm = T)
  max_cu_cont <- max(cu_cont_rst[], na.rm = T)
  
  fu_cont_list <- str_subset(list.var, 
                             paste0("./Diversity/Richness/BR_noout/fu_cont_noout_",
                                    ES[i],".tif"))
  
  fu_cont_rst <- raster(fu_cont_list)
  min_fu_cont <- min(fu_cont_rst[], na.rm = T)
  max_fu_cont <- max(fu_cont_rst[], na.rm = T)
  
  cu_bin_list <- str_subset(list.var, 
                            paste0("./Diversity/Richness/BR_noout/cu_bin_noout_",
                                   ES[i],".tif"))
  
  cu_bin_rst <- raster(cu_bin_list)
  min_cu_bin <- min(cu_bin_rst[], na.rm = T)
  max_cu_bin <- max(cu_bin_rst[], na.rm = T)
  
  
  fu_bin_list <- str_subset(list.var, 
                            paste0("./Diversity/Richness/BR_noout/fu_bin_noout_",
                                   ES[i],".tif"))
  
  fu_bin_rst <- raster(fu_bin_list)
  min_fu_bin <- min(fu_bin_rst[], na.rm = T)
  max_fu_bin <- max(fu_bin_rst[], na.rm = T)
  
  fu_cu_cont_list <- str_subset(list.var_2, 
                                paste0("./Diversity/Richness/Diff_BR_noout/fu_cu_cont_noout_",
                                       ES[i],".tif"))
  
  fu_cu_cont_rst <- raster(fu_cu_cont_list)
  min_dif_cont <- min(fu_cu_cont_rst[], na.rm = T)
  max_dif_cont <- max(fu_cu_cont_rst[], na.rm = T)
  
  fu_cu_bin_list <- str_subset(list.var_2, 
                               paste0("./Diversity/Richness/Diff_BR_noout/fu_cu_bin_noout_",
                                      ES[i],".tif"))
  
  fu_cu_bin_rst <- raster(fu_cu_bin_list)
  min_dif_bin <- min(fu_cu_bin_rst[], na.rm = T)
  max_dif_bin <- max(fu_cu_bin_rst[], na.rm = T)
  
  quantile_fu_cu <- round(unname(quantile(fu_cu_bin_rst)))
  quantile_0 <- quantile_fu_cu[1]
  quantile_25 <- quantile_fu_cu[2]
  quantile_50 <- quantile_fu_cu[3]
  quantile_75 <- quantile_fu_cu[4]
  quantile_100 <- quantile_fu_cu[5]
  
  # Area
  
  cu_area_0 = cu_bin_rst
  cu_area_0[cu_area_0 > 0] <- 1
  cu_area = cu_area_0
  cu_area[cu_area == 0] <- NA
  r = raster(nrow = cu_area@nrows, ncol = cu_area@ncols, xmn = cu_area@extent@xmin, 
             xmx = cu_area@extent@xmax, ymn = cu_area@extent@ymin, ymx = cu_area@extent@ymax) 
  # calculate area of each cell (the area changes along the latitudes / longitudes)
  x = raster::area(r) 
  #plot(x)
  ocorrencia_area = x * cu_area
  area_cu <- cellStats(ocorrencia_area, stat='sum', na.rm=TRUE, asSample=TRUE)
  
  fu_area_0 = fu_bin_rst
  fu_area_0[fu_area_0 > 0] <- 1
  fu_area = fu_area_0
  fu_area[fu_area == 0] <- NA
  ocorrencia_area = x * fu_area
  area_fu <- cellStats(ocorrencia_area, stat='sum', na.rm=TRUE, asSample=TRUE)
  
  #Diff
  
  cu_bin_rst2 = cu_bin_rst
  cu_bin_rst2[cu_bin_rst2 == 0] <- 999
  fu_bin_rst2 = fu_bin_rst
  fu_bin_rst2[fu_bin_rst2 == 0] <- 999
  erase_zerozero <- fu_bin_rst2 + cu_bin_rst2
  erase_zerozero[erase_zerozero < 1998] <- NA
  erase_zerozero[erase_zerozero == 1998] <- 1
  
  diff_fu_cu = fu_area_0 - cu_area_0
  diff = diff_fu_cu
  diff[diff < 0] <- 0
  diff[diff == 0] <- NA
  
  diff <- crop(diff, erase_zerozero)
  
  ocorrencia_area = x * diff
  area_net <- cellStats(ocorrencia_area, stat='sum', na.rm=TRUE, asSample=TRUE)
  
  diff_loss =  fu_area_0 - cu_area_0
  diff_loss[diff_loss != -1] <- NA
  diff_loss[diff_loss == -1] <- 1
  ocorrencia_area = x * diff_loss
  area_dif_loss <- cellStats(ocorrencia_area, stat='sum', na.rm=TRUE, asSample=TRUE)
  
  diff_gain =  fu_area_0 - cu_area_0
  diff_gain[diff_gain >= 1] <- 1
  diff_gain[diff_gain < 1] <- NA
  ocorrencia_area = x * diff_gain
  area_dif_gain <- cellStats(ocorrencia_area, stat='sum', na.rm=TRUE, asSample=TRUE)
  
  percentage_change <- ((area_fu/area_cu) - 1)*100
  
  # Add values to the table
  
  Classic_results[i, ] <- c(ES[i], n_sp_occs, 
                            min_cu_cont,max_cu_cont,
                            min_fu_cont,max_fu_cont,
                            min_cu_bin, max_cu_bin,
                            min_fu_bin, max_fu_bin,
                            min_dif_cont, max_dif_cont,
                            min_dif_bin,max_dif_bin, 
                            area_cu,area_fu,area_net, 
                            area_dif_loss,area_dif_gain,
                            percentage_change, quantile_0,
                            quantile_25,quantile_50,
                            quantile_75,quantile_100)
  
}

# View(Classic_results)



# Results for all ES together ---------------------------------------------


sp_Es_sub  <- sp_Es %>%
  subset(charism| ecotour| carrion| pollin| top_pred| dispers|
           pest| nutrient| engineers| rodent_ctrl| sentinel == 1)

sp_Es_sub_occ  <- sp_Es_sub %>%
  subset(species %in% splist)

n_sp_occs <- dim(sp_Es_sub_occ)[1]

# Minimum and maximum values of raster

cu_cont_list <- str_subset(list.var, 
                           paste0("./Diversity/Richness/BR_noout/cu_cont_noout_ES.tif"))

cu_cont_rst <- raster(cu_cont_list)
min_cu_cont <- min(cu_cont_rst[], na.rm = T)
max_cu_cont <- max(cu_cont_rst[], na.rm = T)

fu_cont_list <- str_subset(list.var, 
                           paste0("./Diversity/Richness/BR_noout/fu_cont_noout_ES.tif"))

fu_cont_rst <- raster(fu_cont_list)
min_fu_cont <- min(fu_cont_rst[], na.rm = T)
max_fu_cont <- max(fu_cont_rst[], na.rm = T)

cu_bin_list <- str_subset(list.var, 
                          paste0("./Diversity/Richness/BR_noout/cu_bin_noout_ES.tif"))

cu_bin_rst <- raster(cu_bin_list)
min_cu_bin <- min(cu_bin_rst[], na.rm = T)
max_cu_bin <- max(cu_bin_rst[], na.rm = T)


fu_bin_list <- str_subset(list.var, 
                          paste0("./Diversity/Richness/BR_noout/fu_bin_noout_ES.tif"))

fu_bin_rst <- raster(fu_bin_list)
min_fu_bin <- min(fu_bin_rst[], na.rm = T)
max_fu_bin <- max(fu_bin_rst[], na.rm = T)

fu_cu_cont_list <- str_subset(list.var_2, 
                              paste0("./Diversity/Richness/Diff_BR_noout/fu_cu_cont_noout_ES.tif"))

fu_cu_cont_rst <- raster(fu_cu_cont_list)
min_dif_cont <- min(fu_cu_cont_rst[], na.rm = T)
max_dif_cont <- max(fu_cu_cont_rst[], na.rm = T)

fu_cu_bin_list <- str_subset(list.var_2, 
                             paste0("./Diversity/Richness/Diff_BR_noout/fu_cu_bin_noout_ES.tif"))

fu_cu_bin_rst <- raster(fu_cu_bin_list)
min_dif_bin <- min(fu_cu_bin_rst[], na.rm = T)
max_dif_bin <- max(fu_cu_bin_rst[], na.rm = T)

quantile_fu_cu <- round(unname(quantile(fu_cu_bin_rst)))
quantile_0 <- quantile_fu_cu[1]
quantile_25 <- quantile_fu_cu[2]
quantile_50 <- quantile_fu_cu[3]
quantile_75 <- quantile_fu_cu[4]
quantile_100 <- quantile_fu_cu[5]

# Area

cu_area_0 = cu_bin_rst
cu_area_0[cu_area_0 > 0] <- 1
cu_area = cu_area_0
cu_area[cu_area == 0] <- NA
r = raster(nrow = cu_area@nrows, ncol = cu_area@ncols, xmn = cu_area@extent@xmin, 
           xmx = cu_area@extent@xmax, ymn = cu_area@extent@ymin, ymx = cu_area@extent@ymax) 
# calculate area of each cell (the area changes along the latitudes / longitudes)
x = raster::area(r) 
#plot(x)
ocorrencia_area = x * cu_area
area_cu <- cellStats(ocorrencia_area, stat='sum', na.rm=TRUE, asSample=TRUE)

fu_area_0 = fu_bin_rst
fu_area_0[fu_area_0 > 0] <- 1
fu_area = fu_area_0
fu_area[fu_area == 0] <- NA
ocorrencia_area = x * fu_area
area_fu <- cellStats(ocorrencia_area, stat='sum', na.rm=TRUE, asSample=TRUE)

# Diff

cu_bin_rst2 = cu_bin_rst
cu_bin_rst2[cu_bin_rst2 == 0] <- 999
fu_bin_rst2 = fu_bin_rst
fu_bin_rst2[fu_bin_rst2 == 0] <- 999
erase_zerozero <- fu_bin_rst2 + cu_bin_rst2
erase_zerozero[erase_zerozero < 1998] <- NA
erase_zerozero[erase_zerozero == 1998] <- 1

diff_fu_cu = fu_area_0 - cu_area_0
diff = diff_fu_cu
diff[diff < 0] <- 0
diff[diff == 0] <- NA

diff <- crop(diff, erase_zerozero)

ocorrencia_area = x * diff
area_net <- cellStats(ocorrencia_area, stat='sum', na.rm=TRUE, asSample=TRUE)

diff_loss =  fu_area_0 - cu_area_0
diff_loss[diff_loss != -1] <- NA
diff_loss[diff_loss == -1] <- 1
ocorrencia_area = x * diff_loss
area_dif_loss <- cellStats(ocorrencia_area, stat='sum', na.rm=TRUE, asSample=TRUE)

diff_gain =  fu_area_0 - cu_area_0
diff_gain[diff_gain >= 1] <- 1
diff_gain[diff_gain < 1] <- NA
ocorrencia_area = x * diff_gain
area_dif_gain <- cellStats(ocorrencia_area, stat='sum', na.rm=TRUE, asSample=TRUE)

percentage_change <- ((area_fu/area_cu) - 1)*100


# Building the final table

Classic_results_1 = Classic_results
all <- "all_ES"
all_ES <- c(all, n_sp_occs, 
            min_cu_cont,max_cu_cont,
            min_fu_cont,max_fu_cont,
            min_cu_bin, max_cu_bin,
            min_fu_bin, max_fu_bin,
            min_dif_cont, max_dif_cont,
            min_dif_bin,max_dif_bin, 
            area_cu,area_fu,area_net, 
            area_dif_loss,area_dif_gain,
            percentage_change, quantile_0,
            quantile_25,quantile_50,
            quantile_75,quantile_100)

# noquote(all_ES[2:21])
Classic_results_1 <- rbind(Classic_results, all_ES)

# For the paper, remove the colunms of areas, they do not make sense
# I just wanted explorated the results

write.csv(Classic_results_1, "./Table_results/13_ES_table_S2b_noout_zerozero_quartile.csv", row.names = F)
