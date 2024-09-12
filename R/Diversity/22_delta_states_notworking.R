# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Last update: 17 Apr 2023

# Evaluating ecosystem services (ES) for each Brazilian state

# Required packages

library(tidyverse)
library(dplyr)
library(raster)
library(rgdal)
library(rgeos)

# Shapefile of the Brazilian states

# Reading shp

states <- shapefile("./Shp/BR_UF_2020.shp")
# +proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs 

# Reprojecting

crs.wgs84 <-
  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
states <- spTransform(states, crs.wgs84)

# Creating vectors

ES <- c("charism","ecotour","carrion","pollin","top_pred","dispers",
        "pest","nutrient","engineers","rodent_ctrl","sentinel", "ES")


acro <- as.vector(states@data$SIGLA_UF)

# List of files with delta diversity of ES

list.var <-
  list.files(paste0("./Diversity/delta"),
             recursive = TRUE,
             pattern = "tif",
             full.names = TRUE)

# Size of the loop (number of ES)

n <-length(ES)

# Creating table of results

nn <- length(states@data$SIGLA_UF)

results <- matrix(nrow = nn, ncol = 4)
colnames(results) <- c("ES","gain","loss","stability")

# Loop to calculate the percentage of loss, gain and stability for each state

for(i in 1:n) {
  fu_cu_bin_list <- str_subset(list.var,
                               paste0("./Diversity/delta/diff_tri_",
                                      ES[i], ".tif"))
  
  
  fu_cu_bin_rst <- raster(fu_cu_bin_list)
  
  perc_diff <- extract(fu_cu_bin_rst, states)
  
 # Calculating percentage of each class
  
   func_perc_1 <- function(x) {
    (sum(x == 1) / length(x)) * 100
  }
  gain <- sapply(perc_diff, func_perc_1)

  func_perc_2 <- function(x) {
    (sum(x == -1) / length(x)) * 100
  }
  loss <- sapply(perc_diff, func_perc_2)
  
  func_perc_3 <- function(x) {
    (sum(x == 1) / length(x)) * 100
  }
  stability <- sapply(perc_diff, func_perc_3)
  
  
  #results[i,] <- c(acro[i], ES[i], gain, loss, stability)
 
} 



# Add the percentage to the shapefile
 
states@data <- cbind(states@data, results)
 
# Saving shp

writeOGR(states, dsn = "./Diversity",
          layer = "states_diff", driver="ESRI Shapefile", overwrite=T)
 
 
 ### End ###