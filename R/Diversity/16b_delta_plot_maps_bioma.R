# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Last update: 27 Jul 2022

# Plotting richness maps for all services

# Required packages

library(dplyr)
library(tidyverse)
library("spatialEco")
library(raster)
library(rgdal)
library(ggmap)
library(ggplot2)
library(scico)
library("gridExtra")
library(sf)


# ES names -----------------------------------------------------------

splist3  <- read.csv("./ENM/12_restricted_range_no_outliers.csv",
                     stringsAsFactors = FALSE) %>%
  pull(Species)


# Data tables

splist2 <- read.csv("./ENM/11_area_sp_BR.csv", 
                    stringsAsFactors = FALSE) %>%
  filter(Species %in% splist3) %>%
  pull(Species)

sp_Es_0  <-
  read.csv("./tables/eco_services_binary.csv", stringsAsFactors = FALSE) 

sp_Es_0$genus_epithet_IUCN <-
  gsub(x = sp_Es_0$genus_epithet_IUCN, pattern = " ", replacement = "_")

sp_Es  <- sp_Es_0 %>%
  filter(genus_epithet_IUCN %in% splist2)

colnames(sp_Es) <- c("species","charism","ecotour","carrion","pollin",
                     "top_pred","dispers","pest","nutrient","engineers",
                     "rodent_ctrl","sentinel")

# splist  <- read.csv("./ENM/11_area_sp_BR.csv", 
#                     stringsAsFactors = FALSE) %>%
#   pull(Species)

ES <- c("charism","ecotour","carrion","pollin","top_pred","dispers",
        "pest","nutrient","engineers","rodent_ctrl","sentinel","ES")

n <-length(ES)


# Listing rasters files ----------------------------------------------------

list.var <-
  list.files(paste0("./Diversity/delta"),
             recursive = TRUE,
             pattern = "tif",
             full.names = TRUE
  )

list.var_2 <-
  list.files(paste0("./Diversity/delta_noout"),
             recursive = TRUE,
             pattern = "tif",
             full.names = TRUE
  )

domains <- shapefile("./Shp/BR_BIOMAS_IBGE.shp")
crs.wgs84 <-
  CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
domains <- spTransform(domains, crs.wgs84)
domains <- st_as_sf(domains)


# Plot --------------------------------------------------------------------

# List of maps

q <- list() # difference for binary (fut - cur)
cc <- list() # difference for continuous (fut - cur)


# Loop to build figures of the maps

# With outliers

# q

for (i in 1:n) {
  
  dif_bin_list <- str_subset(list.var, 
                             paste0("./Diversity/delta/", "diff_tri_", ES[i],
                                    ".tif"))
  rsts_list <-raster(dif_bin_list)
  map.p <- rasterToPoints(rsts_list)
  df <- data.frame(map.p)
  colnames(df) <- c("Longitude", "Latitude", "Richness")
  
  q[[i]] <- ggplot() +
    geom_raster(data = df,aes(y = Latitude, x = Longitude, fill = `Richness`)) + 
    theme_bw() +
    coord_equal() +
    theme(
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16, angle = 90),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.title = element_blank(),
      legend.key = element_blank()
    ) +
    scale_fill_distiller(palette = "Spectral", trans = "reverse") +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank()) + 
    theme(legend.justification = c(0.5, 0),
          legend.position = c(1.2, 0.05),
          legend.text = element_text(size=15)) +
    labs(title = paste0("Difference - ", ES[i], "\n")) +
    theme(plot.title = element_text(
      lineheight = .8,
      size = 20
    )) +
    scale_x_continuous(n.breaks = 2) +
    scale_y_continuous(n.breaks = 3) +
    geom_sf(data = domains,
            color = "black",
            fill = NA,
            size = 0.2) + theme_bw() +
    theme(
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16, angle = 90),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.title = element_blank(),
      legend.key = element_blank()
    ) +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank()) + 
    theme(plot.title = element_text(
      lineheight = .8,
      size = 20
    )) +
    scale_x_continuous(n.breaks = 2) +
    scale_y_continuous(n.breaks = 3) 
  
}

q[[1]]


# Without outliers

# cc

for (i in 1:n) {
  
  dif_cont_list <- str_subset(list.var_2, 
                              paste0("./Diversity/delta_noout/", "diff_tri_noout_", ES[i],
                                     ".tif"))
  rsts_list <-raster(dif_cont_list)
  map.p <- rasterToPoints(rsts_list)
  df <- data.frame(map.p)
  colnames(df) <- c("Longitude", "Latitude", "Richness")
  
  cc[[i]] <- ggplot() +
    geom_raster(data = df,aes(y = Latitude, x = Longitude, fill = `Richness`)) + 
    theme_bw() +
    coord_equal() +
    theme(
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16, angle = 90),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.title = element_blank(),
      legend.key = element_blank()
    ) +
    scale_fill_distiller(palette = "Spectral", trans = "reverse") +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank()) + 
    theme(legend.justification = c(0.5, 0),
          legend.position = c(1.2, 0.05),
          legend.text = element_text(size=15)) +
    labs(title = paste0("Difference - ", ES[i], "\n")) +
    theme(plot.title = element_text(
      lineheight = .8,
      size = 20
    )) +
    scale_x_continuous(n.breaks = 2) +
    scale_y_continuous(n.breaks = 3) +
    geom_sf(data = domains,
            color = "black",
            fill = NA,
            size = 0.2) + theme_bw() +
    theme(
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16, angle = 90),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.title = element_blank(),
      legend.key = element_blank()
    ) +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank()) + 
    theme(plot.title = element_text(
      lineheight = .8,
      size = 20
    )) +
    scale_x_continuous(n.breaks = 2) +
    scale_y_continuous(n.breaks = 3) 
  
}

cc[[1]]


# Save maps

# Loop - error
# arr_1a <- list()
# for(i in 1:6) {
#   arr_1a[[i]] <-
#     grid.arrange(p[[i]], g[[i]], q[[i]], ncol = 1)
#   arrange_all <- grid.arrange(arr_1a[[i]])
# }
# p_arrange <- do.call(grid.arrange, c(p, g, q, ncol = 3))

# One by one

g_arrange <- grid.arrange(q[[1]], q[[2]], q[[3]], q[[4]], 
                          q[[5]], q[[6]], q[[7]], q[[8]], 
                          q[[9]], q[[10]], q[[11]], q[[12]], ncol = 3)
ggsave(
  g_arrange,
  file = "./Figures/Diff_tri_delta_newcol.tiff",
  height = 70,
  width = 75,
  units = "cm"
)

cc_arrange <-
  grid.arrange(cc[[1]], cc[[2]], cc[[3]], cc[[4]], 
               cc[[5]], cc[[6]], cc[[7]], cc[[8]], 
               cc[[9]], cc[[10]], cc[[11]], cc[[12]], ncol = 3)
ggsave(
  cc_arrange,
  file = "./Figures/Diff_tri_delta_noout_newcol.tiff",
  height = 70,
  width = 75,
  units = "cm"
)


# Reverse order of scale, if needed
# https://stackoverflow.com/questions/8750871/ggplot2-reverse-order-of-scale-brewer