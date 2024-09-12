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

# Plot --------------------------------------------------------------------

# List of maps

p <- list() # current binary
g <- list() # future binary
q <- list() # difference for binary (fut - cur)
aa <- list() # current continuous
bb <- list() # future continuous
cc <- list() # difference for continuous (fut - cur)


# Loop to build figures of the maps

# BINARY

# p

for (i in 1:n) {
  
  cu_bin_list <- str_subset(list.var, 
                            paste0("./Diversity/Richness/BR_noout/cu_bin_noout_",
                                   ES[i],".tif"))
  rsts_list <-raster(cu_bin_list)
  map.p <- rasterToPoints(rsts_list)
  df <- data.frame(map.p)
  colnames(df) <- c("Longitude", "Latitude", "Richness")
  
  p[[i]] <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
    geom_raster(aes(fill = `Richness`)) + theme_bw() +
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
    scico::scale_fill_scico(palette = "lajolla") +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank()) + 
    theme(legend.justification = c(0.5, 0),
          legend.position = c(1.2, 0.05),
          legend.text = element_text(size=15)) +
    labs(title = paste0("Current - ", ES[i], "\n")) +
    theme(plot.title = element_text(
      lineheight = .8,
      #face = "italic",
      size = 20
    )) +
    coord_equal()+
    scale_x_continuous(n.breaks = 2) +
    scale_y_continuous(n.breaks = 3)
  
}

p[[1]]


# g

for (i in 1:n) {
  
  fu_bin_list <- str_subset(list.var, 
                            paste0("./Diversity/Richness/BR_noout/fu_bin_noout_",
                                   ES[i],".tif"))
  rsts_list <-raster(fu_bin_list)
  map.p <- rasterToPoints(rsts_list)
  df <- data.frame(map.p)
  colnames(df) <- c("Longitude", "Latitude", "Richness")
  
  g[[i]] <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
    geom_raster(aes(fill = `Richness`)) + theme_bw() +
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
    scico::scale_fill_scico(palette = "lajolla") +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank()) + 
    theme(legend.justification = c(0.5, 0),
          legend.position = c(1.2, 0.05),
          legend.text = element_text(size=15)) +
    labs(title = paste0("Future - ", ES[i], "\n")) +
    theme(plot.title = element_text(
      lineheight = .8,
      size = 20
    )) +
    #coord_equal()+
    scale_x_continuous(n.breaks = 2) +
    scale_y_continuous(n.breaks = 3)
  
}

g[[1]]


# q

for (i in 1:n) {
  
  dif_bin_list <- str_subset(list.var_2, 
                             paste0("./Diversity/Richness/Diff_BR_noout/fu_cu_bin_noout_",
                                    ES[i],".tif"))
  rsts_list <-raster(dif_bin_list)
  map.p <- rasterToPoints(rsts_list)
  df <- data.frame(map.p)
  colnames(df) <- c("Longitude", "Latitude", "Richness")
  
  q[[i]] <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
    geom_raster(aes(fill = `Richness`)) + theme_bw() +
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
    scico::scale_fill_scico(palette = "lajolla") +
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
    coord_equal()+
    scale_x_continuous(n.breaks = 2) +
    scale_y_continuous(n.breaks = 3)
  
}

q[[1]]


# CONTINUOUS

# aa

for (i in 1:n) {
  
  cu_cont_list <- str_subset(list.var, 
                             paste0("./Diversity/Richness/BR_noout/cu_cont_noout_",
                                    ES[i],".tif"))
  rsts_list <-raster(cu_cont_list)
  map.p <- rasterToPoints(rsts_list)
  df <- data.frame(map.p)
  colnames(df) <- c("Longitude", "Latitude", "Richness")
  
  aa[[i]] <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
    geom_raster(aes(fill = `Richness`)) + theme_bw() +
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
    scico::scale_fill_scico(palette = "lajolla") +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank()) + 
    theme(legend.justification = c(0.5, 0),
          legend.position = c(1.2, 0.05),
          legend.text = element_text(size=15)) +
    labs(title = paste0("Current - ", ES[i], "\n")) +
    theme(plot.title = element_text(
      lineheight = .8,
      #face = "italic",
      size = 20
    )) +
    coord_equal()+
    scale_x_continuous(n.breaks = 2) +
    scale_y_continuous(n.breaks = 3)
  
}

aa[[1]]


# bb

for (i in 1:n) {
  
  fu_cont_list <- str_subset(list.var, 
                             paste0("./Diversity/Richness/BR_noout/fu_cont_noout_",
                                    ES[i],".tif"))
  rsts_list <-raster(fu_cont_list)
  map.p <- rasterToPoints(rsts_list)
  df <- data.frame(map.p)
  colnames(df) <- c("Longitude", "Latitude", "Richness")
  
  bb[[i]] <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
    geom_raster(aes(fill = `Richness`)) + theme_bw() +
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
    scico::scale_fill_scico(palette = "lajolla") +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank()) + 
    theme(legend.justification = c(0.5, 0),
          legend.position = c(1.2, 0.05),
          legend.text = element_text(size=15)) +
    labs(title = paste0("Future - ", ES[i], "\n")) +
    theme(plot.title = element_text(
      lineheight = .8,
      size = 20
    )) +
    coord_equal()+
    scale_x_continuous(n.breaks = 2) +
    scale_y_continuous(n.breaks = 3)
  
}

bb[[1]]


# cc

for (i in 1:n) {
  
  dif_cont_list <- str_subset(list.var_2, 
                              paste0("./Diversity/Richness/Diff_BR_noout/fu_cu_cont_noout_",
                                     ES[i],".tif"))
  rsts_list <-raster(dif_cont_list)
  map.p <- rasterToPoints(rsts_list)
  df <- data.frame(map.p)
  colnames(df) <- c("Longitude", "Latitude", "Richness")
  
  cc[[i]] <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
    geom_raster(aes(fill = `Richness`)) + theme_bw() +
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
    scico::scale_fill_scico(palette = "lajolla") +
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
    coord_equal()+
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

# BINARY

arra_cha <-
  grid.arrange(p[[1]], g[[1]], q[[1]], nrow = 1)

arra_eco <-
  grid.arrange(p[[2]], g[[2]], q[[2]], nrow = 1)

arra_car <-
  grid.arrange(p[[3]], g[[3]], q[[3]], nrow = 1)

arra_pol <-
  grid.arrange(p[[4]], g[[4]], q[[4]], nrow = 1)

arra_top <-
  grid.arrange(p[[5]], g[[5]], q[[5]], nrow = 1)

arra_dis <-
  grid.arrange(p[[6]], g[[6]], q[[6]], nrow = 1)

arra_pes <-
  grid.arrange(p[[7]], g[[7]], q[[7]], nrow = 1)

arra_nut <-
  grid.arrange(p[[8]], g[[8]], q[[8]], nrow = 1)

arra_eng <-
  grid.arrange(p[[9]], g[[9]], q[[9]], nrow = 1)

arra_rod <-
  grid.arrange(p[[10]], g[[10]], q[[10]], nrow = 1)

arra_set <-
  grid.arrange(p[[11]], g[[11]], q[[11]], nrow = 1)

arra_ES <-
  grid.arrange(p[[12]], g[[12]], q[[12]], nrow = 1)

# All together

arrange_1_6 <- grid.arrange(arra_cha, arra_eco, arra_car, arra_pol,
                            arra_top, arra_dis, nrow = 6)

ggsave(
  arrange_1_6,
  file = "./Figures/Richness_ES_arrange_1_6_bin_noout.tiff",
  height = 90,
  width = 65,
  units = "cm"
)

arrange_7_12 <- grid.arrange(arra_pes, arra_nut, arra_eng, arra_rod,
                             arra_set, arra_ES, nrow = 6)

ggsave(
  arrange_7_12,
  file = "./Figures/Richness_ES_arrange_7_12_bin_noout.tiff",
  height = 90,
  width = 65,
  units = "cm"
)


# CONTINUOUS

arra_cha <-
  grid.arrange(aa[[1]], bb[[1]], cc[[1]], nrow = 1)

arra_eco <-
  grid.arrange(aa[[2]], bb[[2]], cc[[2]], nrow = 1)

arra_car <-
  grid.arrange(aa[[3]], bb[[3]], cc[[3]], nrow = 1)

arra_pol <-
  grid.arrange(aa[[4]], bb[[4]], cc[[4]], nrow = 1)

arra_top <-
  grid.arrange(aa[[5]], bb[[5]], cc[[5]], nrow = 1)

arra_dis <-
  grid.arrange(aa[[6]], bb[[6]], cc[[6]], nrow = 1)

arra_pes <-
  grid.arrange(aa[[7]], bb[[7]], cc[[7]], nrow = 1)

arra_nut <-
  grid.arrange(aa[[8]], bb[[8]], cc[[8]], nrow = 1)

arra_eng <-
  grid.arrange(aa[[9]], bb[[9]], cc[[9]], nrow = 1)

arra_rod <-
  grid.arrange(aa[[10]], bb[[10]], cc[[10]], nrow = 1)

arra_set <-
  grid.arrange(aa[[11]], bb[[11]], cc[[11]], nrow = 1)

arra_ES <-
  grid.arrange(aa[[12]], bb[[12]], cc[[12]], nrow = 1)

# All together

arrange_1_6 <- grid.arrange(arra_cha, arra_eco, arra_car, arra_pol,
                            arra_top, arra_dis, nrow = 6)

ggsave(
  arrange_1_6,
  file = "./Figures/Richness_ES_arrange_1_6_cont_noout.tiff",
  height = 90,
  width = 65,
  units = "cm"
)

arrange_7_12 <- grid.arrange(arra_pes, arra_nut, arra_eng, arra_rod,
                             arra_set, arra_ES, nrow = 6)

ggsave(
  arrange_7_12,
  file = "./Figures/Richness_ES_arrange_7_12_cont_noout.tiff",
  height = 90,
  width = 65,
  units = "cm"
)



# Reverse order of scale, if needed
# https://stackoverflow.com/questions/8750871/ggplot2-reverse-order-of-scale-brewer