# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Last update: 05 Aug 2022

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

# List rasters' files

list.var <-
  list.files(paste0("./Diversity/beta/each_ES"),
             recursive = TRUE,
             pattern = "tif",
             full.names = TRUE)

ES <- c("charism","ecotour","carrion","pollin","top_pred","dispers",
        "pest","nutrient","engineers","rodent_ctrl","sentinel")

#beta_metr <- c("sor_", "sim_", "nes_")

p <- list()

# Plot

# SOR

for (i in 1:length(ES)) {
  
  beta_list <- str_subset(list.var, 
                          paste0("./Diversity/beta/each_ES/beta_sor_",
                                 ES[i],".tif"))
  rsts_list <-raster(beta_list)
  map.p <- rasterToPoints(rsts_list)
  df <- data.frame(map.p)
  colnames(df) <- c("Longitude", "Latitude", "Beta")
  
  p[[i]] <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
    geom_raster(aes(fill = `Beta`)) + theme_bw() +
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
    labs(title = paste0(ES[i], "\n")) +
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

g_arrange <- grid.arrange(p[[1]], p[[2]], p[[3]], 
                          p[[4]], p[[5]], p[[6]], ncol = 1)
ggsave(
  g_arrange,
  file = "./Figures/Beta_sor_1_6.tiff",
  height = 70,
  width = 20,
  units = "cm"
)

g_arrange <- grid.arrange(p[[7]], p[[8]], p[[9]], 
                          p[[10]], p[[11]], ncol = 1)
ggsave(
  g_arrange,
  file = "./Figures/Beta_sor_7_11.tiff",
  height = 70,
  width = 20,
  units = "cm"
)


# SIM

g <- list()

for (i in 1:length(ES)) {
  
  beta_list <- str_subset(list.var, 
                          paste0("./Diversity/beta/each_ES/beta_sim_",
                                 ES[i],".tif"))
  rsts_list <-raster(beta_list)
  map.p <- rasterToPoints(rsts_list)
  df <- data.frame(map.p)
  colnames(df) <- c("Longitude", "Latitude", "Beta")
  
  g[[i]] <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
    geom_raster(aes(fill = `Beta`)) + theme_bw() +
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
    labs(title = paste0(ES[i], "\n")) +
    theme(plot.title = element_text(
      lineheight = .8,
      #face = "italic",
      size = 20
    )) +
    coord_equal()+
    scale_x_continuous(n.breaks = 2) +
    scale_y_continuous(n.breaks = 3)
  
}

gg_arrange <- grid.arrange(g[[1]], g[[2]], g[[3]], 
                          g[[4]], g[[5]], g[[6]], ncol = 1)
ggsave(
  gg_arrange,
  file = "./Figures/Beta_sim_1_6.tiff",
  height = 70,
  width = 20,
  units = "cm"
)

gg_arrange <- grid.arrange(g[[7]], g[[8]], g[[9]], 
                          g[[10]], g[[11]], ncol = 1)
ggsave(
  gg_arrange,
  file = "./Figures/Beta_sim_7_11.tiff",
  height = 70,
  width = 20,
  units = "cm"
)


# NES


f <- list()

for (i in 1:length(ES)) {
  
  beta_list <- str_subset(list.var, 
                          paste0("./Diversity/beta/each_ES/beta_nes_",
                                 ES[i],".tif"))
  rsts_list <-raster(beta_list)
  map.p <- rasterToPoints(rsts_list)
  df <- data.frame(map.p)
  colnames(df) <- c("Longitude", "Latitude", "Beta")
  
  f[[i]] <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
    geom_raster(aes(fill = `Beta`)) + theme_bw() +
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
    labs(title = paste0(ES[i], "\n")) +
    theme(plot.title = element_text(
      lineheight = .8,
      #face = "italic",
      size = 20
    )) +
    coord_equal()+
    scale_x_continuous(n.breaks = 2) +
    scale_y_continuous(n.breaks = 3)
  
}

f_arrange <- grid.arrange(f[[1]], f[[2]], f[[3]], 
                           f[[4]], f[[5]], f[[6]], ncol = 1)
ggsave(
  f_arrange,
  file = "./Figures/Beta_nes_1_6.tiff",
  height = 70,
  width = 20,
  units = "cm"
)

f_arrange <- grid.arrange(f[[7]], f[[8]], f[[9]], 
                           f[[10]], f[[11]], ncol = 1)
ggsave(
  f_arrange,
  file = "./Figures/Beta_nes_7_11.tiff",
  height = 70,
  width = 20,
  units = "cm"
)

