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
  list.files(paste0("./Diversity/beta/each_ES/noout"),
             recursive = TRUE,
             pattern = "tif",
             full.names = TRUE)

ES <- c("charism","ecotour","carrion","pollin","top_pred","dispers",
        "pest","nutrient","engineers","rodent_ctrl","sentinel")

ff <- list()

# Plot

# SOR

for (i in 1:length(ES)) {
  
  beta_list <- str_subset(list.var, 
                          paste0("./Diversity/beta/each_ES/noout/beta_sor_",
                                 ES[i],".tif"))
  rsts_list <-raster(beta_list)
  map.p <- rasterToPoints(rsts_list)
  df <- data.frame(map.p)
  colnames(df) <- c("Longitude", "Latitude", "Beta")
  
 ff[[i]] <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
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
    scale_y_continuous(n.breaks = 3) + 
   scale_fill_gradient(limits = c(0, 1)) +
   scico::scale_fill_scico(palette = "lajolla") +
   theme(panel.border=element_blank(),
         axis.text.x= element_blank(),
         axis.text.y = element_blank(),
         line = element_blank())
  
}

ff[[1]]

# p[[i]] is from script 19

g_arrange <- grid.arrange(p[[1]],ff[[1]], ff[[2]], ff[[3]], 
                          ff[[4]], ff[[5]],  ncol = 1)
ggsave(
  g_arrange,
  file = "./Figures/Beta_sor_1_6_noout_nogrid.tiff",
  height = 70,
  width = 20,
  units = "cm"
)

g_arrange <- grid.arrange(ff[[7]], ff[[8]], ff[[9]], 
                          ff[[10]], ff[[11]], ff[[6]],ncol = 1)
ggsave(
  g_arrange,
  file = "./Figures/Beta_sor_7_11_noout_nogrid.tiff",
  height = 70,
  width = 20,
  units = "cm"
)


# SIM

g <- list()

for (i in 1:length(ES)) {
  
  beta_list <- str_subset(list.var, 
                          paste0("./Diversity/beta/each_ES/noout/beta_sim_",
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
    scale_y_continuous(n.breaks = 3) + 
    scale_fill_gradient(limits = c(0, 1)) +
    scico::scale_fill_scico(palette = "lajolla") +
    theme(panel.border=element_blank(),
          axis.text.x= element_blank(),
          axis.text.y = element_blank(),
          line = element_blank())
  
}

gg_arrange <- grid.arrange(p[[2]],g[[1]], g[[2]], g[[3]], 
                           g[[4]], g[[5]],  ncol = 1)
ggsave(
  gg_arrange,
  file = "./Figures/Beta_sim_1_6_noout_nogrid.tiff",
  height = 70,
  width = 20,
  units = "cm"
)

gg_arrange <- grid.arrange(g[[7]], g[[8]], g[[9]], 
                           g[[10]], g[[11]], g[[6]],ncol = 1)
ggsave(
  gg_arrange,
  file = "./Figures/Beta_sim_7_11_noout_nogrid.tiff",
  height = 70,
  width = 20,
  units = "cm"
)


# NES


f <- list()

for (i in 1:length(ES)) {
  
  beta_list <- str_subset(list.var, 
                          paste0("./Diversity/beta/each_ES/noout/beta_nes_",
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
    scale_y_continuous(n.breaks = 3) + 
    scale_fill_gradient(limits = c(0, 1)) +
    scico::scale_fill_scico(palette = "lajolla") +
    theme(panel.border=element_blank(),
          axis.text.x= element_blank(),
          axis.text.y = element_blank(),
          line = element_blank())
  
}

f_arrange <- grid.arrange(p[[3]],f[[1]], f[[2]], f[[3]], 
                          f[[4]], f[[5]],  ncol = 1)
ggsave(
  f_arrange,
  file = "./Figures/Beta_nes_1_6_noout_nogrid.tiff",
  height = 70,
  width = 20,
  units = "cm"
)

f_arrange <- grid.arrange(f[[7]], f[[8]], f[[9]], 
                          f[[10]], f[[11]], f[[6]],ncol = 1)
ggsave(
  f_arrange,
  file = "./Figures/Beta_nes_7_11_noout_nogrid.tiff",
  height = 70,
  width = 20,
  units = "cm"
)

