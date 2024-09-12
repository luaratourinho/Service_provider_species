# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Last update: 20 Jul 2022


## Calculating delta, beta (turnover and nestedness) to different scenarios (from ENM)

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
  list.files(paste0("./Diversity/beta/noout"),
             recursive = TRUE,
             pattern = "tif",
             full.names = TRUE)

beta_metr <- c("sor", "sim", "nes")

p <- list()

# Plot

for (i in 1:length(beta_metr)) {
  
  beta_list <- str_subset(list.var, 
                            paste0("./Diversity/beta/noout/beta_",
                                   beta_metr[i],"_noout.tif"))
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
  labs(title = paste0(beta_metr[i], "\n")) +
  theme(plot.title = element_text(
    lineheight = .8,
    #face = "italic",
    size = 20
  )) +
  coord_equal()+
  scale_x_continuous(n.breaks = 2) +
  scale_y_continuous(n.breaks = 3) #+ 
  # scale_fill_gradient(limits = c(0, 1)) +
  # scico::scale_fill_scico(palette = "lajolla") +
  # theme(panel.border=element_blank(),
  #       axis.text.x= element_blank(),
  #       axis.text.y = element_blank(),
  #       line = element_blank())

}

p[[1]]

g_arrange <- grid.arrange(p[[1]], p[[2]], p[[3]], nrow = 1)
ggsave(
  g_arrange,
  file = "./Figures/Beta_allES_noout.tiff",
  height = 10,
  width = 40,
  units = "cm"
)
