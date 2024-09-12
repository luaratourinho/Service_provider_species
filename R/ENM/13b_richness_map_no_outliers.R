
# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Last update: 22 Feb 2022


# Required packages

# To sum raster
library(tidyverse)
library(dplyr)
library(raster)
# To plot
library(raster)
library(dplyr)
library(rgeos)
library(reshape)
library(rgdal)
library(ggmap)
library(ggplot2)
library(scico)
library("gridExtra")


# Reading file with species names

#sp_names <- read_csv("./ENM/11_area_sp_BR.csv")
less15onlycharis <- read_csv("./Table_results/12_sp_table_S1_less15onlycharis.csv")

sp_names <- less15onlycharis %>%
  subset(outliers == 0) %>%
  pull(Species)

n <-length(sp_names)

# Extension

Ams <- raster('./ENM/env_cropped/present/_wc2.1_10m_bio_1.tif')
BR <- shapefile("./Shp/Brasil_wgs84_diss.shp")
SP <- shapefile("./Shp/SP.shp")

# Creating file empty lists

cu <- list()
fu <- list()
cu_cont <- list()
fu_cont <- list()
cu_crop_cont <- list()
fu_crop_cont <- list()


# Reading rasters of dispensers -------------------------------------------

for(i in 1:n){
  
  cu[[i]] <- raster(paste0("./ENM/outputs/", sp_names[i], "/results/", 
                           "CUR.bin_", sp_names[i], '.asc'))
  cu[[i]] <- resample(cu[[i]], Ams, method='bilinear')
  cu[[i]][(cu[[i]])>=1] <- 1
  cu[[i]][(cu[[i]])<1] <- 0
  cu[[i]][is.na(cu[[i]])] <- 0
  
  fu[[i]] <- raster(paste0("./ENM/outputs/", sp_names[i], "/results/", 
                           '/Fut_bcc.bin_', sp_names[i], '.asc'))
  fu[[i]] <- resample(fu[[i]], Ams, method='bilinear')
  fu[[i]][(fu[[i]])>=1] <- 1
  fu[[i]][(fu[[i]])<1] <- 0
  fu[[i]][is.na(fu[[i]])] <- 0
  
  cu_cont[[i]] <- raster(paste0("./ENM/outputs/", sp_names[i], "/results/", 
                                "CUR.cont_", sp_names[i], ".asc"))
  cu_crop_cont[[i]] <- resample(cu_cont[[i]], Ams, method='bilinear')
  cu_crop_cont[[i]][is.na(cu_crop_cont[[i]])] <- 0
  
  fu_cont[[i]] <- raster(paste0("./ENM/outputs/", sp_names[i], "/results/", 
                                '/Fut_bcc.cont_', sp_names[i], '.asc'))
  fu_crop_cont[[i]] <- resample(fu_cont[[i]], Ams, method='bilinear')
  fu_crop_cont[[i]][is.na(fu_crop_cont[[i]])] <- 0
  
}


# Richness maps of disperser species ------------------------------------------

cu_rich_Ams <- Reduce('+', cu)
cu_rich_BR <- crop(cu_rich_Ams, BR)
cu_rich_BR <- mask(cu_rich_BR, BR)
cu_rich_SP <- crop(cu_rich_Ams, SP)
cu_rich_SP <- mask(cu_rich_SP, SP)
# OR
# rs <- stack(cu)
# cu_rich2 <- calc(rs, sum)

fu_rich_Ams <- Reduce('+', fu)
fu_rich_BR <- crop(fu_rich_Ams, BR)
fu_rich_BR <- mask(fu_rich_BR, BR)
fu_rich_SP <- crop(fu_rich_Ams, SP)
fu_rich_SP <- mask(fu_rich_SP, SP)

cu_cont_rich_Ams <- Reduce('+', cu_crop_cont)
cu_cont_rich_BR <- crop(cu_cont_rich_Ams, BR)
cu_cont_rich_BR <- mask(cu_cont_rich_BR, BR)
cu_cont_rich_SP <- crop(cu_cont_rich_Ams, SP)
cu_cont_rich_SP <- mask(cu_cont_rich_SP, SP)

fu_cont_rich_Ams <- Reduce('+', fu_crop_cont)
fu_cont_rich_BR <- crop(fu_cont_rich_Ams, BR)
fu_cont_rich_BR <- mask(fu_cont_rich_BR, BR)
fu_cont_rich_SP <- crop(fu_cont_rich_Ams, SP)
fu_cont_rich_SP <- mask(fu_cont_rich_SP, SP)

diff_rich_BR <- fu_rich_BR - cu_rich_BR
diff_rich_SP <- fu_rich_SP - cu_rich_SP

# Saving to BR

writeRaster(cu_rich_BR, filename = 
              paste0("./Diversity/Richness/BR_noout/Species/cu_rich.tif"),
            format="GTiff", overwrite=T)

writeRaster(fu_rich_BR, filename = 
              paste0("./Diversity/Richness/BR_noout/Species/fu_rich.tif"),
            format="GTiff", overwrite=T)

writeRaster(cu_cont_rich_BR, filename = 
              paste0("./Diversity/Richness/BR_noout/Species/cu_cont_rich.tif"),
            format="GTiff", overwrite=T)

writeRaster(fu_cont_rich_BR, filename = 
              paste0("./Diversity/Richness/BR_noout/Species/fu_cont_rich.tif"),
            format="GTiff", overwrite=T)

writeRaster(diff_rich_BR, filename = 
              paste0("./Diversity/Richness/BR_noout/Species/diff_rich.tif"),
            format="GTiff", overwrite=T)


# Saving to São Paulo

writeRaster(cu_rich_SP, filename = 
              paste0("./Diversity/Richness/SP_noout/Species/cu_rich.tif"),
            format="GTiff", overwrite=T)

writeRaster(fu_rich_SP, filename = 
              paste0("./Diversity/Richness/SP_noout/Species/fu_rich.tif"),
            format="GTiff", overwrite=T)

writeRaster(cu_cont_rich_SP, filename = 
              paste0("./Diversity/Richness/SP_noout/Species/cu_cont_rich.tif"),
            format="GTiff", overwrite=T)

writeRaster(fu_cont_rich_SP, filename = 
              paste0("./Diversity/Richness/SP_noout/Species/fu_cont_rich.tif"),
            format="GTiff", overwrite=T)

writeRaster(diff_rich_SP, filename = 
              paste0("./Diversity/Richness/SP_noout/Species/diff_rich.tif"),
            format="GTiff", overwrite=T)

# Plot --------------------------------------------------------------------

number_ticks <- function(n) {
  function(limits)
    pretty(limits, n)
}

rasters_bin <- stack(cu_rich_BR, fu_rich_BR, diff_rich_BR)

n <- length(rasters_bin@layers)
p <- list()

for (i in 1:n) {
  map.p <- rasterToPoints(rasters_bin[[i]])
  df <- data.frame(map.p)
  colnames(df) <- c("Longitude", "Latitude", "Adequabilidade ambiental")
  
  p[[i]] <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
    geom_raster(aes(fill = `Adequabilidade ambiental`)) + theme_bw() +
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
      legend.key = element_blank(),
      legend.key.size = unit(0.5, 'cm')
    ) +
    # scico::scale_fill_scico(palette = "lajolla") +
    # scale_colour_brewer(palette="BuPu", direction=-1) +
    scale_fill_distiller(palette = "Spectral") +
    #RColorBrewer::display.brewer.all("Spectral") +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank()) + 
    theme(legend.justification = c(0.5, 0),
          legend.position = c(0.85, 0.05),
          legend.text = element_text(size=13)) +
    # labs(title = "Acritopappus harleyi\n") +
    #labs(title = paste0(target_species[i], "\n")) +
    theme(plot.title = element_text(
      lineheight = .8,
      face = "italic",
      size = 20
    )) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1),
                       breaks = number_ticks(3))
}

p[[1]]

p_arrange <-
  grid.arrange(p[[1]], p[[2]], nrow = 1)
ggsave(
  p_arrange,
  file = "./Figures/Richness_species_cu_fu_noout_less15onlycharis.tiff",
  height = 15,
  width = 30,
  units = "cm"
)

ggsave(
  p[[3]],
  file = "./Figures/Richness_species_diff_noout_less15onlycharis.tiff",
  height = 17,
  width = 18,
  units = "cm"
)
