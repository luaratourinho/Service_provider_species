# Credits -----------------------------------------------------------------

# Code created by Luara Tourinho (https://github.com/luaratourinho)
# Edited by
# Julia Niemeyer & Luara Tourinho
# Last update: 13 Jul 2022


# Required packages

library(maps)
library(mapdata)
library(rgdal)
library(raster)
library(rgeos)
library(dplyr)


# Creating some objects to use later --------------------------------------


# projection WGS84
crs.wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

# South America Albers Equal Area Conic
crs.albers <-
  CRS("+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60
                  +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs")


# Reading files -----------------------------------------------------------

file = "./ENM/data/03_clean_df_thin.csv" ##enter the name of your table

splist  <- read.csv("./ENM/data/03_n_thinned_records.csv", 
                    stringsAsFactors = FALSE)

to_filter  <- splist %>%
  subset(n_thin_10 >= 3 & n_thin_10 < 15) %>% 
  pull(species)

sp_occs <- read.table(file, header=TRUE, sep=",")

sp_0 <- sp_occs %>%
  subset(species %in% to_filter)

sp_0$species <-
  gsub(x = sp_0$species,
       pattern = " ",
       replacement = "_")

sp <- sp_0

sp_names <- unique(sp_0$species)


## Enter name of folders to read environmental layers
# I added the chosen file on env_sel directory

# Present

pres_folder = "./ENM/env_sel/present"
pres_files <- list.files(pres_folder, full.names = T, 'tif$|bil$')
##standardize names of the variables as in your model 
#in the order of appearance in head(pres_files)
head(pres_files)
names_var <- c('bio_10', 'bio_14', 'bio_18', 'bio_2', 'bio_3')

envi <- stack(pres_files)
names(envi) <- names_var

# Future layers

fut_files <- list.files(paste0("./ENM/env_sel/future"), full.names = T, 'tif$|bil$')
head(fut_files)
envi_fut <- stack(fut_files)
names(envi_fut) <- names_var  ##standardize names of the variables as in your model


# Building a minimum convex polygon ---------------------------------------

for (a in 1:length(sp_names)){
  
  message("starting the analysis for ", paste0(sp_names[a]))
  
  sp.n = sp_names[[a]]
  
  # Read your table of occurrence records
  
  occurrence_records <- sp %>%
    filter(species == paste0(sp_names[a])) #%>%
    #select(species, lon, lat)
  
  
  # Check the column names for your coordinates and place below within c("","")
  coordinates(occurrence_records) <- c("lon", "lat")
  
  # Define the original projection of your occurrence records
  proj4string(occurrence_records) <- crs.wgs84
  
  # Reproject to an equal area projection
  occurrence_records_albers <-
    spTransform(occurrence_records, crs.albers)
  
  # Minimum convex polygon (mpc) function
  mpc <- gConvexHull(occurrence_records_albers)
  
  
  # Adding a buffer to the mpc -----------------------------------------------
  
  # You can choose a fixed value for a buffer, for example
  # If you want to 500km of ratio, use b = 500000, because b is in meters
  # If you want to a proportional value as suggested by Barve et al. 2011
  # (doi:10.1016/j.ecolmodel.2011.02.011), use the function gArea.
  # The function gArea is in m2, so multiplying by 0.000001 you can get a area in km2
  # For example, if you want to draw a buffer of 20%, in km, around of the mpc
  # 20% of the polygon area is 2e-07
  # If you want to draw a buffer of 2° it is ~111km, i.e. b <- (111000)
  
  # Function for creating different size of buffer
  # This is useful when your species has a very large distribution, then
  # 20% or even 10% of its distribution area would be bigger than the world
  # in this case, the R return an error about infinite values.
  
  b20 <- (gArea(mpc)*2e-07) ##20%
  b10 <- (gArea(mpc)*1e-07) ##10%
  b5 <- (gArea(mpc)*5e-8) ##5%
  b0 <- (gArea(mpc)*1e-09) ## ~ no buffer
  b.list <- list(b20, b10, b5, b0)
  
  for (i in 1:length(b.list)) {
    
    skip_to_next <<- FALSE
    
    buffer_mpc <- gBuffer(mpc, width = b.list[[i]])
    
    tryCatch(polygon_wgs <- spTransform(buffer_mpc, crs.wgs84),
             error = function(e) {skip_to_next <<- TRUE})
    
    if(skip_to_next == TRUE) {
      print(paste0('buffer of place ',i, ' got error, skipping to next'))
      next
    } else {
      
      print(paste0('using buffer of place ', i, ' in the list: ', b.list[[i]], ' m2'))
      # polygon_wgs <- spTransform(buffer_mpc, crs.wgs84)
      break
    }}
  
  
  
  # Applying to the variables -----------------------------------------------
  
  # If you want to clip a worldclim environmental raster
  # (https://www.worldclim.org/), which is in WGS84
  
  # Reproject your polygon to the same projection of your environmental data
  polygon_wgs <- spTransform(buffer_mpc, crs.wgs84)
  # plot(polygon_wgs)
  
  # Cut your study area for the same extension and shape of your mpc
  present_ly <- crop(envi, polygon_wgs)
  present_ly2 <- mask(present_ly, polygon_wgs)
  
  # Plot the results
  # plot(present_ly2[[1]])
  # plot(occurrence_records, add = T)
  
  # Cut your study area for the same extension and shape of your mpc
  future_ly <- crop(envi_fut, polygon_wgs)
  future_ly2 <- mask(future_ly, polygon_wgs)
  
  
  # Save your layers --------------------------------------------------------
  
  target_dir <- (paste0("./ENM/outputs/", sp_names[a]))
  
  if (file.exists(target_dir)) {
    cat("The file is there")
  } else {
    dir.create(target_dir)
  }
  
  target_dir <- (paste0("./ENM/outputs/", sp_names[a], "/Pres_env_crop/"))
  
  if (file.exists(target_dir)) {
    cat("The file is there")
  } else {
    dir.create(target_dir)
  }
  
  writeRaster(present_ly2,
    filename = paste0("./ENM/outputs/", sp_names[a], "/Pres_env_crop/",
      names(present_ly2)), bylayer = TRUE, format = "GTiff", overwrite = T)
  
  target_dir <- (paste0("./ENM/outputs/", sp_names[a], "/Fut_env_crop/"))
  
  if (file.exists(target_dir)) {
    cat("The file is there")
  } else {
    dir.create(target_dir)
  }
  
  writeRaster(future_ly2,
    filename = paste0("./ENM/outputs/", sp_names[a], "/Fut_env_crop/",
      names(future_ly2)), bylayer = TRUE, format = "GTiff", overwrite = T)
  
}




# For species with one or two occurrence records --------------------------


file = "./ENM/data/03_clean_df_thin.csv" ##enter the name of your table

splist  <- read.csv("./ENM/data/03_n_thinned_records.csv", 
                    stringsAsFactors = FALSE)

to_filter  <- splist %>%
  subset(n_thin_10 < 3) %>% 
  pull(species)

sp_occs <- read.table(file, header=TRUE, sep=",")

sp_0 <- sp_occs %>%
  subset(species %in% to_filter)

sp_0$species <-
  gsub(x = sp_0$species,
       pattern = " ",
       replacement = "_")

sp <- sp_0

sp_names <- unique(sp_0$species)

for (a in 1:length(sp_names)){
  
  message("starting the analysis for ", paste0(sp_names[a]))
  
  sp.n = sp_names[[a]]
  
  # Read your table of occurrence records
  
  occurrence_records <- sp %>%
    filter(species == paste0(sp_names[a])) #%>%
    #select(species, lon, lat)
  
  
  # Check the column names for your coordinates and place below within c("","")
  coordinates(occurrence_records) <- c("lon", "lat")
  
  # Define the original projection of your occurrence records
  proj4string(occurrence_records) <- crs.wgs84
  occurrence_records_albers <-
    spTransform(occurrence_records, crs.albers)

  polygon_buf <- gBuffer(occurrence_records_albers, byid = TRUE, width = 50000, capStyle="ROUND")
  polygon_wgs1 <- spTransform(polygon_buf, crs.wgs84)
  polygon_wgs <- aggregate(polygon_wgs1, dissolve = TRUE)

  # plot(envi[[1]])
  # plot(polygon_wgs,add=T)
  # plot(occurrence_records,add=T)
  
  present_ly <- crop(envi, polygon_wgs)
  present_ly2 <- mask(present_ly, polygon_wgs)


# Cut your study area for the same extension and shape of your mpc
future_ly <- crop(envi_fut, polygon_wgs)
future_ly2 <- mask(future_ly, polygon_wgs)
#plot(future_ly2[[1]])



# Save your layers --------------------------------------------------------

dir.create(paste0("./ENM/outputs/", sp_names[a]))

dir.create(paste0("./ENM/outputs/", sp_names[a], "/Pres_env_crop/"))

writeRaster(present_ly2,
            filename = paste0("./ENM/outputs/", sp_names[a], "/Pres_env_crop/",
                              names(present_ly2)), bylayer = TRUE, format = "GTiff", overwrite = T)

dir.create(paste0("./ENM/outputs/", sp_names[a], "/Fut_env_crop/"))

writeRaster(future_ly2,
            filename = paste0("./ENM/outputs/", sp_names[a], "/Fut_env_crop/",
                              names(future_ly2)), bylayer = TRUE, format = "GTiff", overwrite = T)

}
