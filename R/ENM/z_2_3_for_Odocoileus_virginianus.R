library(tidyverse)
library(CoordinateCleaner)
library(countrycode)
library(rgdal)
library(raster)
library(spThin)
library(data.table)

searches_df  <- read_csv("./ENM/data/02_clean_occs.csv")

splist <- read.csv("./ENM/data/02_clean_occs.csv",
                   stringsAsFactors = FALSE) %>% pull(species)

splist <- unique(splist)
splist <- "Odocoileus virginianus"

searches_occs <- searches_df %>%
  filter(species %in% splist)
dim(searches_occs)
#91677

unique(searches_occs$species)

search_occ_by_date <- searches_occs %>% filter(year >= 1960)
dim(search_occ_by_date)
#91183

search_occ_by_date2 <- search_occ_by_date %>% filter(year <= 2010)
dim(search_occ_by_date2)
#10359

occs_til_1960 = search_occ_by_date
occs_bet_1960_2010 = search_occ_by_date2


# Cropping by IUCN polygon ------------------------------------------------

# I did not use that, as considering the range 1960 to 2010 was enough

# variable_world <- raster("./ENM/env/current/worldclim/wc2.1_10m_bio_1.tif")
# variable <- shapefile("./ENM/shp/data_0.shp")
# 
# coordinates(occs_bet_1960_2010) <- ~lon+lat
# proj4string(occs_bet_1960_2010)=CRS("+proj=longlat +datum=WGS84")
# searches_prj<-spTransform(occs_bet_1960_2010,
#                           CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# plot(variable, axes=T)
# plot(searches_prj, add=T, col= "red")
# 
# varcrop = crop(variable, searches_prj)
# 
# searches_extract <- raster::extract(varcrop, searches_prj, method = "bilinear")
# searches_prjext<- data.frame(searches_prj,searches_extract)
# 
# which(searches_prjext$searches_extract ==0)
# which(is.na(searches_prjext$searches_extract))
# search_occ_extracted <- searches_prjext[!is.na(searches_prjext$searches_extract),]
# head(search_occ_extracted)
# 
# search_occ_extracted <- search_occ_extracted[,c("family", "species", "lon", 
#                                                 "lat", "year", "country")]
# head(search_occ_extracted)
# dim(search_occ_extracted)


# spThin ------------------------------------------------------------------

clean_df <- occs_bet_1960_2010[,2:4]
spp <- clean_df %>% pull(species)
spp <- unique(spp)

thin_10 <- list()
for(i in 1:length(spp)){
  df <- clean_df %>%
    filter(species %in% spp[i])
  thinned <- thin(df,
                  lat.col = "lat",
                  long.col = "lon",
                  spec.col = "species",
                  thin.par = 20, # distance in km
                  reps = 1,
                  locs.thinned.list.return = TRUE,
                  write.files = FALSE,
                  write.log.file = FALSE)
  thin_10[[i]] <- data.frame(species = rep(spp[i], nrow(thinned[[1]])),
                             lon = thinned[[1]]$Longitude,
                             lat = thinned[[1]]$Latitude)
}
clean_df_thin_10 <- rbindlist(thin_10)

ggplot() +
  borders("world", colour="gray50", fill="gray50") +
  geom_point(data = clean_df_thin_10, aes(x = lon, y = lat),
             colour = "blue", size = 1.5) +
  # geom_point(data = clean_df_thin_10, aes(x = lon, y = lat),
  #            colour = "red", size = 1.0) +
  coord_sf(xlim = c(-160, -28), ylim = c(-60,90)) +
  theme_bw()


# counting records by species

n_10 <- clean_df_thin_10 %>%
  group_by(species) %>%
  summarize(n_thin_10 = n())


# adding counts to the n_records table
n_records <- read_csv("./ENM/data/02_n_records.csv")

n_records <- n_records %>%
  left_join(n_10, by = "species") %>%
  replace_na(list(n_thin_10 = 0))

# writing outputs
write_csv(n_records, path = "./ENM/data/03_n_thinned_records_287.csv")
write_csv(clean_df_thin_10, path = "./ENM/data/03_clean_df_thin_287.csv")
