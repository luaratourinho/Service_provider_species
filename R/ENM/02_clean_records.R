
# Credits ---------------------------

# Created by
# Sara Mortara (https://github.com/saramortara/data_cleaning)
# Bruno M. Carvalho (https://github.com/brunomc-eco)

# Edited by
# Luara Tourinho (https://github.com/luaratourinho)

# Date: 08 Jul 2022


# Cleaning records --------------------------------------------------------



# Required packages

library(tidyverse)
library(CoordinateCleaner)
library(countrycode)



# reading data

searches_df  <- read_csv("./ENM/data/1_occs_terr.csv")

splist <- read.csv("./ENM/data/1_occs_terr.csv",
                stringsAsFactors = FALSE) %>%
  pull(species)

splist <- unique(splist)


# removing records with NA coordinates, keeping only species from our list
searches_occs <- searches_df %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  filter(species %in% splist)


# Viewing unclean records
ggplot()+ coord_fixed()+
  borders("world", colour="gray50", fill="gray50") +
  geom_point(data = searches_occs, aes(x = lon, y = lat),
             colour = "yellow", size = 0.5)+
  theme_bw()

searches_occs$lon <- ifelse(is.na(searches_occs$lon), 
                            0, searches_occs$lon)

flags_occs <- clean_coordinates(
  x = searches_occs,
  lon = "lon",
  lat = "lat",
  # countries = "countrycode",
  # centroids_rad = 2000,
  # had to increase this limit because was not flagging the centroid of Brazil
  species = "species",
  tests = c(
    #"capitals",
    # flags records at adm-0 capitals
    #"centroids",
    # flags records at country centroids
    #"equal",
    # flags records with equal lon and lat
    "gbif",
    # flags records at gbif headquarters
    #"institutions",
    # flags records at biodiversity institutions
    "seas",
    # flags records at sea
    "zeros"
  )
) # flags records with zero lon or lat

# Viewing flagged records
plot(flags_occs, lon = "lon", lat = "lat")

# Removing flagged records and duplicates
searches_occs_clean1 <- searches_occs[flags_occs$.summary, ] %>%
  distinct()


# Cleaning by worldclim files ------------------------------------------------

library(raster)

searches_occs_clean2 = searches_occs_clean1

variable_world <- raster("./ENM/env/current/worldclim/wc2.1_10m_bio_1.tif")
# All Americas
variable <- crop(variable_world, c(-140, -30, -50, 45))
# North America
# variable <- crop(variable_world, c(-170, -20, 30, 80))

coordinates(searches_occs_clean2) <- ~lon+lat
proj4string(searches_occs_clean2)=CRS("+proj=longlat +datum=WGS84")
searches_prj<-spTransform(searches_occs_clean2,
                          CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(variable, axes=T)
plot(searches_prj, add=T, col= "red")

varcrop = crop(variable, searches_prj)

searches_extract <- raster::extract(varcrop, searches_prj, method = "bilinear")
searches_prjext<- data.frame(searches_prj,searches_extract)

which(searches_prjext$searches_extract ==0)
which(is.na(searches_prjext$searches_extract))
search_occ_extracted <- searches_prjext[!is.na(searches_prjext$searches_extract),]
head(search_occ_extracted)

search_occ_extracted <- search_occ_extracted[,c("family", "species", "lon", 
                                                "lat", "year", "country")]
head(search_occ_extracted)
dim(search_occ_extracted)


# Removing north occurrences of south species

south <- read.csv("./ENM/data/sp_northAm.csv", stringsAsFactors = FALSE) %>%
  filter(iucn == "S") %>% 
  pull(species)

splist_sth <- search_occ_extracted %>% 
  subset(species %in% south)

splist_remain <- search_occ_extracted %>% 
  subset(!species %in% south)

searches_occs_clean3 = splist_sth

# South America
variable <- crop(variable_world, c(-140, -30, -50, 13))

coordinates(searches_occs_clean3) <- ~lon+lat
proj4string(searches_occs_clean3)=CRS("+proj=longlat +datum=WGS84")
searches_prj_2<-spTransform(searches_occs_clean3,
                            CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(variable, axes=T)
plot(searches_prj_2, add=T, col= "red")

varcrop = crop(variable, searches_prj_2)

searches_extract_2 <- raster::extract(varcrop, searches_prj_2, method = "bilinear")
searches_prjext_2<- data.frame(searches_prj_2,searches_extract_2)

which(searches_prjext_2$searches_extract ==0)
which(is.na(searches_prjext_2$searches_extract))
search_occ_extracted_2 <- searches_prjext_2[!is.na(searches_prjext_2$searches_extract),]
head(search_occ_extracted_2)

search_occ_extracted_2 <- search_occ_extracted_2[,c("family", "species", "lon", 
                                                    "lat", "year", "country")]
head(search_occ_extracted_2)
dim(search_occ_extracted_2)

search_occ_extracted_3 <- bind_rows(search_occ_extracted_2, splist_remain)


# # Cleaning old date -------------------------------------------------------
# 
search_occ_by_date <- search_occ_extracted_3 %>%
 filter(year >= 1950)

# Number of records -------------------------------------------------------

n_records <- count(search_occ_by_date, species)
n_records_before <- count(search_occ_extracted_3, species)

n_two <- left_join(n_records_before, n_records, by="species")
colnames(n_two) <- c("species", "n_before_date_rm", "n_date_rm")


# Writing outputs ---------------------------------------------------------

write_csv(n_records, path = "./ENM/data/02_n_records.csv")
write_csv(n_two, path = "./ENM/data/02_n_records_w_wth_date_rm.csv")
write_csv(search_occ_by_date, path = "./ENM/data/02_clean_occs.csv")
write_csv(search_occ_extracted_3, path = "./ENM/data/02_clean_occs_original.csv")

