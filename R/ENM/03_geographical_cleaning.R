
# Credits ---------------------------

# Script created by
# Aiello-Lammens et al. (https://cran.r-project.org/web/packages/spThin/spThin.pdf)
# Aiello-Lammens et al. 2015 (doi: 10.1111/ecog.01132)

# Edited by
# Bruno Carvalho (https://github.com/brunomc-eco)
# by 2019
# and 
# Luara Tourinho (https://github.com/luaratourinho)
# Date: 12 Jul 2022


# Geographical cleaning using spThin package ------------------------------


# Required packages

library(spThin) # Aiello-Lammens et al. 2015
library(tidyverse)
library(data.table)


## loading clean occs and getting clean species list

occs_to_thin <- read.csv("./ENM/data/02_clean_occs.csv",
                     stringsAsFactors = FALSE)

occs_to_thin <- occs_to_thin[,2:4]
occs_to_thin_ord <- occs_to_thin %>% arrange(species)

spp_names <- read.csv("./ENM/data/02_clean_occs.csv", stringsAsFactors = FALSE) %>% 
  arrange(species) %>%
  pull(species)

spp_names <- unique(spp_names)


# Problem with memory, separating species
# spp <- spp_names[1:50]
# spp <- spp_names[51:70]
# spp <- spp_names[71:286]
# spp <- spp_names[287]
# spp <- spp_names[288:330]
# spp <- spp_names[331:431]

clean_df <- occs_to_thin_ord %>% 
  subset(species %in% spp)


# Example of two distances in km
# Run to two distances you are interested in, then choose one option to proceed
# Worldclim: 10 minutes (18.6 x 18.6 = 344 km2 at the equator)
# thinning records by 20 km
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


# Check thinned records
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
write_csv(n_records, path = "./ENM/data/03_n_thinned_records.csv")
write_csv(clean_df_thin_10, path = "./ENM/data/03_clean_df_thin.csv")
