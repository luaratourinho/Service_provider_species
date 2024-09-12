
# Credits ---------------------------

# Authors: Bruno Vilela, Filipe Augusto Nascimento & Marcos Vinícius Carneiro Vital
# Tittle: Impacts of climate change on small-ranged amphibians of the northern Atlantic Forest
# Oecologia Australis 22(2) 2018
# doi: 10.4257/oeco.2018.2202.03

# Edited by
# Luara Tourinho (https://github.com/luaratourinho)
# Last update: 17 jul 2022



# Required packages -------------------------------------------------------

library(sp)
library(maptools)
library(vegan)
library(geosphere)
library(dismo)
library(letsR)
library(sf)
library(dplyr)
library(tidyverse)
library(raster)

# Species list ------------------------------------------------------------

splist  <- read.csv("./ENM/data/03_n_thinned_records.csv", 
                    stringsAsFactors = FALSE)

to_filter  <- splist %>%
  subset(n_thin_10 < 15) %>%
  pull(species)

sp_occs <- read.table("./ENM/data/03_clean_df_thin.csv",
                      header=TRUE, sep=",")

sp_0 <- sp_occs %>%
  subset(species %in% to_filter)

sp_0$species <-
  gsub(x = sp_0$species,
       pattern = " ",
       replacement = "_")

# Only charismatics

sp_Es  <- read.csv("./tables/eco_services_binary.csv", stringsAsFactors = FALSE)

sp_Es$genus_epithet_IUCN <-
  gsub(x = sp_Es$genus_epithet_IUCN,
       pattern = " ",
       replacement = "_")

sp_Es_sub  <- sp_Es %>%
  subset(CHARISMATIC_AND_OR_CULTURAL_IMPORTANT_SPECIES == 1)%>%
  pull(genus_epithet_IUCN)

less15_charism  <- sp_0 %>%
  subset(sp_0$species %in% sp_Es_sub)

# Species list to run

# sp <- sp_0
# sp_names <- unique(sp_0$species)

sp <- less15_charism
sp_names <- unique(less15_charism$species)


# Running model -----------------------------------------------------------



for (a in 1:length(sp_names)){
  
  message("starting the analysis for ", paste0(sp_names[a]))
  

# Preparing the data ------------------------------------------------------

list.var.present <-
  list.files(paste0("./ENM/outputs/", sp_names[a], "/Pres_env_crop/"),
             recursive = TRUE,
             pattern = "tif",
             full.names = TRUE
  )


predictors <- stack(list.var.present)

# Future

scenario_1 <- list.files(
  paste0("./ENM/outputs/", sp_names[a], "/Fut_env_crop/"),
  pattern = "tif",
  full.names = TRUE,
  recursive = TRUE
)

scenario_1 <- stack(scenario_1)


scenario_2 <- list.files(
  paste0("./ENM/outputs/", sp_names[a], "/Fut_env_crop/"),
  pattern = "tif",
  full.names = TRUE,
  recursive = TRUE
)

scenario_2 <- stack(scenario_2)

future_var_P1 <- list(scenario_1, scenario_2)
names(future_var_P1) <- c(126, 585)


#2071-2100, i.e. second interest period P2
scenario_3 <- list.files(
  paste0("./ENM/outputs/", sp_names[a], "/Fut_env_crop/"),
  pattern = "tif",
  full.names = TRUE,
  recursive = TRUE
)

scenario_3 <- stack(scenario_3)

scenario_4 <- list.files(
  paste0("./ENM/outputs/", sp_names[a], "/Fut_env_crop/"),
  pattern = "tif",
  full.names = TRUE,
  recursive = TRUE
)

scenario_4 <- stack(scenario_4)

future_var_P2 <- list(scenario_3, scenario_4)
names(future_var_P2) <- c(126, 585)


# Standardization of predictors (mean = 0, sd = 2)
valores <- values(predictors)
remove <- is.na(valores[, 1])
valores_P1 <- lapply(future_var_P1, values)
valores_P2 <- lapply(future_var_P2, values)
valores_P1 <-
  lapply(valores_P1, function(x, rem) {
    x[!rem,]
  }, rem = remove)
valores_P2 <-
  lapply(valores_P2, function(x, rem) {
    x[!rem,]
  }, rem = remove)
valores <- valores[!remove,]


#1:n, n is the number of scenarios, here, 1 current and 4 futures
ID <- rep(1:5, each = nrow(valores)) 
complete.table <- rbind(valores,
                        do.call(rbind, valores_P1),
                        do.call(rbind, valores_P2))
decos <-
  apply(complete.table,
        2,
        decostand,
        method = "standardize",
        na.rm = TRUE)

#returning values to raster
values(predictors)[!remove,] <- decos[ID == 1,] 


#plot(predictors)
for (i in 1:length(valores_P1)) {
  valores_P1[[i]] <- decos[ID == i + 1,]
}
for (i in 1:length(valores_P2)) {
  valores_P2[[i]] <- decos[ID == i + 5,]
}

#predictors <-stack(predictors)


# Occurrence records

file = "./ENM/data/03_clean_df_thin.csv" ##enter the name of your table

splist  <- read.csv("./ENM/data/03_n_thinned_records.csv", 
                    stringsAsFactors = FALSE)

to_filter  <- splist %>%
  subset(n_thin_10 < 15) %>% 
  pull(species)

sp_occs <- read.table(file, header=TRUE, sep=",")

sp_0 <- sp_occs %>%
  subset(species %in% to_filter)

sp_0$species <-
  gsub(x = sp_0$species,
       pattern = " ",
       replacement = "_")

occurrence <- sp_0%>%
  subset(species == sp_names[a])

occurrence <- occurrence[!is.na(raster::extract(raster(predictors, 1), occurrence[, 2:3])),]
# plot(raster(predictors, 1))
# points(occurrence[, 2:3], col = "red")
# plot(raster(predictors, 2))
# points(occurrence[, 2:3], col = "red")


# Species
species <- as.factor(occurrence[, 1])
spp <- levels(species)
N_of_occurrences_per_species <- table(species)

reference <- raster::extract(predictors, occurrence[, 2:3])
valores <- values(predictors)



# Euclidian distance for current ------------------------------------------


# Env dist
N_species <- length(spp)
N_occurrences <- nrow(reference)

# Euclidean function
euclidean <- function(x,
                      n_oc = N_occurrences,
                      rem = remove,
                      n_sp = N_species,
                      ref = reference,
                      sp = species,
                      sp2 = spp) {
  s_rem <- nrow(x)
  eu <- matrix(ncol = n_oc, nrow = s_rem)
  for (i in 1:n_oc) {
    eu1 <- (ref[i, 1] - x[, 1]) ^ 2
    eu2 <- (ref[i, 2] - x[, 2]) ^ 2 #repetir para a quantidade de variaveis
    eu[, i] <- sqrt(eu1 + eu2)
  }
  # summarize per species
  result <- matrix(ncol = n_sp, nrow = s_rem)
  colnames(result) <- sp2
  for (i in 1:n_sp) {
    result[, i] <- apply(eu[, sp == sp2[i], drop = F], 1, mean)
  }
  return(result)
}

# Env dist to the present
eu.present <- euclidean(x = decos[ID == 1,]) 

# Plots
for (i in 1:N_species) {
  r <- raster(predictors, 1)
  values(r)[!remove] <- eu.present[, i]
  plot(r, main = colnames(eu.present)[i], col = gray.colors(100))
  points(occurrence[species == colnames(eu.present)[i], 2:3],
         pch = 20,
         col = rgb(1, 0, 0, .3),
         cex = 2)}

# Normalize and invert

  minimo <- min(r[], na.rm=T)
  maximo <- max(r[], na.rm=T)
  
  normalizar <- function(x) {
    (x-minimo)/(maximo-minimo)}
  
  norm <- normalizar(r)
  
  norm_inv <- 
    (((norm - max(norm[], na.rm=T)) * -1) + min(norm[], na.rm=T))
  
# Save continuous

# Create the directory where results from ENM will be saved
target_dir = paste(paste0("./ENM/outputs/", sp_names[a], "/results/", sep=""))

if (file.exists(target_dir)) {
  cat("The file is there")
} else {
  dir.create(target_dir)
}

  
writeRaster(norm_inv, 
            paste0("./ENM/outputs/", sp_names[a], "/results/", 
                   "CUR.cont_", sp_names[a], '.asc', sep=""),
            overwrite=TRUE)


# Binary ------------------------------------------------------------------

norm_inv_thr <- norm_inv
norm_inv_thr[norm_inv_thr <= 0.7] <- 0
norm_inv_thr[norm_inv_thr > 0.7] <- 1

# Save binary

writeRaster(norm_inv_thr, 
            paste0("./ENM/outputs/", sp_names[a], "/results/", 
                   "CUR.bin_", sp_names[a], '.asc', sep=""),
            overwrite=TRUE)




# Env dist to the future --------------------------------------------------


eu.2011_40_126 <- euclidean(decos[ID == 2,]) 
eu.2011_40_585 <- euclidean(decos[ID == 3,]) 
eu.2071_00_126 <- euclidean(decos[ID == 4,]) 
eu.2071_00_585 <- euclidean(decos[ID == 5,])

eu.dists <- rbind(
  eu.present,
  eu.2011_40_126,
  eu.2011_40_585,
  eu.2071_00_126,
  eu.2071_00_585
)

for (i in 1:N_species) {
r_1 <- raster(predictors, 1)
values(r_1)[!remove] <- eu.2011_40_126[, i]
r_2 <- raster(predictors, 1)
values(r_2)[!remove] <- eu.2011_40_585[, i]
r_3 <- raster(predictors, 1)
values(r_3)[!remove] <- eu.2071_00_126[, i]
r_4 <- raster(predictors, 1)
values(r_4)[!remove] <- eu.2071_00_585[, i]
}

# all_future <- stack(r_1,r_2,r_3,r_4)
# names(all_future) <- c("eu.2011_40_126", "eu.2011_40_585", "eu.2071_00_126", "eu.2071_00_585")
# 
# writeRaster(all_future, filename=paste0("./data/outputs/ED/future/", sp.names[8], "/"), 
#             format="GTiff", bylayer=TRUE, suffix="names", overwrite=TRUE)

# Normalize and invert

minimo <- min(r_1[], na.rm=T)
maximo <- max(r_1[], na.rm=T)

normalizar <- function(x) {
  (x-minimo)/(maximo-minimo)}

norm_1 <- normalizar(r_1)

norm_inv_1 <- 
  (((norm_1 - max(norm_1[], na.rm=T)) * -1) + min(norm_1[], na.rm=T))

# Save continuous

writeRaster(norm_inv_1, 
            paste0("./ENM/outputs/", sp_names[a], "/results/", 
                   '/Fut_bcc.cont_', sp_names[a], '.asc', sep=""), 
            overwrite=TRUE)

# Binary ------------------------------------------------------------------

norm_inv_thr_1 <- norm_inv_1
norm_inv_thr_1[norm_inv_thr_1 <= 0.7] <- 0
norm_inv_thr_1[norm_inv_thr_1 > 0.7] <- 1

# Save binary

writeRaster(norm_inv_thr_1, 
            paste0("./ENM/outputs/", sp_names[a], "/results/", 
                   '/Fut_bcc.bin_', sp_names[a], '.asc', sep=""),
            overwrite=TRUE)

}

