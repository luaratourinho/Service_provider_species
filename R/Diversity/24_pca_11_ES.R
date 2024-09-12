library(dplyr)
library(tidyverse)
library(raster)
library(stringr)


files_list <-
  list.files(
    paste0("~/Service_provider_species/Diversity/Richness/BR_noout"),
    recursive = TRUE,
    pattern = "tif",
    full.names = TRUE
  ) # a set of rasters

files <- str_subset(
  files_list,
  "^/home/users/luaratourinho2/Service_provider_species/Diversity/Richness/BR_noout/cu_cont_noout_"
)

# files <- str_subset(
#   files_list,
#   "^/home/users/luaratourinho2/Service_provider_species/Diversity/Richness/BR_noout/fu_cont_noout_"
# )

layers <- stack(files[-6])

sr <- sampleRandom(layers, 50000) # sample 5000 random grid cells

# run PCA on random sample with correlation matrix
# retx=FALSE means don't save PCA scores
pca <- prcomp(sr, scale = TRUE, retx = FALSE)

pca_table <- as.data.frame(pca$rotation, row.names = T)
pca_table_summary <- as.data.frame(summary(pca)$importance)

write_csv(pca_table, path = "./ES_11_pca_cur.csv")
write_csv(pca_table_summary, path = "./ES_11_pca_summary_cur.csv")

write_csv(pca_table, path = "./ES_11_pca_fut.csv")
write_csv(pca_table_summary, path = "./ES_11_pca_summary_fut.csv")


# write PCA model to file

fviz_eig(pca)

dput(pca, file = paste("~/Service_provider_species/Diversity/", "Eleven_ES_pca.csv", sep = ""))

x <- predict(layers, pca, index = 1:6) 
plot(x)
