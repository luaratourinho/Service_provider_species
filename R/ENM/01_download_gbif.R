
# Credits ---------------------------

# Script created by
# Bruno M. Carvalho (https://github.com/brunomc-eco)

# Edited by
# Luara Tourinho (https://github.com/luaratourinho)

# Last update: 05 Jul 2022


# Getting species records -------------------------------------------------



# Required packages

library(tidyverse)
library(taxize) # for get_gbifid_
library(data.table)
library(dplyr)
library(purrr)
library(readr)
library(magrittr) # for %T>% pipe
library(rgbif) # for occ_download


# Creating/reading our species list


splist <- read.csv("./ENM/data/sp_names.csv", stringsAsFactors = FALSE) %>%
  pull(species)



# GBIF --------------------------------------------------------------------


# getting records from gbif
# got this code from https://data-blog.gbif.org/post/downloading-long-species-lists-on-gbif/
# Manual of rgbif: https://cran.r-project.org/web/packages/rgbif/rgbif.pdf

gbif_taxon_keys <- splist %>%
  get_gbifid_(method="backbone") %>% # get taxonkeys for each species name
  imap(~ .x %>% mutate(original_sciname = .y)) %>% # add original name back to data.frame
  bind_rows() # combine all results in a single data.frame

only_keys <- gbif_taxon_keys %>%
  filter(matchtype == "EXACT" & status == "ACCEPTED") %>% # get only accepted names
  pull(usagekey) #retain only the taxonkeys

# download data directly at GBIF
# (file needs to be manually fetched at the user's downloads page at gbif.org)

# enter GBIF credentials
user <- "luaratourinho" # your gbif.org username
pwd <- "58458988Gbif" # your gbif.org password
email <- "luatourinho@gmail.com" # your email

occ_download(
  pred_in("taxonKey", only_keys),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  format = "SIMPLE_CSV",
  user = user, pwd = pwd, email = email
)

# https://doi.org/10.15468/dl.6karx3
gbif_df <- fread("./data/0120643-210914110416597.csv", na.strings = c("", NA))

gbif_df2 <- gbif_df[,c("family","species","decimalLongitude","decimalLatitude",
                       "year","countryCode")]
head(gbif_df2)
colnames(gbif_df2) <- c("family", "species", "lon", "lat", "year", "country")


# Table with search results -----------------------------------------------

searches <-
  tibble(species = splist,
         date_of_search = rep(Sys.Date(), length(splist))) %>%  
  left_join(gbif_df2, by = "species")

#only_keys <- tibble(taxonKey = only_keys)


# Saving outputs ----------------------------------------------------------

write_csv(searches, "./outputs/01_search_refined_results.csv")
write_csv(gbif_df2, "./outputs/01_gbif_refined.csv")
write_csv(gbif_df, "./outputs/01_unclean_records_gbif.csv")
#write_csv(only_keys, "./outputs/01_gbif_taxonkeys.csv")
