# Required packages
library(dplyr)
library(tidyverse)
library(plyr)
library(readr)


# IUCN tables (710 sp) ----------------------------------------------------

mydir = "./Working_tables/mammals"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
myfiles

# dat_csv = ldply(myfiles, read_csv)
# dat_csv

taxonomy <- read_csv(myfiles[5])
synonyms <- read_csv(myfiles[9])
common_names <- read_csv(myfiles[2])
countries <- read_csv(myfiles[3])
simple_summary <- read_csv(myfiles[4])
assessments <- read_csv(myfiles[6])
habitats <- read_csv(myfiles[8])
threats <- read_csv(myfiles[10])
conservation_needed <- read_csv(myfiles[7])
all_other_fields <- read_csv(myfiles[1])


t_s <- left_join(taxonomy, synonyms, by="scientificName")
t_s_cn <- left_join(t_s, common_names, by="scientificName")
t_s_cn_c <- left_join(t_s_cn, countries, by="scientificName")
t_s_cn_c_ss <- left_join(t_s_cn_c, simple_summary, by="scientificName")
t_s_cn_c_ss_a <- left_join(t_s_cn_c_ss, assessments, by="scientificName")
t_s_cn_c_ss_a_h <- left_join(t_s_cn_c_ss_a, habitats, by="scientificName")
t_s_cn_c_ss_a_h_t <- left_join(t_s_cn_c_ss_a_h, threats, by="scientificName")
t_s_cn_c_ss_a_h_t_cne <- left_join(t_s_cn_c_ss_a_h_t, conservation_needed, by="scientificName")
t_s_cn_c_ss_a_h_t_cne_o <- left_join(t_s_cn_c_ss_a_h_t_cne, all_other_fields, by="scientificName")

#write.csv(lv_paglia, "./Working_tables/livro_vermelho_paglia.csv")



which(duplicated(livro_vermelho$Species))