# Required packages
library(dplyr)
library(tidyverse)


# Livro Vermelho Mamiferos ------------------------------------------------

livro_vermelho <- read_csv("./Working_tables/livro_vermelho_mamiferos_nosubespecies.csv")
which(duplicated(livro_vermelho$Species))

# Paglia et al. 2012 & ----------------------------------------------------

paglia <- read_csv("./Working_tables/Paglias_etal_2012.csv")

lv_paglia <- left_join(livro_vermelho, paglia, by="Species")
#write.csv(lv_paglia, "./Working_tables/livro_vermelho_paglia.csv")


# Soria et al. 2021 -------------------------------------------------------

soria <- read_csv("./Working_tables/Soria_etal_2021_COMBINE_trait_data_reported.csv")

lv_p_soria <- left_join(lv_paglia, soria, by="Species")
#write.csv(lv_p_soria, "./Working_tables/livro_vermelho_Paglia_soria.csv")

which(duplicated(lv_p_soria$Species))
lv_p_soria_nodupl <- lv_p_soria[!duplicated(lv_p_soria$Species),]
#write.csv(lv_p_soria_nodupl, "./Working_tables/lv_p_soria.csv")


# Wilman et al. 2014 ------------------------------------------------------

wilman <- read_csv("./Working_tables/Wilman_etal_2014_MamFuncDat.csv")

lv_p_s_wilman <- left_join(lv_p_soria_nodupl, wilman, by="Species")

#write.csv(livro_vermelho_wilman, "./Working_tables/livro_vermelho_wilman.csv")


# EDGE --------------------------------------------------------------------

edge <- read_csv("./Working_tables/edge.csv")

lv_p_s_w_edge <- left_join(lv_p_s_wilman, edge, by="Species")

#write.csv(livro_vermelho_edge, "./Working_tables/livro_vermelho_edge.csv")


# Bello et al. 2017 -------------------------------------------------------


bello <- read_csv("./Working_tables/Bello_etal_2017_ATLANTIC_frugivory.csv")
lv_p_s_w_e_bello <- left_join(lv_p_s_w_edge, bello, by="Species")
write.csv(lv_p_s_w_e_bello, "./Working_tables/lv_p_s_w_e_bello_all_occs.csv")

which(duplicated(lv_p_s_w_e_bello$Species))
lv_p_s_w_e_bello_nodupl <- lv_p_s_w_e_bello[!duplicated(lv_p_s_w_e_bello$Species),]
write.csv(lv_p_s_w_e_bello_nodupl, "./Working_tables/lv_p_s_w_e_bello_nodupl.csv")

livro_vermelho_species <- livro_vermelho %>% pull(Species)
bello_species <- bello %>% pull(Species)
bello_species <- unique(bello_species)

livro_vermelho_bello_species <- bello %>% 
  subset(Species %in% livro_vermelho_species)
livro_vermelho_bello_species <- unique(livro_vermelho_bello_species$Species)
livro_vermelho_bello_species <- as.vector(livro_vermelho_bello_species)

#livro_vermelho_nodispersors <- rep(0,row(livro_vermelho))
Dispersor_Frugivore <- rep(1,length(livro_vermelho_bello_species))

table_dispersor <- as_tibble(cbind(livro_vermelho_bello_species,Dispersor_Frugivore))
colnames(table_dispersor) <- c("Species", "Dispersor_Frugivore")

lv_p_s_w_e_b_dispersors <- left_join(lv_p_s_w_e_bello_nodupl, table_dispersor, by="Species")

write.csv(lv_p_s_w_e_b_dispersors, "./Working_tables/lv_p_s_w_e_b_dispersors.csv")



# Farneda et al. 2019 -----------------------------------------------------

farneda <- read_csv("./Working_tables/Farneda_etal_2019.csv")
lv_p_s_w_e_b_dispersors <- read_csv("./Working_tables/lv_p_s_w_e_b_dispersors.csv")

lv_p_s_w_e_b_disp_farneda <- left_join(lv_p_s_w_e_b_dispersors, farneda, by="Species")
write.csv(lv_p_s_w_e_b_disp_farneda, "./Working_tables/lv_p_s_w_e_b_disp_f.csv")






# nodispersors <- unique(livro_vermelho_species[! livro_vermelho_species %in% livro_vermelho_bello_species])
# nodispersors_0 <- rep(0,length(nodispersors))
# 
# table_nodispersor <- as_tibble(cbind(nodispersors,nodispersors_0))
# colnames(table_nodispersor) <- c("Species", "Dispersor_Frugivore")
# 
# livro_vermelho_bello_dispersors_and_no <- left_join(livro_vermelho_bello_dispersors, table_nodispersor, by="Species")
