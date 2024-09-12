
# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Last update: 25 Jul 2022


# Required packages

library(tidyverse)
library(dplyr)

# Records numbers (< or > 15)

# splist  <- read.csv("./ENM/data/03_n_thinned_records.csv", 
#                     stringsAsFactors = FALSE)
# 
# splist$species <-
#   gsub(x = splist$species,
#        pattern = " ",
#        replacement = "_")
# 
# more_15  <- splist %>%
#   subset(n_thin_10 >= 15) %>%
#   pull(species)
# 
# less_15  <- splist %>%
#   subset(n_thin_10 < 15) %>%
#   pull(species)

# Read your species list
sp_names <- read.csv("./ENM/data/03_n_thinned_records.csv", 
                     stringsAsFactors = FALSE)
sp_names$species <-
  gsub(x = sp_names$species,
       pattern = " ",
       replacement = "_")

# Only charismatics with less than 15 occurrences

sp_names_l15  <- sp_names %>%
  subset(n_thin_10 < 15) %>%
  pull(species)

sp_names_h15  <- sp_names %>%
  subset(n_thin_10 >= 15) %>%
  pull(species)

sp_Es  <- read.csv("./tables/eco_services_binary.csv", stringsAsFactors = FALSE)

sp_Es$genus_epithet_IUCN <-
  gsub(x = sp_Es$genus_epithet_IUCN,
       pattern = " ",
       replacement = "_")

sp_Es_sub  <- sp_Es %>%
  subset(CHARISMATIC_AND_OR_CULTURAL_IMPORTANT_SPECIES == 1)%>%
  pull(genus_epithet_IUCN)

less15_charism  <- sp_names_l15 %>%
  subset(sp_names_l15 %in% sp_Es_sub)

# Species to run

more_15  <- sp_names_h15
less_15  <- less15_charism

#table_results_original <- read_csv("./ENM/11_area_sp_BR.csv")
table_results_original <- read_csv("./ENM/11_area_sp_BR_less15onlycharis.csv")
table_results_counting <- table_results_original[,1:2]
colnames(table_results_counting) <-c("species","bla")

table_results_original_m15 <- table_results_counting %>%
  subset(species %in% more_15)
dim(table_results_original_m15)[1] # take it note

table_results_original_l15 <- table_results_counting %>%
  subset(species %in% less_15)
dim(table_results_original_l15)[1] # take it note


# Reading file with species names

table_results <- table_results_original[,c("Species", "ocorrencia_area_cu", "percentage_change")]

restricted_range <- rep(0, length(table_results$Species))

table_results$restricted_range <- restricted_range
head(table_results)

# Identify outliers

#I identified the outliers by the method rout, removing likely outliers (Q = 10%) 
#using the software GraphPad Prism (version 8.0.1).
# https://www.graphpad.com/support/faq/comparing-the-grubbs-and-rout-method-of-identifying-outliers/
# Q = 1% -> 46 species, outlier threshold 184.8282706
# Q = 10% -> 55 species, outlier threshold 138.6386153

# OR do it in R, for example
# https://statsandr.com/blog/outliers-detection-in-r/
# OR https://cran.r-project.org/web/packages/outliers/outliers.pdf

# out <- boxplot.stats(df1$percentage_change)$out
# lower_bound <- quantile(df1$percentage_change, 0.025) # we defined the lower threshol -100%
# upper_bound <- quantile(df1$percentage_change, 0.9) # for the upper, we could use the quartile
# > 90% 194.8549 
# outlier_ind <- which(df1$percentage_change < lower_bound | df1$percentage_change > upper_bound)
# df1[outlier_ind, "percentage_change"]
# df1[outlier_ind, ]

# Restricted range and mean

df1 <- table_results[table_results$ocorrencia_area_cu <= 10000,]
restricted_range <- rep(1, length(df1$Species))
df1$restricted_range <- restricted_range

df1_perc_greater_100 <- df1[df1$percentage_change > 138,]
df1_diff <- dplyr::setdiff(df1, df1_perc_greater_100)

mean_with_outliers_1 <- mean(df1$percentage_change)
mean_without_outliers_1 <- mean(df1_diff$percentage_change)

# # With quantile
# 
# quantile(table_results$ocorrencia_area_cu)
# #0%          25%          50%          75%         100% 
# #322.5917   65407.0398  350450.3268 1312692.7710 7971529.4070 
# 
# df1 <- table_results[table_results$ocorrencia_area_cu <= 323,]
# restricted_range <- rep(1, length(df1$Species))
# df1$restricted_range <- restricted_range
# 
# df1_perc_greater_100 <- df1[df1$percentage_change > 138,]
# df1_diff <- dplyr::setdiff(df1, df1_perc_greater_100)
# 
# mean_with_outliers_1 <- mean(df1$percentage_change)
# mean_without_outliers_1 <- mean(df1_diff$percentage_change)
# 
# #


df2 <- table_results[table_results$ocorrencia_area_cu > 10000 &
                       table_results$ocorrencia_area_cu < 50000,]
restricted_range <- rep(2, length(df2$Species))
df2$restricted_range <- restricted_range

df2_perc_greater_100 <- df2[df2$percentage_change > 138,]
df2_diff <- dplyr::setdiff(df2, df2_perc_greater_100)

mean_with_outliers_2 <- mean(df2$percentage_change)
mean_without_outliers_2 <- mean(df2_diff$percentage_change)

df3 <- table_results[table_results$ocorrencia_area_cu >= 50000,]
restricted_range <- rep(3, length(df3$Species))
df3$restricted_range <- restricted_range

df3_perc_greater_100 <- df3[df3$percentage_change > 138,]
df3_diff <- dplyr::setdiff(df3, df3_perc_greater_100)

mean_with_outliers_3 <- mean(df3$percentage_change)
mean_without_outliers_3 <- mean(df3_diff$percentage_change)

table_results1 <- bind_rows(df1, df2, df3)
table_results_outliers <- bind_rows(df1_perc_greater_100,df2_perc_greater_100,
                                    df3_perc_greater_100)
outliers_proportion <- (dim(table_results_outliers)[1]/dim(table_results1)[1])*100
# ~15%
table_results_without_outliers <- bind_rows(df1_diff,df2_diff,df3_diff)



# Statistics --------------------------------------------------------------


# All species

mean_table_results1 <-
  aggregate(
    x = table_results1$percentage_change,
    by = list(table_results1$restricted_range),
    FUN = mean)

colnames(mean_table_results1) <- c("restricted_range", "mean")

summary(table_results1)
table_results1_summary <- data.frame(unclass(summary(table_results1)),
                                     check.names = FALSE)
write.csv(table_results1_summary,
          "./ENM/12_statistics_allsp.csv", row.names = F)

write.csv(table_results1,
          "./ENM/12_restricted_range_allsp_less15onlycharis.csv", row.names = F)


# Only outliers species

mean_table_results_outliers <-
  aggregate(
    x = table_results_outliers$percentage_change,
    by = list(table_results_outliers$restricted_range),
    FUN = mean)

colnames(mean_table_results_outliers) <- c("restricted_range", "mean")

summary(table_results_outliers)
table_results_outliers_summary <-
  data.frame(unclass(summary(table_results_outliers)),
             check.names = FALSE)

write.csv(table_results_outliers_summary,
          "./ENM/12_statistics_outliers.csv", row.names = F)

write.csv(table_results_outliers,
          "./ENM/12_restricted_range_outliers_less15onlycharis.csv", row.names = F)


# Only no outliers species

mean_table_results_without_outliers <-
  aggregate(
    x = table_results_without_outliers$percentage_change,
    by = list(table_results_without_outliers$restricted_range),
    FUN = mean)

colnames(mean_table_results_without_outliers) <-
  c("restricted_range", "mean")

summary(table_results_without_outliers)
table_results_without_outliers_summary <-
  data.frame(unclass(summary(table_results_without_outliers)),
             check.names = FALSE)

write.csv(
  table_results_without_outliers_summary,
  "./ENM/12_statistics_no_outliers.csv", row.names = F)

write.csv(table_results_without_outliers,
          "./ENM/12_restricted_range_no_outliers_less15onlycharis.csv", row.names = F)



# Creating table for plot

# All species

n_sp_table_results1 <- table_results1 %>%
  group_by(restricted_range) %>%
  summarize(Species = n())

all_sp_mean <- left_join(n_sp_table_results1,mean_table_results1,
                         by="restricted_range")

write.csv(all_sp_mean,
          "./ENM/12_allsp_restrict_nsp_mean_less15onlycharis.csv", row.names = F)

summary(all_sp_mean)

# Only outliers species

n_sp_table_results_outliers <- table_results_outliers %>%
  group_by(restricted_range) %>%
  summarize(Species = n())

outliers_mean <- left_join(n_sp_table_results_outliers,
                         mean_table_results_outliers,
                         by="restricted_range")

write.csv(outliers_mean,
          "./ENM/12_allsp_restrict_nsp_mean_outliers_less15onlycharis.csv", row.names = F)

summary(outliers_mean)

# Only no outliers species

n_sp_table_results_without_outliers <- table_results_without_outliers %>%
  group_by(restricted_range) %>%
  summarize(Species = n())

no_outliers_mean <- left_join(n_sp_table_results_without_outliers,
                           mean_table_results_without_outliers,
                           by="restricted_range")

write.csv(no_outliers_mean,
          "./ENM/12_allsp_restrict_nsp_mean_no_outliers_less15onlycharis.csv", row.names = F)

summary(no_outliers_mean)



# # Table S1 --------------------------------------------------------------

no_out_col <- rep(0,length(table_results_without_outliers$Species))
table_results_without_outliers$outliers <- no_out_col
table_no_out_col <-table_results_without_outliers[,c(1,5)]

out_col <- rep(1,length(table_results_outliers$Species))
table_results_outliers$outliers <- out_col
table_out_col <-table_results_outliers[,c(1,5)]

table_out_noout <- rbind(table_no_out_col,table_out_col)

table_S1_bef <- read_csv("./ENM/11_area_sp_BR_less15onlycharis.csv")

table_S1 <- left_join(table_S1_bef,table_out_noout, by="Species")


n_occs <- read.csv("./ENM/data/03_n_thinned_records.csv")
n_occs$species <-
  gsub(x = n_occs$species, pattern = " ", replacement = "_")
colnames(n_occs) <- c("Species", "n_raw", "n_remaining")

table_S1 <- left_join(table_S1, n_occs, by = "Species")

write.csv(table_S1,
          "./Table_results/12_sp_table_S1_less15onlycharis.csv", row.names = F)

# Summary of results

mean_no_out <- mean(no_outliers_mean$mean)
min_no_out <- min(no_outliers_mean$mean)
max_no_out <- max(no_outliers_mean$mean)
n_sp_no_out <- dim(table_results_without_outliers)[1]

no_outliers_stat <- c(n_sp_no_out,min_no_out,mean_no_out,max_no_out)

mean_out <- mean(outliers_mean$mean)
min_out <- min(outliers_mean$mean)
max_out <- max(outliers_mean$mean)
n_sp_out <- dim(table_results_outliers)[1]

outliers_stat <- c(n_sp_out,min_out,mean_out,max_out)

table_manusc <- rbind(no_outliers_stat,outliers_stat)
colnames(table_manusc) <- c("species number","minimum value", "mean", "maximum value")

write.csv(table_manusc,
          "./ENM/12_principal_results_less15onlycharis.csv", row.names = F)

