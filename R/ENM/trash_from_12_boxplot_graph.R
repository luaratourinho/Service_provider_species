# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Last update: 20 Jul 2022


# Required packages

library(tidyverse)
library(dplyr)
library(tidyverse)
require('fBasics')
library(ggplot2)


# Outliers
table_results_outliers <- read.csv(
          "./ENM/12_restricted_range_outliers.csv")

outliers <- rep(1, length(table_results_outliers$Species))
table_results_outliers$outliers <- outliers

table_results_outliers <- table_results_outliers[,c(3,5)]

# No outliers

table_results_without_outliers <- read.csv(
  "./ENM/12_restricted_range_no_outliers.csv")

no_outliers <- rep(0, length(table_results_without_outliers$Species))
table_results_without_outliers$outliers <- no_outliers

table_results_without_outliers <- table_results_without_outliers[,c(3,5)]

# Creating table for plot

table_plot <- full_join(table_results_outliers, table_results_without_outliers)



# Plot --------------------------------------------------------------------


ggplot(table_results_without_outliers, aes(y=percentage_change, x=outliers)) + 
  geom_boxplot()




(g <- table_plot %>%
    
    ggplot(aes(x = outliers, y = percentage_change, 
               fill = outliers, group = outliers)) +
    geom_boxplot(aes(fill = outliers), varwidth = TRUE) +
    #geom_text(data = labs1, label = textos1, check_overlap = TRUE, size = 3.2, nudge_x = .5) +
    theme_classic() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 14, face = "plain"),
          axis.title.y = element_blank(),
          legend.position = "bottom", legend.title = element_blank(), 
          legend.box.background = element_rect(color = "white", size = 0.3),
          axis.ticks = element_blank()) +
    scale_fill_manual(values = c("#5CACEE", "#FFD700"),
                      labels = c("Outliers", "No outliers")) + 
    xlab("\nPercentage of change") + 
    #ylim(0, 1) +
    # facet_wrap(vars(name), nrow = 3, 
    #            labeller = labeller(name = supp.labs), 
    #            scales = "free_x") +
    theme(
      #strip.text.x = element_markdown(), 
      # element_text(size = 12, face = "bold", margin = margin(b = 10)
      strip.background = element_rect(color="white")))
