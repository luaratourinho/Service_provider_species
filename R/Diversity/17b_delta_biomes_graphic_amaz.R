# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Last update: 05 Aug 2022

# Plotting richness maps for all services

# Required packages

library(dplyr)
library(tidyverse)
library("spatialEco")
library(raster)
library(rgdal)
library(ggmap)
library(ggplot2)
library(dplyr)

# Creating bioma tables ---------------------------------------------------

tab.mud_2 <- read.csv("./Table_results/17_ES_tab.mud_domains_noout.csv")
tab.mud_2_round <- round(tab.mud_2[,-1])
tab.mud_2_round$ES <- tab.mud_2$ES
tab.mud_2_round <- tab.mud_2_round[,c(19, 1:18)]
head(tab.mud_2_round)

# AMAZON

amaz <- tab.mud_2_round[,1:4]
head(amaz)

# transpose all but the first column (name)
n <- amaz$ES
amaz_t <- as.data.frame(t(amaz[,-1]))
colnames(amaz_t) <- n
amaz_t$change <- c(1:3)
head(amaz_t)

# test <- read_csv("./test.csv")
group <- rep("charismatic",100)
ch1<- rep(1, 38)
ch2<- rep(2, 1)
ch3<- rep(3, 61)
change <- c(ch1,ch2,ch3)

es_change_cha <- cbind(group, change)

group <- rep("ecotour",100)
ch1<- rep(1, 35)
ch2<- rep(2, 8)
ch3<- rep(3, 57)
change <- c(ch1,ch2,ch3)

es_change_eco <- cbind(group, change)

group <- rep("carrion",100)
ch1<- rep(1, 17)
ch2<- rep(2, 22)
ch3<- rep(3, 61)
change <- c(ch1,ch2,ch3)

es_change_car <- cbind(group, change)

group <- rep("pollin",100)
ch1<- rep(1, 43)
ch2<- rep(2, 3)
ch3<- rep(3, 54)
change <- c(ch1,ch2,ch3)

es_change_pol <- cbind(group, change)

group <- rep("top_pred",100)
ch1<- rep(1, 10)
ch2<- rep(2, 47)
ch3<- rep(3, 42)
change <- c(ch1,ch2,ch3)

es_change_top <- cbind(group, change)

group <- rep("dispers",100)
ch1<- rep(1, 38)
ch2<- rep(2, 6)
ch3<- rep(3, 56)
change <- c(ch1,ch2,ch3)

es_change_dis <- cbind(group, change)

group <- rep("pest",100)
ch1<- rep(1, 26)
ch2<- rep(2, 1)
ch3<- rep(3, 74)
change <- c(ch1,ch2,ch3)

es_change_pes <- cbind(group, change)

group <- rep("nutrient",100)
ch1<- rep(1, 28)
ch2<- rep(2, 7)
ch3<- rep(3, 65)
change <- c(ch1,ch2,ch3)

es_change_nut <- cbind(group, change)

group <- rep("engineers",100)
ch1<- rep(1, 12)
ch2<- rep(2, 38)
ch3<- rep(3, 50)
change <- c(ch1,ch2,ch3)

es_change_eng <- cbind(group, change)

group <- rep("rodent_ctrl",100)
ch1<- rep(1, 8)
ch2<- rep(2, 8)
ch3<- rep(3, 83)
change <- c(ch1,ch2,ch3)

es_change_rod <- cbind(group, change)

group <- rep("sentinel",100)
ch1<- rep(1, 25)
ch2<- rep(2, 1)
ch3<- rep(3, 74)
change <- c(ch1,ch2,ch3)

es_change_sen <- cbind(group, change)

group <- rep("all_ES",100)
ch1<- rep(1, 12)
ch2<- rep(2, 22)
ch3<- rep(3, 67)
change <- c(ch1,ch2,ch3)

es_change_all <- cbind(group, change)

df_ES_change <- rbind(es_change_cha, es_change_eco, es_change_car,
                      es_change_pol, es_change_top, es_change_dis,
                      es_change_pes, es_change_nut, es_change_eng,
                      es_change_rod, es_change_sen, es_change_all)

df_temp <- transform(df_ES_change, change = as.numeric(change))
rownames(df_temp) <- NULL


# Plot --------------------------------------------------------------------

d_ama <- df_temp %>% 
  group_by(group, change) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

ama <- ggplot(d_ama, aes(
  x = factor(group),
  y = perc * 100,
  fill = factor(change))) +
  geom_bar(stat = "identity", width = 0.4) +
  labs(x = "Ecosystem Services", y = "Percent", fill = "Change") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_line(color = "gray", size = 0.5),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 90),
    panel.background = element_rect(colour = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.line = element_line(color = "gray", size = 0.5)
  )  +
  theme(legend.title = element_blank()) +
  #ggtitle("Amazon")
  labs(title ="Amazon") +
  theme(plot.title = element_text(size=16))+
  scale_fill_discrete(labels = c("Loss", "Stability", "Gain")) +
  scale_fill_manual(values = c("#FA5F55", "#FFF68F", "#6495ED"))
 

# ATLANTIC FOREST

mata <- tab.mud_2_round[,c(1,5:7)]
head(mata)

# transpose all but the first column (name)
n <- mata$ES
df_t <- as.data.frame(t(mata[,-1]))
colnames(df_t) <- n
df_t$change <- c(1:3)
head(df_t)

# test <- read_csv("./test.csv")
group <- rep("charismatic",100)
ch1<- rep(1, 61)
ch2<- rep(2, 1)
ch3<- rep(3, 38)
change <- c(ch1,ch2,ch3)

es_change_cha <- cbind(group, change)

group <- rep("ecotour",100)
ch1<- rep(1, 28)
ch2<- rep(2, 10)
ch3<- rep(3, 61)
change <- c(ch1,ch2,ch3)

es_change_eco <- cbind(group, change)

group <- rep("carrion",100)
ch1<- rep(1, 67)
ch2<- rep(2, 11)
ch3<- rep(3, 22)
change <- c(ch1,ch2,ch3)

es_change_car <- cbind(group, change)

group <- rep("pollin",100)
ch1<- rep(1, 73)
ch2<- rep(2, 2)
ch3<- rep(3, 25)
change <- c(ch1,ch2,ch3)

es_change_pol <- cbind(group, change)

group <- rep("top_pred",100)
ch1<- rep(1, 12)
ch2<- rep(2, 48)
ch3<- rep(3, 40)
change <- c(ch1,ch2,ch3)

es_change_top <- cbind(group, change)

group <- rep("dispers",100)
ch1<- rep(1, 62)
ch2<- rep(2, 9)
ch3<- rep(3, 29)
change <- c(ch1,ch2,ch3)

es_change_dis <- cbind(group, change)

group <- rep("pest",100)
ch1<- rep(1, 49)
ch2<- rep(2, 1)
ch3<- rep(3, 51)
change <- c(ch1,ch2,ch3)

es_change_pes <- cbind(group, change)

group <- rep("nutrient",100)
ch1<- rep(1, 21)
ch2<- rep(2, 2)
ch3<- rep(3, 77)
change <- c(ch1,ch2,ch3)

es_change_nut <- cbind(group, change)

group <- rep("engineers",100)
ch1<- rep(1, 21)
ch2<- rep(2, 18)
ch3<- rep(3, 61)
change <- c(ch1,ch2,ch3)

es_change_eng <- cbind(group, change)

group <- rep("rodent_ctrl",100)
ch1<- rep(1, 66)
ch2<- rep(2, 13)
ch3<- rep(3, 22)
change <- c(ch1,ch2,ch3)

es_change_rod <- cbind(group, change)

group <- rep("sentinel",100)
ch1<- rep(1, 51)
ch2<- rep(2, 1)
ch3<- rep(3, 48)
change <- c(ch1,ch2,ch3)

es_change_sen <- cbind(group, change)

group <- rep("all_ES",100)
ch1<- rep(1, 23)
ch2<- rep(2, 38)
ch3<- rep(3, 39)
change <- c(ch1,ch2,ch3)

es_change_all <- cbind(group, change)

df_ES_change <- rbind(es_change_cha, es_change_eco, es_change_car,
                      es_change_pol, es_change_top, es_change_dis,
                      es_change_pes, es_change_nut, es_change_eng,
                      es_change_rod, es_change_sen, es_change_all)

df_temp <- transform(df_ES_change, change = as.numeric(change))
rownames(df_temp) <- NULL


# Plot --------------------------------------------------------------------

d_mat <- df_temp %>% 
  group_by(group, change) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

mat <- ggplot(d_mat, aes(
  x = factor(group),
  y = perc * 100,
  fill = factor(change))) +
  geom_bar(stat = "identity", width = 0.4) +
  labs(x = "Ecosystem Services", y = "Percent", fill = "Change") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_line(color = "gray", size = 0.5),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 90),
    panel.background = element_rect(colour = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.line = element_line(color = "gray", size = 0.5)
  )  +
  theme(legend.title = element_blank()) +
  labs(title ="Atlantic Forest") +
  theme(plot.title = element_text(size=16))+
  scale_fill_discrete(labels = c("Loss", "Stability", "Gain")) +
  scale_fill_manual(values = c("#FA5F55", "#FFF68F", "#6495ED"))


# PAMPA

pamp <- tab.mud_2_round[,c(1,8:10)]
head(pamp)

# transpose all but the first column (name)
n <- pamp$ES
df_t <- as.data.frame(t(pamp[,-1]))
colnames(df_t) <- n
df_t$change <- c(1:3)
head(df_t)

# test <- read_csv("./test.csv")
group <- rep("charismatic",100)
ch1<- rep(1, 86)
ch2<- rep(2, 1)
ch3<- rep(3, 13)
change <- c(ch1,ch2,ch3)

es_change_cha <- cbind(group, change)

group <- rep("ecotour",100)
ch1<- rep(1, 30)
ch2<- rep(2, 28)
ch3<- rep(3, 42)
change <- c(ch1,ch2,ch3)

es_change_eco <- cbind(group, change)

group <- rep("carrion",100)
ch1<- rep(1, 14)
ch2<- rep(2, 64)
ch3<- rep(3, 21)
change <- c(ch1,ch2,ch3)

es_change_car <- cbind(group, change)

group <- rep("pollin",100)
ch1<- rep(1, 43)
ch2<- rep(2, 44)
ch3<- rep(3, 13)
change <- c(ch1,ch2,ch3)

es_change_pol <- cbind(group, change)

group <- rep("top_pred",100)
ch1<- rep(1, 1)
ch2<- rep(2, 91)
ch3<- rep(3, 8)
change <- c(ch1,ch2,ch3)

es_change_top <- cbind(group, change)

group <- rep("dispers",100)
ch1<- rep(1, 58)
ch2<- rep(2, 20)
ch3<- rep(3, 22)
change <- c(ch1,ch2,ch3)

es_change_dis <- cbind(group, change)

group <- rep("pest",100)
ch1<- rep(1, 42)
ch2<- rep(2, 3)
ch3<- rep(3, 55)
change <- c(ch1,ch2,ch3)

es_change_pes <- cbind(group, change)

group <- rep("nutrient",100)
ch1<- rep(1, 33)
ch2<- rep(2, 19)
ch3<- rep(3, 48)
change <- c(ch1,ch2,ch3)

es_change_nut <- cbind(group, change)

group <- rep("engineers",100)
ch1<- rep(1, 42)
ch2<- rep(2, 47)
ch3<- rep(3, 11)
change <- c(ch1,ch2,ch3)

es_change_eng <- cbind(group, change)

group <- rep("rodent_ctrl",100)
ch1<- rep(1, 77)
ch2<- rep(2, 15)
ch3<- rep(3, 8)
change <- c(ch1,ch2,ch3)

es_change_rod <- cbind(group, change)

group <- rep("sentinel",100)
ch1<- rep(1, 15)
ch2<- rep(2, 75)
ch3<- rep(3, 42)
change <- c(ch1,ch2,ch3)

es_change_sen <- cbind(group, change)

group <- rep("all_ES",100)
ch1<- rep(1, 15)
ch2<- rep(2, 75)
ch3<- rep(3, 10)
change <- c(ch1,ch2,ch3)

es_change_all <- cbind(group, change)

df_ES_change <- rbind(es_change_cha, es_change_eco, es_change_car,
                      es_change_pol, es_change_top, es_change_dis,
                      es_change_pes, es_change_nut, es_change_eng,
                      es_change_rod, es_change_sen, es_change_all)

df_temp <- transform(df_ES_change, change = as.numeric(change))
rownames(df_temp) <- NULL


# Plot --------------------------------------------------------------------

d_pam <- df_temp %>% 
  group_by(group, change) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

pam <- ggplot(d_pam, aes(
  x = factor(group),
  y = perc * 100,
  fill = factor(change))) +
  geom_bar(stat = "identity", width = 0.4) +
  labs(x = "Ecosystem Services", y = "Percent", fill = "Change") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_line(color = "gray", size = 0.5),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 90),
    panel.background = element_rect(colour = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.line = element_line(color = "gray", size = 0.5)
  )  +
  theme(legend.title = element_blank()) +
  labs(title ="Pampa") +
  theme(plot.title = element_text(size=16))+
  scale_fill_discrete(labels = c("Loss", "Stability", "Gain")) +
  scale_fill_manual(values = c("#FA5F55", "#FFF68F", "#6495ED"))


# CERRADO

cer <- tab.mud_2_round[,c(1,11:13)]
head(cer)

# transpose all but the first column (name)
n <- cer$ES
df_t <- as.data.frame(t(cer[,-1]))
colnames(df_t) <- n
df_t$change <- c(1:3)
head(df_t)

# test <- read_csv("./test.csv")
group <- rep("charismatic",100)
ch1<- rep(1, 41)
ch2<- rep(2, 4)
ch3<- rep(3, 54)
change <- c(ch1,ch2,ch3)

es_change_cha <- cbind(group, change)

group <- rep("ecotour",100)
ch1<- rep(1, 37)
ch2<- rep(2, 23)
ch3<- rep(3, 40)
change <- c(ch1,ch2,ch3)

es_change_eco <- cbind(group, change)

group <- rep("carrion",100)
ch1<- rep(1, 29)
ch2<- rep(2, 42)
ch3<- rep(3, 29)
change <- c(ch1,ch2,ch3)

es_change_car <- cbind(group, change)

group <- rep("pollin",100)
ch1<- rep(1, 54)
ch2<- rep(2, 7)
ch3<- rep(3, 38)
change <- c(ch1,ch2,ch3)

es_change_pol <- cbind(group, change)

group <- rep("top_pred",100)
ch1<- rep(1, 15)
ch2<- rep(2, 54)
ch3<- rep(3, 31)
change <- c(ch1,ch2,ch3)

es_change_top <- cbind(group, change)

group <- rep("dispers",100)
ch1<- rep(1, 45)
ch2<- rep(2, 11)
ch3<- rep(3, 44)
change <- c(ch1,ch2,ch3)

es_change_dis <- cbind(group, change)

group <- rep("pest",100)
ch1<- rep(1, 22)
ch2<- rep(2, 3)
ch3<- rep(3, 75)
change <- c(ch1,ch2,ch3)

es_change_pes <- cbind(group, change)

group <- rep("nutrient",100)
ch1<- rep(1, 21)
ch2<- rep(2, 10)
ch3<- rep(3, 69)
change <- c(ch1,ch2,ch3)

es_change_nut <- cbind(group, change)

group <- rep("engineers",100)
ch1<- rep(1, 14)
ch2<- rep(2, 40)
ch3<- rep(3, 47)
change <- c(ch1,ch2,ch3)

es_change_eng <- cbind(group, change)

group <- rep("rodent_ctrl",100)
ch1<- rep(1, 25)
ch2<- rep(2, 23)
ch3<- rep(3, 52)
change <- c(ch1,ch2,ch3)

es_change_rod <- cbind(group, change)

group <- rep("sentinel",100)
ch1<- rep(1, 41)
ch2<- rep(2, 9)
ch3<- rep(3, 50)
change <- c(ch1,ch2,ch3)

es_change_sen <- cbind(group, change)

group <- rep("all_ES",100)
ch1<- rep(1, 18)
ch2<- rep(2, 36)
ch3<- rep(3, 45)
change <- c(ch1,ch2,ch3)

es_change_all <- cbind(group, change)

df_ES_change <- rbind(es_change_cha, es_change_eco, es_change_car,
                      es_change_pol, es_change_top, es_change_dis,
                      es_change_pes, es_change_nut, es_change_eng,
                      es_change_rod, es_change_sen, es_change_all)

df_temp <- transform(df_ES_change, change = as.numeric(change))
rownames(df_temp) <- NULL


# Plot --------------------------------------------------------------------

d_cer <- df_temp %>% 
  group_by(group, change) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

cer <- ggplot(d_cer, aes(
  x = factor(group),
  y = perc * 100,
  fill = factor(change))) +
  geom_bar(stat = "identity", width = 0.4) +
  labs(x = "Ecosystem Services", y = "Percent", fill = "Change") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_line(color = "gray", size = 0.5),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 90),
    panel.background = element_rect(colour = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.line = element_line(color = "gray", size = 0.5)
  )  +
  theme(legend.title = element_blank()) +
  labs(title ="Cerrado") +
  theme(plot.title = element_text(size=16))+
  scale_fill_discrete(labels = c("Loss", "Stability", "Gain")) +
  scale_fill_manual(values = c("#FA5F55", "#FFF68F", "#6495ED"))

# PANTANAL

pan <- tab.mud_2_round[,c(1,14:16)]
head(pan)

# transpose all but the first column (name)
n <- pan$ES
df_t <- as.data.frame(t(pan[,-1]))
colnames(df_t) <- n
df_t$change <- c(1:3)
head(df_t)

# test <- read_csv("./test.csv")
group <- rep("charismatic",100)
ch1<- rep(1, 94)
ch2<- rep(2, 1)
ch3<- rep(3, 5)
change <- c(ch1,ch2,ch3)

es_change_cha <- cbind(group, change)

group <- rep("ecotour",100)
ch1<- rep(1, 71)
ch2<- rep(2, 17)
ch3<- rep(3, 12)
change <- c(ch1,ch2,ch3)

es_change_eco <- cbind(group, change)

group <- rep("carrion",100)
ch1<- rep(1, 27)
ch2<- rep(2, 13)
ch3<- rep(3, 60)
change <- c(ch1,ch2,ch3)

es_change_car <- cbind(group, change)

group <- rep("pollin",100)
ch1<- rep(1, 56)
ch2<- rep(2, 9)
ch3<- rep(3, 35)
change <- c(ch1,ch2,ch3)

es_change_pol <- cbind(group, change)

group <- rep("top_pred",100)
ch1<- rep(1, 44)
ch2<- rep(2, 41)
ch3<- rep(3, 15)
change <- c(ch1,ch2,ch3)

es_change_top <- cbind(group, change)

group <- rep("dispers",100)
ch1<- rep(1, 87)
ch2<- rep(2, 5)
ch3<- rep(3, 8)
change <- c(ch1,ch2,ch3)

es_change_dis <- cbind(group, change)

group <- rep("pest",100)
ch1<- rep(1, 27)
ch2<- rep(2, 1)
ch3<- rep(3, 72)
change <- c(ch1,ch2,ch3)

es_change_pes <- cbind(group, change)

group <- rep("nutrient",100)
ch1<- rep(1, 90)
ch2<- rep(2, 3)
ch3<- rep(3, 7)
change <- c(ch1,ch2,ch3)

es_change_nut <- cbind(group, change)

group <- rep("engineers",100)
ch1<- rep(1, 3)
ch2<- rep(2, 92)
ch3<- rep(3, 5)
change <- c(ch1,ch2,ch3)

es_change_eng <- cbind(group, change)

group <- rep("rodent_ctrl",100)
ch1<- rep(1, 37)
ch2<- rep(2, 26)
ch3<- rep(3, 37)
change <- c(ch1,ch2,ch3)

es_change_rod <- cbind(group, change)

group <- rep("sentinel",100)
ch1<- rep(1, 93)
ch2<- rep(2, 1)
ch3<- rep(3, 5)
change <- c(ch1,ch2,ch3)

es_change_sen <- cbind(group, change)

group <- rep("all_ES",100)
ch1<- rep(1, 61)
ch2<- rep(2, 34)
ch3<- rep(3, 5)
change <- c(ch1,ch2,ch3)

es_change_all <- cbind(group, change)

df_ES_change <- rbind(es_change_cha, es_change_eco, es_change_car,
                      es_change_pol, es_change_top, es_change_dis,
                      es_change_pes, es_change_nut, es_change_eng,
                      es_change_rod, es_change_sen, es_change_all)

df_temp <- transform(df_ES_change, change = as.numeric(change))
rownames(df_temp) <- NULL


# Plot --------------------------------------------------------------------

d_pant <- df_temp %>% 
  group_by(group, change) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

pan <- ggplot(d_pant, aes(
  x = factor(group),
  y = perc * 100,
  fill = factor(change))) +
  geom_bar(stat = "identity", width = 0.4) +
  labs(x = "Ecosystem Services", y = "Percent", fill = "Change") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_line(color = "gray", size = 0.5),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 90),
    panel.background = element_rect(colour = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.line = element_line(color = "gray", size = 0.5)
  )  +
  theme(legend.title = element_blank()) +
  labs(title ="Pantanal") +
  theme(plot.title = element_text(size=16))+
  scale_fill_discrete(labels = c("Loss", "Stability", "Gain")) +
  scale_fill_manual(values = c("#FA5F55", "#FFF68F", "#6495ED"))

# CAATINGA

caa <- tab.mud_2_round[,c(1,17:19)]
head(caa)

# transpose all but the first column (name)
n <- caa$ES
df_t <- as.data.frame(t(caa[,-1]))
colnames(df_t) <- n
df_t$change <- c(1:3)
head(df_t)

# test <- read_csv("./test.csv")
group <- rep("charismatic",100)
ch1<- rep(1, 36)
ch2<- rep(2, 17)
ch3<- rep(3, 47)
change <- c(ch1,ch2,ch3)

es_change_cha <- cbind(group, change)

group <- rep("ecotour",100)
ch1<- rep(1, 5)
ch2<- rep(2, 42)
ch3<- rep(3, 53)
change <- c(ch1,ch2,ch3)

es_change_eco <- cbind(group, change)

group <- rep("carrion",100)
ch1<- rep(1, 28)
ch2<- rep(2, 53)
ch3<- rep(3, 19)
change <- c(ch1,ch2,ch3)

es_change_car <- cbind(group, change)

group <- rep("pollin",100)
ch1<- rep(1, 66)
ch2<- rep(2, 6)
ch3<- rep(3, 28)
change <- c(ch1,ch2,ch3)

es_change_pol <- cbind(group, change)

group <- rep("top_pred",100)
ch1<- rep(1, 5)
ch2<- rep(2, 80)
ch3<- rep(3, 15)
change <- c(ch1,ch2,ch3)

es_change_top <- cbind(group, change)

group <- rep("dispers",100)
ch1<- rep(1, 50)
ch2<- rep(2, 17)
ch3<- rep(3, 33)
change <- c(ch1,ch2,ch3)

es_change_dis <- cbind(group, change)

group <- rep("pest",100)
ch1<- rep(1, 45)
ch2<- rep(2, 4)
ch3<- rep(3, 51)
change <- c(ch1,ch2,ch3)

es_change_pes <- cbind(group, change)

group <- rep("nutrient",100)
ch1<- rep(1, 25)
ch2<- rep(2, 19)
ch3<- rep(3, 55)
change <- c(ch1,ch2,ch3)

es_change_nut <- cbind(group, change)

group <- rep("engineers",100)
ch1<- rep(1, 4)
ch2<- rep(2, 76)
ch3<- rep(3, 20)
change <- c(ch1,ch2,ch3)

es_change_eng <- cbind(group, change)

group <- rep("rodent_ctrl",100)
ch1<- rep(1, 19)
ch2<- rep(2, 54)
ch3<- rep(3, 26)
change <- c(ch1,ch2,ch3)

es_change_rod <- cbind(group, change)

group <- rep("sentinel",100)
ch1<- rep(1, 60)
ch2<- rep(2, 4)
ch3<- rep(3, 35)
change <- c(ch1,ch2,ch3)

es_change_sen <- cbind(group, change)

group <- rep("all_ES",100)
ch1<- rep(1, 17)
ch2<- rep(2, 55)
ch3<- rep(3, 27)
change <- c(ch1,ch2,ch3)

es_change_all <- cbind(group, change)

df_ES_change <- rbind(es_change_cha, es_change_eco, es_change_car,
                      es_change_pol, es_change_top, es_change_dis,
                      es_change_pes, es_change_nut, es_change_eng,
                      es_change_rod, es_change_sen, es_change_all)

df_temp <- transform(df_ES_change, change = as.numeric(change))
rownames(df_temp) <- NULL


# Plot --------------------------------------------------------------------

d_caa <- df_temp %>% 
  group_by(group, change) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

caa <- ggplot(d_caa, aes(
  x = factor(group),
  y = perc * 100,
  fill = factor(change))) +
  geom_bar(stat = "identity", width = 0.4) +
  labs(x = "Ecosystem Services", y = "Percent", fill = "Change") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_line(color = "gray", size = 0.5),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 90),
    panel.background = element_rect(colour = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.line = element_line(color = "gray", size = 0.5)
  )  +
  theme(legend.title = element_blank()) +
  labs(title ="Caatinga") +
  theme(plot.title = element_text(size=16))+
  scale_fill_discrete(labels = c("Loss", "Stability", "Gain")) +
  scale_fill_manual(values = c("#FA5F55", "#FFF68F", "#6495ED"))

f_arrange <- grid.arrange(ama, mat, pam, cer, pan, caa, nrow = 3)
ggsave(
  f_arrange,
  file = "./Figures/Delta_biomes_3row.tiff",
  height = 100,
  width = 80,
  units = "cm"
)
