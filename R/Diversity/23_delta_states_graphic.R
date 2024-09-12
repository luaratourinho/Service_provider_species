library(tidyverse)

# Open and organize table

results <- read_csv("./Diversity/Delta_states.csv")
tab.mud_2_round <- round(results[,c(-1,-6)])
tab.mud_2_round$ES <- results$ES
tab.mud_2_round$state <- results$acro
results <- tab.mud_2_round[,c(6, 5, 1:4)]

# Creating vector for the loop

ES <- c("charism","ecotour","carrion","pollin","top_pred","dispers",
        "pest","nutrient","engineers","rodent_ctrl","sentinel", "ES")

states <- shapefile("./Shp/BR_UF_2020.shp")
acro <- as.vector(states@data$SIGLA_UF)

n <-length(acro)

for(i in 1:n) {
  target_st <- results[results$state == acro[i],]
  es <- target_st$ES
  target_st_t <- data.frame(t(target_st[,-1]))
  target_st_t <- (target_st_t[-1,])
  colnames(target_st_t) <- es
  
  target_st_t$ES <- rep(acro[i],4)
  
  if (exists("results2")) {
    results2 <- rbind(results2, target_st_t)
  } else {
    results2 <- target_st_t
  }
}

amaz <- results2[,1:4]
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