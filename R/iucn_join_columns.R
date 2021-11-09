# Required packages
library(dplyr)
library(tidyverse)
library(plyr)
library(readr)


# IUCN tables (710 sp) ----------------------------------------------------

#mydata <- read.csv("Working_tables/mammals/habitats.csv")
mydata <- read.csv("Working_tables/mammals/assessments.csv")


# Load function
names_standardize <- function(splist) {
  fixed1 <- toupper(splist) # all up
  fixed2 <- gsub("CF\\.", "", fixed1)
  fixed3 <- gsub("AFF\\.", "", fixed2)
  fixed4 <- trimws(fixed3) # remove trailing and leading space
  fixed5 <- gsub("_", " ", fixed4) # change names separated by _ to space
  # Merge multiple spaces
  fixed6 <- gsub("(^X )|( X$)|( X )", " ", fixed5)
  fixed7 <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", fixed6, perl = TRUE)
  return(fixed7)
}


#
sci2 <- names_standardize(mydata$scientificName) # standardize names

sci3 <- sapply(strsplit(sci2, " "), function(x){paste(x[1:2], collapse = " ")})

species <- unique(sci3)

n <- length(species)

#countries <- character(n)
new_line <- list()
for (i in 1:n) {
  qual <- which(species[i] == sci3)
  new_line[[i]] <- mydata[qual[1], ]
  #new_line[[i]][, -(1:3)] <- apply(mydata[qual, -(1:3)], 2, paste, collapse = ", ") #for habitat table
  new_line[[i]][, ] <- apply(mydata[qual, ], 2, paste, collapse = ", ") #for assessments table
}

results <- do.call(rbind, new_line)
View(results)

#results_redu <- results[,c(3,5)]
results_redu <- results[,c(3:6,10:15,18,19)]
View(results_redu)

# only genus_epithet
names_standardize2 <- function(splist) {
  fixed2 <- gsub("CF\\.", "", splist)
  fixed3 <- gsub("AFF\\.", "", fixed2)
  fixed4 <- trimws(fixed3) # remove trailing and leading space
  fixed5 <- gsub("_", " ", fixed4) # change names separated by _ to space
  # Merge multiple spaces
  fixed6 <- gsub("(^X )|( X$)|( X )", " ", fixed5)
  fixed7 <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", fixed6, perl = TRUE)
  return(fixed7)
}


sci2 <- names_standardize2(results_redu$scientificName) # standardize names

genus_epithet <- sapply(strsplit(sci2, " "), function(x){paste(x[1:2], collapse = " ")})

results_redu$genus_epithet <- genus_epithet
results_redu$genus_epithet <- gsub(",","",as.character(results_redu$genus_epithet))


View(results_redu)
#results_redu <- results_redu[,c(3,2)]
#colnames(results_redu) <- c("genus_epithet", "habitats")


# Saving
#write.csv(results_redu, "Working_tables/mammals/changed/habitats.csv", row.names = F)
write.csv(results_redu, "Working_tables/mammals/changed/assessments.csv", row.names = F)
