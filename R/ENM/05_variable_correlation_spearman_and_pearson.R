
# Credits ---------------------------

# Edited by
# Luara Tourinho (https://github.com/luaratourinho)

# Using a set of script from github, e.g.: 
# Diogo S. B. Rocha (https://github.com/diogosbr)
# Daniele Moreira (https://github.com/daniomoreira)

# Last update: 12 Jul 2022


# Select variable with less collinearity ---------------------------------------


library(raster)
library(dplyr)
library(corrplot)



# list of files of present variables
present_list <- list.files("./ENM/env_cropped/present", pattern = "tif$", full.names = T)

# object with present variables
present_ras <- stack(present_list)

# id from cells without NA
mi <- Which(present_ras[[1]], cells = TRUE)

# sampling cells to extract values
sampled <- sample(mi, 5000)

# values of selected cells from rasters of present variables
vals <- present_ras[sampled]


# An alternative using spearman ------------------------------------------------

# selecting variables to exclude with correlation 0.6 or more
# First try low values, as 0.6, if it return few variables, try higher as 0.7
exclude_vars <- caret::findCorrelation(cor(vals, method = 'spearman'), cutoff = 0.6, names = TRUE)
all_table <- as.data.frame(cor(vals, method = 'spearman'))

# selecting variables with lower correlation (<0.6)
pres_vars_sel <- present_ras[[which(!names(present_ras) %in% exclude_vars)]]
pres_vars_sel

# selecting variables with lower correlation (<0.6)
pres_vars_sel_names <- names(present_ras)[!names(present_ras) %in% exclude_vars]
pres_vars_sel_names
# [1] "X_wc2.1_10m_bio_10" "X_wc2.1_10m_bio_14" "X_wc2.1_10m_bio_18" "X_wc2.1_10m_bio_2" 
# [5] "X_wc2.1_10m_bio_3" 

# An alternative using pearson -------------------------------------------------

exclude_vars2 <- caret::findCorrelation(cor(vals, method = 'pearson'), cutoff = 0.6, names = TRUE)
all_table2 <- as.data.frame(cor(vals, method = 'pearson'))

pres_vars_sel2 <- present_ras[[which(!names(present_ras) %in% exclude_vars2)]]
pres_vars_sel2

pres_vars_sel_names2 <- names(present_ras)[!names(present_ras) %in% exclude_vars2]
pres_vars_sel_names2
# [1] "X_wc2.1_10m_bio_10" "X_wc2.1_10m_bio_14" "X_wc2.1_10m_bio_15" "X_wc2.1_10m_bio_18"
# [5] "X_wc2.1_10m_bio_2" 


# creating directory
dir.create(paste0("./ENM/env_sel"))
# Add the chosen variables there

write.csv(all_table, "./ENM/env_sel/5_variables_correlation_spearman.csv")
write.csv(all_table2, "./ENM/env_sel/5_variables_correlation_pearson.csv")

# Check Worldclim to chose which set of variable make more sense, if by spearman
# or pearson
# https://www.worldclim.org/data/bioclim.html

# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# BIO3 = Isothermality (BIO2/BIO7) (×100)
# BIO10 = Mean Temperature of Warmest Quarter
# BIO14 = Precipitation of Driest Month
# BIO18 = Precipitation of Warmest Quarter
# BIO15 = Precipitation Seasonality (Coefficient of Variation)


# Building graphics -------------------------------------------------------

# corrplot ----------------------------------------------------------------

# I chose spearman
corrplot_graph <- cor(as.matrix(all_table))
# As smaller the square, as less significant the correlation
# We want to the less significant relationships
corrplot(corrplot_graph, method="square")



# Exploring designs of corrplot... ----------------------------------------

#Combining correlogram with the significance test
res1 <- cor.mtest(corrplot_graph, conf.level = .95)
## specialized the insignificant value according to the significant level
corrplot(corrplot_graph, p.mat = res1$p, sig.level = .05) 
## add p-values on no significant coefficient
corrplot(corrplot_graph, p.mat = res1$p, insig = "p-value") 
corrplot(corrplot_graph, method = "circle")
# Display the correlation coefficient
corrplot(corrplot_graph, method = "number") 
#corrplot.mixed() is a wrapped function for mixed visualization style.
corrplot.mixed(corrplot_graph) 
#"FPC" for the first principal component order
corrplot(corrplot_graph, type = "upper", order = "FPC")
corrplot(corrplot_graph, method="square", type = "upper", order = "FPC")

