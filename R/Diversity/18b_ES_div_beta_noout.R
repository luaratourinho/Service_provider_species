# Credits ---------------------------

# Created by
# Micael Parreira 09/2019

# Edited by
# Luara Tourinho (https://github.com/luaratourinho)

# Last update: 20 Jul 2022


## Calculating delta, beta (turnover and nestedness) to different scenarios (from ENM)

# Required packages

library(raster)
library(dismo)
library(rgdal)
library(psych)



# List of rasters ---------------------------------------------------------


list.var <-
  list.files(paste0("./Diversity/Richness/BR_noout"),
             recursive = TRUE,
             pattern = "tif",
             full.names = TRUE)

cu_bin_list <- str_subset(list.var, 
                          paste0("^./Diversity/Richness/BR_noout/cu_bin_"))

cu_bin_list <- cu_bin_list[c(1:5,7:12)]
servicos.p.cer <- stack(cu_bin_list)

riq.serv.p.cer <- sum(servicos.p.cer > 0)
# riq.serv.p.cer_2 <- raster("./Diversity/Richness/BR_noout/cu_bin_ES.tif")

fu_bin_list <- str_subset(list.var, 
                          paste0("^./Diversity/Richness/BR_noout/fu_bin_"))

servicos.f.cer <- stack(fu_bin_list)
riq.serv.f.cer <- sum(servicos.f.cer > 0)


# Delta diversity ---------------------------------------------------------


## Calculating delta diversity of ES from the current to future

# delta.serv <- riq.serv.f.cer - riq.serv.p.cer
# rich_ES_fu <- raster("./Diversity/Richness/BR/fu_bin_ES.tif")
# rich_ES_cu <- raster("./Diversity/Richness/BR/cu_bin_ES.tif")
# delta.serv <- rich_ES_fu - rich_ES_cu
delta.serv <- raster("./Diversity/Richness/Diff_BR_noout/fu_cu_bin_noout_ES.tif")

delta.serv.vals <- na.omit(values(delta.serv))
summary(delta.serv.vals)
table(delta.serv.vals)

# Loss, gain and stability values for delta

tab.mud <- cbind(Loss = length(which(delta.serv.vals < 0)),
                 Stability =  length(which(delta.serv.vals == 0)),
                 Gain = length(which(delta.serv.vals > 0)))

write.csv(tab.mud, "./Diversity/beta/15_delta_results_noout.csv", row.names = F)


# Loss/Gain histogram of ES

par(mfrow = c(1, 1), mar = c(5, 5, 5, 5))
hist(delta.serv,
  xlab = 'Delta', ylab = 'Grid cell frequency',
  main = paste('Loss/Gain histogram', sep = '-'),
  xlim = c(-20, 20), nclass = 20, cex.lab = 1.2)
abline(v = 0, col = 'red', lty = 2, lwd = 2)



# Turnover, Nestedness, Sorense -------------------------------------------

# Calculating turnover, nestedness and sorense of ES between current and future

pres01 <- na.omit(values(as.integer(servicos.p.cer > 0)))
fut01 <- na.omit(values(as.integer(servicos.f.cer > 0)))

serv.vals <-
  values(servicos.p.cer[[1]])
xy <- xyFromCell(servicos.p.cer, 1:ncell(servicos.p.cer))
xy.sem.na <- na.omit(cbind(xy, serv.vals))[,1:2]

a <-
  b <-
  c <-
  a.p <-
  b.p <-
  c.p <-
  min.bc <-
  max.bc <- sum.bc <- beta.sim <- beta.nes <- beta.sor <- NULL

for(k in 1:nrow(pres01)){
  for(l in 1:ncol(pres01)){
    if(pres01[k,l] + fut01[k,l] == 2){
      a.p[l] <- 1
    } else {
      a.p[l] <- 0
    }
    if(pres01[k,l] - fut01[k,l] == 1){
      b.p[l] <- 1
    } else {
      b.p[l] <- 0
    }
    if(pres01[k,l] - fut01[k,l] == -1){
      c.p[l] <- 1
    } else {
      c.p[l] <- 0
    }
  }
  a[k] <- sum(a.p)
  b[k] <- sum(b.p)
  c[k] <- sum(c.p)
  
  min.bc[k] <- min(b[k], c[k])
  max.bc[k] <- max(b[k], c[k])
  sum.bc[k] <- sum(b[k], c[k])
  
  beta.sim[k] <- min.bc[k]/(min.bc[k] + a[k])
  beta.nes[k] <- ((max.bc[k] - min.bc[k]) / (2 * a[k] + sum.bc[k])) * (a[k] / (a[k] + min.bc[k]))
  beta.sor[k] <- sum.bc[k] / (2 * a[k] + sum.bc[k])
} # k (beta)

beta.sim.r <- rasterFromXYZ(cbind(xy.sem.na, beta.sim))
beta.nes.r <- rasterFromXYZ(cbind(xy.sem.na, beta.nes))
beta.sor.r <- rasterFromXYZ(cbind(xy.sem.na, beta.sor))


# Plot beta (nes e turn) and delta

plot(beta.sim.r, main = paste0('Turnover'))
plot(beta.nes.r, main = paste0('Nestedness'))
plot(beta.sor.r, main = paste0('Beta total'))
plot(delta.serv, main = paste0('Delta'))

# Proportion

beta.sim.r_2 <- beta.sim.r
beta.sim.r_2[beta.sim.r_2 != 0] <- 1
high_values <- ncell(which(beta.sim.r_2[] == 1))
low_values <- ncell(which(beta.sim.r_2[] == 0))
propor_high <- ((high_values/(high_values + low_values))*100)

beta.sor.r_2 <- beta.sor.r
beta.sor.r_2[beta.sor.r_2 != 0] <- 1
high_values <- ncell(which(beta.sor.r_2[] == 1))
low_values <- ncell(which(beta.sor.r_2[] == 0))
propor_high_2 <- ((high_values/(high_values + low_values))*100)

beta.nes.r_2 <- beta.nes.r
beta.nes.r_2[beta.nes.r_2 != 0] <- 1
high_values <- ncell(which(beta.nes.r_2[] == 1))
low_values <- ncell(which(beta.nes.r_2[] == 0))
propor_high_3 <- ((high_values/(high_values + low_values))*100)


# Save results

writeRaster(beta.sim.r, 
            "./Diversity/beta/beta_sim_noout.tif",
            format="GTiff", overwrite=T)

writeRaster(beta.nes.r, 
            "./Diversity/beta/beta_nes_noout.tif",
            format="GTiff", overwrite=T)

writeRaster(beta.sor.r, 
            "./Diversity/beta/beta_sor_noout.tif",
            format="GTiff", overwrite=T)

# writeRaster(delta.serv, 
#             "./Diversity/delta/delta_serv.tif",
#             format="GTiff", overwrite=T)


