#####
## Calcular delta, beta (turnover e aninhamento) para ENMs entre cenarios climaticos
#                                                            Micael Parreira 09/2019

rm(list = ls())

library(raster)
library(dismo)
library(rgdal)
library(psych)

species <- '~/Documents/Artigos publicados/Ferreira et al. 2021 - EMAS/Analises/modeling/species/'
shape <- '~/Documents/Artigos publicados/Ferreira et al. 2021 - EMAS/Analises/modeling/Shapes/'
thr.pres <- '~/Documents/Artigos publicados/Ferreira et al. 2021 - EMAS/Analises/modeling/outputs/presente/'
thr.f50.45 <- '~/Documents/Artigos publicados/Ferreira et al. 2021 - EMAS/Analises/modeling/outputs/rcp45 2050/'
thr.f50.85 <- '~/Documents/Artigos publicados/Ferreira et al. 2021 - EMAS/Analises/modeling/outputs/rcp85 2050/'
thr.f70.45 <- '~/Documents/Artigos publicados/Ferreira et al. 2021 - EMAS/Analises/modeling/outputs/rcp45 2070/'
thr.f70.85 <- '~/Documents/Artigos publicados/Ferreira et al. 2021 - EMAS/Analises/modeling/outputs/rcp85 2070/'
out.serv50.45 <- '~/Documents/Artigos publicados/Ferreira et al. 2021 - EMAS/Analises/modeling/outputs/out_servs/serv50_45/'
out.serv70.45 <- '~/Documents/Artigos publicados/Ferreira et al. 2021 - EMAS/Analises/modeling/outputs/out_servs/serv70_45/'
out.serv50.85 <- '~/Documents/Artigos publicados/Ferreira et al. 2021 - EMAS/Analises/modeling/outputs/out_servs/serv50_85/'
out.serv70.85 <- '~/Documents/Artigos publicados/Ferreira et al. 2021 - EMAS/Analises/modeling/outputs/out_servs/serv70_85/'
out.beta.delta <- '~/Documents/Artigos publicados/Ferreira et al. 2021 - EMAS/Analises/modeling/outputs/out_beta_delta/'

thrs <- c(thr.f50.45, thr.f70.45, thr.f50.85, thr.f70.85) # caminhos para for
outs <- c(out.serv50.45, out.serv70.45, out.serv50.85, out.serv70.85)


# Nomes dos servicos
setwd('~/Documents/Artigos publicados/Ferreira et al. 2021 - EMAS/Analises/modeling/')
servicos <- read.delim('servicos.txt', h=T, na.strings = c("", "NA"))
(serv_names <- colnames(servicos))

# Carregar nomes das spp corretas com sep = '_'
setwd(species)
spp.names <- read.csv('all_spp_final.csv', h=T)
(spp.names <- as.character(unique(spp.names$Species))) #nomes das spp separados por espa?o
(spp_names <- gsub(" ", "_", spp.names)) #nomes das spp separados por underline

# Carregar shape cerrado para corte da div beta
setwd(shape)
cerrado <- readOGR('biomas_IBGE', 'Cerrado')

cenario <- c('_rcp45 2050', '_rcp45 2070', '_rcp85 2050', '_rcp85 2070') #cria objeto cenario
cenarios <- c('RCP4.5 2050', 'RCP4.5 2070', 'RCP8.5 2050', 'RCP8.5 2070') #cria objeto cenarioS


# Carregar lista de todos os thresholds das spp para cada clima (alterar pasta de diretorio p cada clima)
tab.final <- NULL
for(i in 1:length(cenarios)){ #Funcao para alternar pasta de diretorio para salvar
  print(paste(cenarios[i], 'Servicos', sep = ' - '))
  
  setwd(thr.pres)  #Pasta de presente n?o altera dentro do for
  spp.list.p <- stack(paste0(spp.names, '_thresholded','.asc'))
  
  setwd(thrs[i])
  spp.list.f <- stack(paste0(spp.names, '_thresholded','.asc'))
  names(spp.list.p) <- names(spp.list.f) <- spp_names #usar nomes das spp separados por underline para bater com o que dos servicos

  # Funcao para selecionar rasters de cada servicos (somando as grids) - (alterar pasta de diretorio p cada clima)
  # for(j in 1:length(serv_names)){
  #   serv <- as.character(na.omit(servicos[,j]))
  #   if(length(serv) > 1){
  #     riq.spp.p <- sum(spp.list.p[[serv]])
  #     riq.spp.f <- sum(spp.list.f[[serv]])
  #   } else {
  #     riq.spp.p <- spp.list.p[[serv]]
  #     riq.spp.f <- spp.list.f[[serv]]
  #   }
  #   setwd(outs[i])
  #   writeRaster(riq.spp.p, paste0(serv_names[j], '_p.asc'), method = 'ascii', overwrite = T)#salva pres services
  #   writeRaster(riq.spp.f, paste0(serv_names[j], '_f.asc'), method = 'ascii', overwrite = T)#salva fut services
  # } # fecha for j (servicos)

  # print(paste(cenarios[i], 'Delta/Beta', sep = ' - '))
  
  ## Calculo da riqueza e delta de servicos para o Cerrado
  # Presente
  setwd(outs[i])
  servicos.p <- stack(list.files('.', '_p.asc', full.names = T))
  servicos.p.cer <- mask(crop(servicos.p, cerrado), cerrado)
  riq.serv.p.cer <- sum(servicos.p.cer > 0) # numero de servicos por celula
  
  # Futuro
  setwd(outs[i])
  servicos.f <- stack(list.files('.', '_f.asc', full.names = T))
  servicos.f.cer <- mask(crop(servicos.f, cerrado), cerrado)
  riq.serv.f.cer <- sum(servicos.f.cer > 0) # numero de servicos por celula
  
  
  # Plot pranchas dos mapas de servicos (MS2)
  # serv_names <- read.csv('~/Documents/Artigos para publicar/Artigos Rafael/Cap 2 rafael/Analises/modeling/servs_names.csv')
  # par(mfrow=c(6,4), mar = c(1,3,1,3))
  # for (j in 1:nlayers(servicos.p.cer)) {
  #   plot(servicos.p.cer[[j]], main = serv_names[j,])
  # }


  ## Riqueza servi?os presente para Cerrado (Figura 1)
  # par(mfrow=c(1,2))
  # plot(riq.serv.p.cer, main= 'Riqueza Cerrado - Presente'); plot(riq.serv.f.cer, main= 'Riqueza Cerrado - Futuro')

  # setwd('~/Documents/Artigos para publicar/Artigos Rafael/Cap 2 rafael/ARTIGO FINAL/Figuras/Figura 1/')
  # writeRaster(riq.serv.p.cer, 'richness_services.asc', 'ascii', overwrite = T)

  
  ## Calcular delta de servicos do presente pro futuro
  delta.serv <- riq.serv.f.cer - riq.serv.p.cer ## para o cerrado
  delta.serv.vals <- na.omit(values(delta.serv))
  summary(delta.serv.vals)
  table(delta.serv.vals)

  tab.mud <- cbind(Loss = length(which(delta.serv.vals < 0)),
                   Stability =  length(which(delta.serv.vals == 0)),
                   Gain = length(which(delta.serv.vals > 0)))
  
  # Valores de perda, ganho e estabilidade para o delta do cenario com presente
  tab.final <- rbind(tab.final, tab.mud); rownames(tab.final)[i] <- cenarios[i] 

  
  # Histograma de frequencia de perda/ganho de servicos
  # par(mfrow=c(1,1), mar = c(5,5,5,5))
  # hist(delta.serv, xlab = 'Delta', ylab = 'Grid cell frequency', main = paste('Histograma perda/ganho', cenarios[i], sep = '-'),
  #      xlim = c(-20,20), nclass = 20, cex.lab = 1.2); abline(v=0, col = 'red', lty = 2, lwd = 2)


  # Figura 2 - Mapa Cerrado + histograma de perda/ganho de servicos
  pal <- colorRampPalette(c('darkorange', 'orange', 'white', 'orchid', 'darkorchid'))
  # brewer.pal(limit, name = 'PuOr')
  # terrain.colors(limit)
  # heat.colors(limit)
  
  limit = length(table(delta.serv.vals))
  n.breaks = as.integer(dimnames(table(delta.serv.vals))[[1]])
  # n.breaks = seq(-20,17) #extremos entre todos os cen?rios
  
  if(i != 3){a <- i} else {a <- 0}
  if(i == 4){b <- -1} else {b <- 0}
  
  {par(mfrow = c(1,1), mar = c(5,5,5,5))
  library(RcmdrMisc)
  Barplot(as.factor(na.omit(values(delta.serv))), scale = 'percent', col = pal(limit+3+a+b), 
          ylab = 'Percentage of Cerrado area', xlab = 'Delta', label.bars = T,
          cex.axis = 1.8, cex.names=1.8, cex.lab = 1.9, ylim = c(0,35))
          title(cenarios[i], adj = 0, cex.main = 2)
  # barplot(delta.serv, col = pal(limit+3+a+b), xlab = 'Delta', ylab = 'Number of cells',
  #         ylim = c(0, 2500), cex.axis = 1.8, cex.names=1.8, cex.lab = 1.9)
  par(new = T, mar = c(3,1,1,10))
  plot(delta.serv, col = pal(limit+1+a+b), breaks = n.breaks, axes = F, box = F, legend = T)
  plot(cerrado, add = T)}


    # library(ggplot2); library(scales)
    # ggplot(teste2,aes(teste))+geom_bar(aes(y=(..count..)/sum(..count..)))+
    # scale_y_continuous(labels=percent_format())+
    # 
    # scale_fill_brewer(palette = 'Blues')
    
   
  
  
  # Calcular turnover, aninhamento, sorense de servicos entre pres e fut
  pres01 <- na.omit(values(as.integer(servicos.p.cer > 0)))
  fut01 <- na.omit(values(as.integer(servicos.f.cer > 0)))

  serv.vals <- values(servicos.p.cer[[1]]); xy <- xyFromCell(servicos.p.cer, 1:ncell(servicos.p.cer))
  xy.sem.na <- na.omit(cbind(xy, serv.vals))[,1:2]

  a <- b <- c <- a.p <- b.p <- c.p <- min.bc <- max.bc <- sum.bc <- beta.sim <- beta.nes <- beta.sor <- NULL

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
  } #fecha for k (beta)

  beta.sim.r <- rasterFromXYZ(cbind(xy.sem.na, beta.sim))
  beta.nes.r <- rasterFromXYZ(cbind(xy.sem.na, beta.nes))
  beta.sor.r <- rasterFromXYZ(cbind(xy.sem.na, beta.sor))

  # Plotar beta (nes e turn) e delta por grid em todo neotropico
  par(mfrow = c(2,2))
  plot(beta.sim.r, main = paste0('Turnover', cenario[i])); plot(cerrado, add=T)
  plot(beta.nes.r, main = paste0('Nestedness', cenario[i])); plot(cerrado, add=T)
  plot(beta.sor.r, main = paste0('Beta total', cenario[i])); plot(cerrado, add=T)
  plot(delta.serv, main = paste0('Delta', cenario[i])); plot(cerrado, add=T)


  # # # Salvar raster de output para cada cenario
  # setwd(out.beta.delta)
  # writeRaster(beta.sim.r, paste0('turnover', cenario[i], '.tif'), method = 'GeoTiff', overwrite = T)
  # writeRaster(beta.nes.r, paste0('nestedness', cenario[i], '.tif'), method = 'GeoTiff', overwrite = T)
  # writeRaster(beta.sor.r, paste0('beta_total', cenario[i], '.tif'), method = 'GeoTiff', overwrite = T)
  # writeRaster(delta.serv, paste0('delta', cenario[i], '.tif'), method = 'GeoTiff', overwrite = T)
  # 
} # fecha for i (cenario)




