library(dplyr)
library(reshape2)
library(data.table)
library(matrixStats)

setwd("C:\\Users\\Artur\\AppData\\Roaming\\LOVE\\unnamed\ music\ game")

d <- NULL
outfile <- NULL
d_cor <- NULL
d_cor_melt <- NULL
d_cor_filtered <- NULL
matrix_filtered <- NULL
c2 <- NULL
c3 <- NULL
grey <- NULL
black <- NULL
selected_frequencies <- NULL
aux_selected_frequencies <- NULL
spectral_flux <- NULL
spectral_flux_threshold <- NULL
pruned_spectral_flux <- NULL
mask <- NULL

d <- aloha.mp3
outfile <- "aloha.beats"

#d <- `imagematerial.mp3`
#outfile <- "imagematerial.beats"
#d <- `Masayoshi.Minoshima...Bad.Apple!!.feat..nomico...Touhou.PV.[iichan].mp3`
#outfile <- "apple.beats"
#d <- opm.mp3
#outfile <- "opm.beats"
#d <- Brain.Power.mp3
#outfile <- "brain.beats"
#d <- Crack.Traxxxx.mp3
#outfile <- "crack.beats"
#d <- `IOSYS...Cirno's.Perfect.Math.Class.mp3`
#outfile <- "math.beats"
#d <- `9d430cfd1e0510.mp3`
#outfile <- "church.beats"
#d <- pensamento_t_pico_de_esquerda_caviar.mp3
#outfile <- "caviar.beats"
#d <- Reol.Danshi..mp3
#outfile <- "reol_danshi.beats"
# <- kawaii.mp3
#outfile <- "ageage.beats"

#d <- data.frame(lapply(d, function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE)/1)))
#names(d) <- {as.numeric(gsub("V","", names(d)))}

# d_cor <- as.matrix(cor(d))
# d_cor_melt <- arrange(melt(d_cor), -abs(value))
# #d_cor_melt <- data.frame(lapply(d_cor_melt, function(x) {as.numeric(gsub("V","",x))}))
# 
# d_cor_filtered <- dplyr::filter(d_cor_melt, value > .6)
# matrix_filtered = as.matrix(d_cor_filtered)[,1:2]
# 
# c2 = matrix_filtered[matrix_filtered[,1] != matrix_filtered[,2],-3]
# #c2 = matrix_filtered
# c3 = data.frame(t(apply(c2, 1, function(x) sort(x))))
# c3 = c3[!duplicated(c3), ]
# c3 = c3[order(c3$X1),]
# #c3 = c3[order(std_d, decreasing = TRUE),]
# 
# #c3 %>% group_by(X1) %>% summarise(n=n()) %>% ungroup() %>% arrange(n)
# 
# c3 = as.matrix(c3)
# 
# grey <- NULL
# black <- NULL
# 
# for(i in 1:nrow(c3)) {
#   if(!(c3[i,1] %in% grey | c3[i,2] %in% grey)) {
#     black <- c(black, c3[i,1])
#   }
#   grey <- c(grey, c3[i,1])
#   grey <- c(grey, c3[i,2])
# 
# }
# 
# black = sort(black[complete.cases(black)])
# black
# 
# 
# selected_frequencies = as.data.frame(d[,black])

bands = 4

bandwidth <- dim(d)[2]/bands


spectra <- NULL
for(i in 1:bands) {
  spectra[[i]] <- apply(d[,((i-1)*bandwidth+1):(i*bandwidth)], 1, function(x) max(x))
}

selected_frequencies <- as.matrix(unname(as.data.frame(spectra)))

aux_selected_frequencies = as.matrix(selected_frequencies[-1,])
spectral_flux = aux_selected_frequencies - selected_frequencies[1:nrow(selected_frequencies)-1,]
spectral_flux <- apply(as.data.frame(spectral_flux), 2, function(x) {ifelse(x < 0, 0, x)})

spectral_flux <- data.frame(lapply(as.data.frame(spectral_flux), function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE)/1)))

summary(spectral_flux)

spectral_flux_threshold <- NULL
for(i in 1:nrow(spectral_flux)) {
  start = max(0,i-5)
  end = min(i+5,nrow(spectral_flux))
  #spectral_flux_threshold <- rbind(spectral_flux_threshold, colMeans(as.data.frame(spectral_flux[start:end,]))*1.5)
  spectral_flux_threshold <- rbind(spectral_flux_threshold, apply(as.data.frame(spectral_flux[start:end,]), 2, max)*0.99)
}

plot(spectral_flux[,1], type="n")
lines(spectral_flux[,1])
lines(spectral_flux_threshold[,1], col=2)

pruned_spectral_flux = as.data.table(spectral_flux)
spectral_flux_threshold = as.data.table(spectral_flux_threshold)
mask <- (pruned_spectral_flux <= spectral_flux_threshold)
# mask <- (abs(pruned_spectral_flux) <= 100)
pruned_spectral_flux <- pruned_spectral_flux - spectral_flux_threshold
pruned_spectral_flux[mask] <- NA
#mask <- (pruned_spectral_flux < 0.1)
#pruned_spectral_flux[mask] <- NA

pruned_spectral_flux <- as.data.frame(pruned_spectral_flux)

# essa coisa aí embaixo é pra manter somente os picos que não são menores que seus sucessores
for(i in 1:ncol(pruned_spectral_flux)) {
  for(j in 1:nrow(pruned_spectral_flux)) {
    if(!is.na(pruned_spectral_flux[j,i])) {
      k <- j+1
      flag = FALSE
      while(!flag && is.na(pruned_spectral_flux[k,i])) {
        k <- k+1
        if(k>nrow(pruned_spectral_flux)) {
          flag = TRUE
        }
      }
      if(!flag && (pruned_spectral_flux[j,i] < pruned_spectral_flux[k,i])) {
        pruned_spectral_flux[j,i] <- NA
      }
    }
  }
}

write.table(unname(pruned_spectral_flux), outfile, sep=" ", row.names=F)
