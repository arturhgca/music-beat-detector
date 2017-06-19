library(dplyr)
library(reshape2)
library(data.table)
library(matrixStats)

Mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux)); ux[tab == max(tab)]
}

setwd("%appdata%\\LOVE\\unnamed\ music\ game")

bands <- 4
averaging_window <- 5
threshold_factor <- 1.5

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
spectra <- NULL

#d <- .mp3 file
#outfile <- .beats file

bandwidth <- dim(d)[2]/bands
for(i in 1:bands) {
  spectra[[i]] <- apply(d[,((i-1)*bandwidth+1):(i*bandwidth)], 1, function(x) max(x))
}
selected_frequencies <- as.matrix(unname(as.data.frame(spectra)))
aux_selected_frequencies = as.matrix(selected_frequencies[-1,])
spectral_flux = aux_selected_frequencies - selected_frequencies[1:nrow(selected_frequencies)-1,]
spectral_flux <- apply(as.data.frame(spectral_flux), 2, function(x) {ifelse(x < 0, 0, x)})
spectral_flux <- data.frame(lapply(as.data.frame(spectral_flux), function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE)/1)))
for(i in 1:nrow(spectral_flux)) {
  start = max(0,i-averaging_window)
  end = min(i+averaging_window,nrow(spectral_flux))
  #spectral_flux_threshold <- rbind(spectral_flux_threshold, colMeans(as.data.frame(spectral_flux[start:end,]))*threshold_factor)
  spectral_flux_threshold <- rbind(spectral_flux_threshold, apply(as.data.frame(spectral_flux[start:end,]), 2, max)*0.99)
}

plot(spectral_flux[,1], type="n")
lines(spectral_flux[,1])
lines(spectral_flux_threshold[,1], col=2)

pruned_spectral_flux = as.data.table(spectral_flux)
spectral_flux_threshold = as.data.table(spectral_flux_threshold)
mask <- (pruned_spectral_flux <= spectral_flux_threshold)
pruned_spectral_flux <- pruned_spectral_flux - spectral_flux_threshold
pruned_spectral_flux[mask] <- NA
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
#write.table(unname(pruned_spectral_flux), outfile, sep=" ", row.names=F)

all_col_intervals <- NULL
for(i in 1:ncol(pruned_spectral_flux)) {
  col_intervals <- NULL
  col_count <- 0
  for(j in 1:nrow(pruned_spectral_flux)) {
    if(is.na(pruned_spectral_flux[j,i])) {
      col_count <- col_count + 1
    }
    else if(col_count != 1) {
      col_intervals <- c(col_intervals,col_count)
      col_count <- 1
    }
  }
  print(mean(col_intervals))
  all_col_intervals <- c(all_col_intervals,col_intervals)
}
print(mean(all_col_intervals))

all_col_intervals[1:323]
sort(all_col_intervals[1:323])
summary(sort(all_col_intervals[1:323]))
