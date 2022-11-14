#brisanje prethodnih varijabli u okruzenju
rm(list = ls())

#pregled koji je radni direktorij i priprema zeljenog radnog direktorija
getwd()
setwd("~/Desktop/UznL")

#crtanje mape Hrvatske s oznacenim najdražim mjesima
library(rworldmap)
cromap <- getMap(resolution = "low")
plot(cromap, xlim = c(17.5, 18), ylim = c(42, 47), asp = 1)
favplaces <- data.frame(c(15.225,13.8836,14.1197), c(44.3375,45.0881,45.095))
colnames(favplaces) <- c('lon', 'lat')
points(favplaces$lon, favplaces$lat, col='black', pch=16)