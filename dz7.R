#Ciscenje radne povrsine
rm(list = ls())
library(sp)
library(spdep)
library(raster)
library(stringr)
options(encoding = "UTF-8")

#Ucitavanje podataka iz Covid19CountyRec.csv datoteke
setwd("~/Desktop/UZnL")
dz7data <- read.csv('dz7data.csv', header=TRUE)

#Dohvati prostorne podatke zupanija pomocu GADM-a
zupanije <- raster::getData('GADM', country='HRV', level=1)

zupanije@data <- merge(zupanije@data, dz7data, by="NAME_1", all.x=TRUE)

nb <- poly2nb(zupanije, queen=FALSE)
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

I <- moran(zupanije$BDP, lw, length(nb), Szero(lw))[1]
print(I)

MC<- moran.mc(zupanije$BDP, lw, nsim=999, alternative="greater")
print(MC)