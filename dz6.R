rm(list = ls())

library(raster)
UK_Adm_1 <- raster::getData("GADM", country="GBR", level=1)
k <- subset(UK_Adm_1, NAME_1=="England")
raster::plot(UK_Adm_1)
raster::plot(k)
tailored_layers <- raster::getData('alt', country='GBR', mask=TRUE)
plot(tailored_layers, main='England',ylab='latitude [deg]', xlab ='longitude [deg]')
USA_Adm_1 <- raster::getData("GADM", country="USA", level=1)
k <- subset(USA_Adm_1, NAME_1=="Massachusetts")
raster::plot(USA_Adm_1)
raster::plot(k)
tailored_layers <- raster::getData('alt', country='USA', mask=TRUE)
#plot(tailored_layers, main='Massachusetts',ylab='latitude [deg]', xlab ='longitude [deg]')
#vraća grešku: Error in xy.coords(x, y, xlabel, ylabel, log) : 'x' is a list, but does not have components 'x' and 'y'