rm(list = ls())
setwd("E:\\")

library(leaflet)
dz5data <- read.csv('dz5.csv', header = TRUE)
dz5data <- data.frame(dz5data)
dz5data.pos <- dz5data[,c(4:6)]
dz5data.pos <- na.omit(dz5data.pos)
colnames(dz5data.pos) <- c('latitude','longitude','height')
print(summary(dz5data.pos))
plot(dz5data.pos$longitude, dz5data.pos$latitude)
lonlat <- cbind(dz5data.pos$longitude, dz5data.pos$latitude)
pts <- SpatialPoints(lonlat)
crdref <- CRS('+proj=longlat +datum=WGS84')
pts <- SpatialPoints(lonlat, proj4string=crdref)
lns <- spLines(lonlat, crs=crdref)
# Print a leaflet object on the Open Street Map background
print(leaflet() %>%
        addTiles() %>% # add basemap to your map
        addPolylines(data = lns, color = "blue", fillColor = "green", 
                     weight = 1, smoothFactor = 0.5,
                     opacity = 1.0))