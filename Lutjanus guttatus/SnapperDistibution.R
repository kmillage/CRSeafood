# Mapping snapper distribution from IUCN

# Loading packages
library(raster) 
library(rgdal) 
library(rasterVis) 
library(maps) 
library(rgeos)
library(dplyr)
library(RColorBrewer) 

# Loading in snapper shapefile
snapper = readOGR(dsn='Lutjanus guttatus',layer='species_183777')
plot(snapper)
map('world',fill=T,add=T,col='gray')