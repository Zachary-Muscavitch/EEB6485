rm(list=ls())

library(tidyverse)
library(ggrepel)
library(ggplot2)
library(memisc)
library(assertthat)
library(sqldf)
library(magrittr)
library(reshape2)
library(oz)
library(scatterpie)
library(rgdal)
library(maptools)
library(sf)
library(rnaturalearth)
library(rgeos)



############### Formulas

pivot_by_site <- function(data) {
  
  s1 = melt(data, id = c("locality", "Alga"), measure.vars = "V1")
  s2 = dcast(s1, locality ~ Alga, sum)
  
  s2$Total = rowSums(s2[,2:NCOL(s2)])
  return(s2)
}

##### read in data and manipilate it into single site and count data
data <- read.csv("Werth_et_al_2016_AJB.csv")

datacount <- ddply(data,.(Alga,locality,Lat,Long),nrow)

pivotted.data <- pivot_by_site(datacount)

##### Site locality info

data.localities <- select(data, - c("X","Alga"))

unique.data.localities <- unique( data.localities[ , 1:3 ] )

###### Merging photoboint occurance and location
final_data <- merge (x= pivotted.data, y = unique.data.localities)

final_data

############################################################# Mapping


######## alternative map

worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
worldmap

north.america <- worldmap[worldmap$continent == 'North America',]

ggplot() + geom_sf(data = north.america) + theme_bw()

na_cropped <- st_crop(north.america, xmin = -140, xmax = -110,
                      ymin = 20, ymax = 60)

ggplot() + geom_sf(data = na_cropped) + theme_bw()

mapplot1 <- ggplot() + geom_sf(data = na_cropped) + theme_bw() + 
  geom_scatterpie(aes(x=Long, y=Lat, group = locality, r = 0.5), 
                  data = final_data, cols = colnames(final_data[,c(2:15)]))

mapplot1

####Need to figure out how to prevent the points from overtopping - repel? 


###### Crappy way to do this####

# Using map_data()

# Using map_data()
#worldmap <- map_data ("world")

#na <- map_data("world",region = c("Canada","usa","mexico"))

#mapplot1 <- ggplot(worldmap) + 
#  geom_map(data = na.only, map = na.only, 
#           aes(x=long, y=lat, map_id=region), col = "white", fill = "gray50") +
#  coord_map(xlim = c(-140, -115),ylim = c(25, 60))

#mapplot1

#mapplot1 <- ggplot(worldmap) + 
#  geom_map(data = na.only, map = na, aes(x=long, y=lat, map_id=region), col = "white", fill = "gray50") +
#  coord_map(xlim = c(-140, -115),ylim = c(25, 60)) +
#  geom_scatterpie(aes(x=Long, y=Lat, group = locality, r = 0.5), 
                 # data = final_data, cols = colnames(final_data[,c(2:15)]))

#mapplot1


#coord_map(xlim = c(-140, -115),ylim = c(25, 60) 
            
