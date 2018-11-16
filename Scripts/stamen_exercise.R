library(tidyverse)
library(ggmap)

#Set Working Directory to where data is stored
setwd("D:/ReefFish/Red_Snapper")
mydata <- read_csv("D:/ReefFish/Red_Snapper/index.Lutjanus_campechanus.csv")

mydata <- mydata %>% rename(cpue_dat = LUTJANUS_CAMPECHANUS) %>% 
  mutate(success = ifelse(cpue_dat > 0, 1, 0),                      
         lgcpue = ifelse(cpue_dat > 0, log(cpue_dat), NA))

#Using Stamen maps  
height <- max(mydata$sta_lat) - min(mydata$sta_lat)
width <- max(mydata$sta_lon) - min(mydata$sta_lon)
sac_borders <- c(bottom  = min(mydata$sta_lat)  - 0.1 * height, 
                 top     = max(mydata$sta_lat)  + 0.1 * height,
                 left    = min(mydata$sta_lon) - 0.1 * width,
                 right   = max(mydata$sta_lon) + 0.1 * width)

map <- get_stamenmap(sac_borders, zoom = 6, maptype = "toner-lite")

#did not work well with the data points
ggmap(map) +
  geom_point(data = mydata, mapping = aes(x = sta_lon, y = sta_lat)) +
  scale_color_distiller(palette = "YlOrRd", direction = 1)
