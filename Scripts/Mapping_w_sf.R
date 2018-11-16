############################
# Template map for Gulf of Mecico

#required packages
library(raster)
library(sf)
library(tidyverse)

############  Spatial Features  ############

# 1. Contiguous US
usa_adam <- read_sf("Y:/GIS Files/GIS shapefiles/Gulf of Mexico/gom_states.shp")           
st_crs(usa_adam) <- 4326

# 2. Mexico and latin america
mex_adam <- read_sf("Y:/GIS Files/GIS shapefiles/Gulf of Mexico/coast_1km_FLto_MX.shp")    
st_crs(mex_adam) <- 4326

#crop mexico so coastlines are cleaner
box <- c(xmin = -98, xmax = -96, ymin = 23, ymax = 26.5)
mex_adam <- st_crop(mex_adam, box)


# 3. Bathymetric contours
bathyp <- read_sf("Y:/GIS Files/GIS shapefiles/Gulf of Mexico/bathy_p.shp")
bathyp <- bathyp %>% dplyr::filter(DEPTH != 15)
box <- c(xmin = -99, xmax = -78, ymin = 22, ymax = 33)
bathyp <- st_crop(bathyp, box)

#make colours for contours, and change labels to shorten them
bathyp <- bathyp %>% mutate(RANGE = str_remove(RANGE, pattern = "res in depth"),
                            RANGE = str_c(RANGE, "ers", sep = ""),
                            RANGE = factor(RANGE, 
                                           levels = c("0-200 meters",
                                                      "201-500 meters",
                                                      "501-2500 meters",
                                                      ">2500 meters")),
                            cont_col = ifelse(DEPTH == 1, "skyblue2", "skyblue4"),
                            cont_col = ifelse(DEPTH == 2, "skyblue1", cont_col),
                            cont_col = ifelse(DEPTH == 3, "skyblue3", cont_col))


# 4. Lake Pontchartrain fill in
pontchbox <- st_as_sf(as(raster::extent(-95, -89.60, 29, 30.4), "SpatialPolygons"))
st_crs(pontchbox) <- 4326





#############  Map Template 1 - Bathymetery  ################
ggplot() + 
  geom_sf(data = bathyp, aes(fill = RANGE)) +
  geom_sf(data = pontchbox, fill = "skyblue1", color = NA) +
  geom_sf(data = mex_adam, fill = "antiquewhite1") +                                                   
  geom_sf(data = usa_adam, fill = "antiquewhite1") +
  coord_sf(xlim = c(-97.20, -81), ylim = c(24.35, 31.5)) +
  xlab("") + ylab("") +
  #ggtitle("Map Template for SEAMAP plots") +
  scale_fill_manual("", values = c(bathyp$cont_col)) +
  theme(panel.grid.major = element_line(colour = gray(0.85), 
                                        linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = NA),                                                
        panel.border = element_rect(fill = NA),
        legend.position = "bottom")





############  Statistical Zones  ##########
#2013 stat zones
zones2 <- read_sf("Y:/GIS Files/GIS shapefiles/Gulf of Mexico/NMFS_Fishing_Grids_GOM_2013.shp")
zones2 <- cbind(zones2, st_coordinates(st_centroid(zones2)))
zones2$ID <- as.character(zones2$StatZone)
zones2$nudge_y <- 0
zones2$nudge_x <- 0
#stat_zones$nudge_y[stat_zones$ID == "13"] <- -0.1  #Do this to nudge




################  Template 2 - Statistical Zones  ##################
ggplot() + 
  geom_sf(data = bathyp, aes(fill = RANGE)) +
  geom_sf(data = pontchbox, fill = "skyblue1", color = NA) +
  geom_sf(data = mex_adam, fill = "antiquewhite1") +                                                   
  geom_sf(data = usa_adam, fill = "antiquewhite1") +
  geom_sf(data = zones2, aes(), fill = NA) +
  geom_text(data = zones2, aes(X, Y, label = ID), size = 3, fontface = "bold", 
            nudge_y = zones2$nudge_y,
            nudge_x = zones2$nudge_x) +
  coord_sf(xlim = c(-97, -81), ylim = c(24.35, 31.5)) +
  scale_fill_manual("", values = c(bathyp$cont_col)) +
  xlab("") + ylab("") +
  theme(panel.grid.major = element_line(colour = gray(0.85), 
                                        linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = NA),                             #aliceblue is good for color                   
        panel.border = element_rect(fill = NA),
        legend.position = "bottom")
