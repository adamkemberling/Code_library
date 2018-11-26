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
  coord_sf(xlim = c(-97.15, -80.75), ylim = c(24.35, 31.5)) +
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
  geom_text(data = zones2, aes(X, Y, label = ID), 
            size = 3, 
            fontface = "bold", 
            nudge_y = zones2$nudge_y,
            nudge_x = zones2$nudge_x) +
  coord_sf(xlim = c(-97.15, -80.75), 
           ylim = c(24.35, 31.5)) +
  scale_fill_manual("", values = c(bathyp$cont_col)) +
  xlab("") + ylab("") +
  theme(panel.grid.major = element_line(colour = gray(0.85), 
                                        linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = NA),                                                
        panel.border = element_rect(fill = NA),
        legend.position = "bottom")










################  Template 3 - Species Distribution maps  #############
rsnap <- read_csv("D:/ReefFish/Red_Snapper/index.Lutjanus_campechanus.csv", guess_max = 10000, col_types = cols())
rsnap <- rsnap %>% rename(mincount = LUTJANUS_CAMPECHANUS)

snap_sf <- st_as_sf(rsnap, 
                  coords = c("sta_lon", "sta_lat"), 
                  crs = 4326, 
                  agr = "constant")

snap_sf <- snap_sf %>% filter(mincount > 0)


#Bubble plots
ggplot() + 
  geom_sf(data = bathyp, aes(fill = RANGE)) +
  geom_sf(data = pontchbox, fill = "skyblue1", color = NA) +
  geom_sf(data = mex_adam, fill = "antiquewhite1") +                                                   
  geom_sf(data = usa_adam, fill = "antiquewhite1") +
  geom_sf(data = snap_sf, 
          size = sqrt(snap_sf$mincount),
          alpha = 0.75) +
  coord_sf(xlim = c(-97.15, -80.75), 
           ylim = c(24.35, 31.5)) +
  scale_fill_manual("", values = c(bathyp$cont_col)) +
  xlab("") + ylab("") +
  theme(panel.grid.major = element_line(colour = gray(0.85), 
                                        linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = NA),                                                
        panel.border = element_rect(fill = NA),
        legend.position = "bottom")





#Sampling Effort Map - Number of Stations
ggplot() + 
  geom_sf(data = mex_adam, fill = "antiquewhite1") +                                                   
  geom_sf(data = usa_adam, fill = "antiquewhite1") +
  stat_binhex(aes(x = sta_lon, y = sta_lat,
                  fill = cut(..count.., c(0,25,50,100,200,500,1000, Inf))),
              colour = gray(.6),
              bins = 30, 
              alpha = 0.75,
              data = rsnap) +
  scale_fill_brewer("",
                    palette = "OrRd",
                    labels = c("<25", "25-50", "50-100", 
                               "100-200","200-500", "500-1000", ">1000")) +
  coord_sf(xlim = c(-97.15, -80.75), 
           ylim = c(24.35, 31.5)) +
  xlab("") + ylab("") + ggtitle("Number of Stations") +
  theme(panel.grid.major = element_line(colour = gray(0.85), 
                                        linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),                                                
        panel.border = element_rect(fill = NA),
        legend.position = "bottom")





#Average CPUE/mincount map - Hex or Rectangle
ggplot() + 
  geom_sf(data = mex_adam, fill = "antiquewhite1") +                                                   
  geom_sf(data = usa_adam, fill = "antiquewhite1") + 
  stat_summary_hex(aes(x = sta_lon, y = sta_lat, z = mincount),
                   fun = mean,
                   colour = gray(.6),
                   bins = 30,
                   alpha = 0.75,
                   data = rsnap) +
  # stat_summary_2d(aes(x = sta_lon, y = sta_lat, z = mincount),
  #                  fun = mean,
  #                  colour = gray(.6),
  #                  bins = 40, 
  #                  alpha = 0.75,
  #                  data = rsnap) +
  coord_sf(xlim = c(-97.15, -80.75), 
           ylim = c(24.35, 31.5)) +
  xlab("") + ylab("") + ggtitle("Average Mincount/CPUE") + 
  scale_fill_gradient("",
                    low = "white", high = "darkred") +
  theme(panel.grid.major = element_line(colour = gray(0.85), 
                                        linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),                                                
        panel.border = element_rect(fill = NA),
        legend.position = "bottom")

#Issue 2, if you want the depth contours as well, they need to be added as a second grob






#Choropleths using stat zone
rsnap %>% group_by(seamapstratum) %>% summarise()

