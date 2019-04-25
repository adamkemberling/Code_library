
# Gulf Coast R Users
# Leaflet Package
# 



#install.packages("leaflet")
library(here)
library(raster)
library(leaflet)
library(maps)
library(sf)
library(tidyverse)
library(htmlwidgets)
library(readxl)
library(rgdal)







sites <- data.frame(area = c('Nodaway Valley Conservation Area',
                             'Loess Bluff National Wildlife Refuge',
                             "Fountain Grove Conservation Area",
                             "Swan Lake National Wildlife Refuge",
                             "Ted Shanks Conservatino Area",
                             "Clarence Cannon National Wildlife Refuge",
                             "B.K. Leach Conservation Area",
                             "Duck Creek Conservation Area",
                             "Otter Slough Conservation Area",
                             "Ten Mile Pond Conservation Area"),
                    lat = c(40.083979,40.086189,39.700193,39.590943,
                            39.520186,39.258942,39.194173, 37.069277, 
                            36.663006, 36.720828),
                    long = c(-95.243862,-95.046928,-93.358501,-93.195568,
                             -91.115087,-90.756951, -90.768015,-90.112259,
                             -90.138140,-89.320857),
                    type = c("state","federal","state","federal",
                             "state","federal","state","state",
                             "state","state"))

statesites <- sites %>% filter(type=="state")
federalsites <- sites %>% filter(type=="federal")

# https://fontawesome.com/icons/pagelines?style=brands
property_icons <- awesomeIconList(
  
  state  = makeAwesomeIcon(text="S", 
                           library = "fa",
                           markerColor = "green"),
  
  federal = makeAwesomeIcon(text = "F", 
                            library="fa",
                            markerColor = "lightgray")
)



html_legend <- "<i>S</i>State<br/>
<i>F</i>Federal"


m <- leaflet() %>%
  # Set starting location and zoom
  setView(lng = -92.656909, lat = 38.530138, zoom = 06) %>%
  #Start loading layers
  addTiles() %>%
  # Add markers for Federal Sites
  addAwesomeMarkers(
    data=federalsites,
    group = "federal",
    icon = ~property_icons[type],
    label = sites$area) %>%
  # Add markers for State Sites
  addAwesomeMarkers(
    data=statesites,
    group = "state",
    icon = ~property_icons[type],
    label = sites$area) %>%
  # Add Circles around federal points
  addCircles(
    data=federalsites,
    group = "federal",
    radius=5000,
    color="blue"
  ) %>%
  # Add Circles around state sites
  addCircles(
    data=statesites,
    group = "state",
    radius=5000,
    color="green"
  ) %>%
  # Layer control options
  addLayersControl(overlayGroups = c("state","federal"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addControl(html = html_legend, position = "topright") %>%
  addScaleBar()




saveWidget(m, file="missouri-field-sites.html")
