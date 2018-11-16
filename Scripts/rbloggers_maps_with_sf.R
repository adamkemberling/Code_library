# Tutorial from R-bloggers
# "Drawing beautiful maps programmatically with R, sf and ggplot2 — Part 2: Layers"
# October 24, 2018
# By Mel Moreno and Mathieu Basille

install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rworldmap", "rworldxtra"))


library("ggplot2")
theme_set(theme_bw())
library("sf")


'
The package rworldmap provides a map of countries of the entire world;
a map with higher resolution is available in the package rworldxtra.
We use the function getMap to extract the world map (the resolution
can be set to "low", if preferred):
'

library("rworldmap")
library("rworldxtra")
world <- getMap(resolution = "high")
class(world)


'
The world map is available as a SpatialPolygonsDataFrame from the
package sp; we thus convert it to a simple feature using st_as_sf
from package sf:
'

world <- st_as_sf(world)
class(world)

'
Adding additional layers: an example with points and polygons
Field sites (point data)

We start by defining two study sites, according to their longitude and
latitude, stored in a regular data.frame:
'


(sites <- data.frame(longitude = c(-80.144005, -80.109), 
                    latitude = c(26.479005, 26.83)))
'
The quickest way to add point coordinates is with the general-purpose
function geom_point, which works on any X/Y coordinates, of regular
data points (i.e. not geographic). As such, we can adjust all
characteristics of points (e.g. color of the outline and the filling,
shape, size, etc.), for all points, or using grouping from the data (i.e
defining their “aesthetics”). In this example, we add the two points as
diamonds (shape = 23), filled in dark red (fill = "darkred") and of
bigger size (size = 4):
'

#Map with points - world resolution
ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)



'
A better, more flexible alternative is to use the power of sf:
Converting the data frame to a sf object allows to rely on sf to
handle on the fly the coordinate system (both projection and extent),
which can be very useful if the two objects (here world map, and sites)
are not in the same projection. To achieve the same result, the
projection (here WGS84, which is the CRS code #4326) has to be a priori
defined in the sf object:
'


(sites <- st_as_sf(sites, 
                  coords = c("longitude", "latitude"), 
                  crs = 4326, agr = "constant"))


#plot the locaitons now with geom_sf not geom_point
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = sites, size = 4, shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)
  #Note that coord_sf has to be called after all geom_sf calls, as to
  #supersede any former input.


'
States (polygon data)

It would be informative to add finer administrative information on top
of the previous map, starting with state borders and names. The package
maps (which is automatically installed and loaded with ggplot2)
provides maps of the USA, with state and county borders, that can be
retrieved and converted as sf objects:
'

library("maps")
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)


'
State names are part of this data, as the ID variable. A simple (but
not necessarily optimal) way to add state name is to compute the
centroid of each state polygon as the coordinates where to draw their
names. Centroids are computed with the function st_centroid, their
coordinates extracted with st_coordinates, both from the package sf,
and attached to the state object:
'


states <- cbind(states, st_coordinates(st_centroid(states)))

'
Note the warning, which basically says that centroid coordinates using
longitude/latitude data (i.e. WGS84) are not exact, which is perfectly
fine for our drawing purposes. State names, which are not capitalized in
the data from maps, can be changed to title case using the function
toTitleCase from the package tools:
'

library("tools")
states$ID <- toTitleCase(states$ID)
head(states)


'
To continue adding to the map, state data is directly plotted as an
additional sf layer using geom_sf. In addition, state names will be
added using geom_text, declaring coordinates on the X-axis and Y-axis,
as well as the label (from ID), and a relatively big font size.
'

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_text(data = states, aes(X, Y, label = ID), size = 5) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)


'
We can move the state names slightly to be able to read better “South
Carolina” and “Florida”. For this, we create a new variable nudge_y,
which is -1 for all states (moved slightly South), 0.5 for Florida
(moved slightly North), and -1.5 for South Carolina (moved further
South):
'

states$nudge_y <- -1
states$nudge_y[states$ID == "Florida"] <- 0.5
states$nudge_y[states$ID == "South Carolina"] <- -1.5

'
To improve readability, we also draw a rectangle behind the state name,
using the function geom_label instead of geom_text, and plot the map
again.
'

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_label(data = states, aes(X, Y, label = ID), size = 5, fontface = "bold", 
             nudge_y = states$nudge_y) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)


'
Counties (polygon data)

County data are also available from the package maps, and can be
retrieved with the same approach as for state data. This time, only
counties from Florida are retained, and we compute their area using
st_area from the package sf:
'

