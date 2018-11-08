#Basic ggplot demo from the gulf coast research lab's student workshop training series 2018
#8/23/2018

library(tidyverse)
mtcars

mtcars %>% ggplot(aes(wt, mpg, color = cyl)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = F, color = "black", lty = 2)


#Baseplot for a bar graph in base r
barplot(mtcars$mpg)

#ggplot comparison 
ggplot(mtcars, aes(mpg, fill = as.factor(cyl))) + #ggplot likes to color by factors for barplots
  geom_bar(binwidth = 1, position = 'dodge')



#scatterplot in base r
plot(mtcars$wt, mtcars$mpg, main = "Scatterplot Example",
     xlab = "Car Weight", ylab = "Miles per gallon",
     pch = 19,
     col = mtcars$cyl)


#ggplot version
mtcars %>%  ggplot(aes(wt, mpg, col = as.factor(cyl))) +
  geom_point()



#R color brewer package
install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all(type = "all", exact.n = T)


#ggmap
install.packages("ggmap")
library(ggmap)
GCRL <- make_bbox(lat = c(30.2,30.5), lon = c(-88.5,-88.85))
GCRL

a <- get_map(location = GCRL, maptype = 'roadmap', source = "google")
ggmap(a)


#say you didn't want google to change your bbox, other sources don't force that shape
a <- get_map(location = GCRL, maptype = 'watercolor', source = "stamen")
ggmap(a)
