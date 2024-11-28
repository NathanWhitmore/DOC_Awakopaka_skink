library(sf)
library(tidyverse)
library(ggspatial)


tt <- read_sf("Tracking tunnels.shp")%>% 
  st_transform(crs = 2193)
bait <- read_sf("all_baitstations.shp") %>% 
  st_transform(crs = 2193)

# make buffer for area of interest
aoi <- bait  %>% st_buffer(10) %>%
  st_union()
  
# map
ggplot()+
  theme_bw()+
  geom_sf(data = tt, aes(colour = "tracking tunnels"), size = 5)+
  geom_sf(data = bait , aes(colour = "bait stations"))+
  # geom_sf(data = aoi, colour = "red", fill = NA, lwd = 2)+
  annotation_scale(location = "tr")+
  labs(colour = "Device")+
  scale_colour_manual(values = c("grey60", "purple"))+
  ggtitle("You need to define your area of interest")

ggsave("Current configuration.png", scale = 1.2, height = 6, width = 9)

# change bait to long lat
bait.ll <- bait %>% st_transform(crs = 4326)

st_write(bait.ll, "Awakopaka.kml")

# read approx file

awa <- st_read("awakopaka_approx.kml") %>%
  st_transform(crs = 2193)


awa.b <- awa %>% st_buffer(10)

ggplot()+
  geom_sf(data = awa)+
  geom_sf(data = awa.b)

# make a mesh (grid) 
my.grid <- st_make_grid(awa.b,
                        cellsize = c(20, 20))

# make as sf object
my.grid <- my.grid %>% st_as_sf() 

# intersection with aoi
my.grid.2193 <- my.grid %>% st_intersection(awa.b) 

# assess area of each unit 
my.grid.2193$area <- my.grid.2193 %>% st_area() %>% as.numeric()

hist(my.grid.2193$area)

# keep only units > 23 sq m
my.grid.2193 <- my.grid.2193 %>% filter(area >=400)

ggplot()+
  geom_sf(data = my.grid.2193, fill = NA, colour = "red")+
  geom_sf(data = awa, fill = NA, colour = "purple")+
  geom_sf(data = awa.b, fill = NA)
  

  