library(sf)
library(tidyverse)
library(ggspatial)


tt <- read_sf("Tracking tunnels.shp")%>% 
  st_transform(crs = 2193)
bait <- read_sf("all_baitstations.shp") %>% 
  st_transform(crs = 2193)

aoi <- st_read("awakopaka polygon.kml")  %>%
  st_transform(crs = 2193)

# map
ggplot()+
  theme_bw()+
  geom_sf(data = tt, aes(colour = "tracking tunnels"), size = 5)+
  geom_sf(data = bait , aes(colour = "bait stations"))+
  geom_sf(data = aoi, colour = "red", fill = NA, lwd = 2)+
  geom_sf(data = aoi.b, colour = "orange", fill = NA, lwd = 2)+
  annotation_scale(location = "tr")+
  labs(colour = "Device")+
  scale_colour_manual(values = c("grey60", "purple"))+
  ggtitle("You need to define your area of interest")

ggsave("Current configuration.png", scale = 1.2, height = 6, width = 9)


# make buffer for area of interest
aoi.b <- aoi  %>% st_buffer(7) %>%
  st_union() 


# make a mesh (grid) 
my.grid <- st_make_grid(aoi.b,
                        cellsize = c(10, 10))

# make as sf object
my.grid <- my.grid %>% st_as_sf() 

# intersection with aoi
my.grid.2193 <- my.grid %>% st_intersection(aoi.b) 

# assess area of each unit 
my.grid.2193$area <- my.grid.2193 %>% st_area() %>% as.numeric()

hist(my.grid.2193$area)

# keep only units > 100 sq m
my.grid.2193 <- my.grid.2193 %>% filter(area >=100)

# label cells
my.grid.2193$my.label <- 1: nrow(my.grid.2193)

ggplot()+
  theme_bw()+
  geom_sf_text(data = my.grid.2193, aes(label = my.label))+
  geom_sf(data = my.grid.2193, colour = "black", 
          fill = "grey", alpha = 0.7)+
  geom_sf(data = aoi, fill = NA, colour = "red",
          alpha = 0.5) +
  ggtitle("10 x 10 m grid layout")+

  annotation_scale(location = "bl")+
  labs(x = "", y ="")
  

  