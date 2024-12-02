library(sf)
library(tidyverse)
library(ggspatial)


tt <- read_sf("Tracking tunnels.shp")%>% 
  st_transform(crs = 2193)
bait <- read_sf("all_baitstations.shp") %>% 
  st_transform(crs = 2193)

aoi <- st_read("awakopaka polygon.kml")  %>%
  st_transform(crs = 2193)


# make buffer for area of interest
aoi.b <- aoi  %>% st_buffer(10) %>%
  st_union() 

# map
# ggplot()+
#  theme_bw()+
#  geom_sf(data = tt, aes(colour = "tracking tunnels"), size = 5)+
#  geom_sf(data = bait , aes(colour = "bait stations"))+
#  geom_sf(data = aoi, colour = "red", fill = NA, lwd = 2)+
#  geom_sf(data = aoi.b, colour = "orange", fill = NA, lwd = 2)+
#  annotation_scale(location = "tr")+
#  labs(colour = "Device")+
#  scale_colour_manual(values = c("grey60", "purple"))+
#  ggtitle("You need to define your area of interest")

#ggsave("Current configuration.png", scale = 1.2, height = 6, width = 9)

# make a mesh (grid) 
my.grid <- st_make_grid(aoi.b,
                        cellsize = c(15, 15))

# make as sf object
my.grid <- my.grid %>% st_as_sf() 

# intersection with aoi
my.grid.2193 <- my.grid %>% st_intersection(aoi.b) 

# assess area of each unit 
my.grid.2193$area <- my.grid.2193 %>% st_area() %>% as.numeric()

hist(my.grid.2193$area)

# keep only units > 100 sq m
my.grid.2193 <- my.grid.2193 %>% filter(area >=225)

# label cells
my.grid.2193$my.label <- 1: nrow(my.grid.2193)

ggplot()+
  theme_bw()+
  geom_sf_text(data = my.grid.2193, aes(label = my.label))+
  geom_sf(data = my.grid.2193, colour = "black", 
          fill = "grey", alpha = 0.7)+
  geom_sf(data = aoi, fill = NA, colour = "red",
          alpha = 0.5) +
  ggtitle("15 x 15 m grid layout")+
  annotation_scale(location = "bl")+
  labs(x = "", y ="")
  
ggsave("Idealised grid layout 15 x 15m.png", scale = 1.2, height = 6, width = 9)

############# Part 2


# make 7.5 x 7.5 
small.grid <- st_make_grid(aoi.b,
                        cellsize = c(7.5, 7.5)) %>% st_as_sf()

# intersection with aoi
small.grid.2193 <- small.grid %>% st_intersection(aoi.b) 

# make union large grid
my.union.large <- st_union(my.grid.2193) %>% st_make_valid()

# make intersection
small.grid.2193  <- small.grid.2193  %>% st_intersection(my.union.large )

# check which points intersect with the polygon
intersects <- st_intersection(small.grid.2193, my.union.large)

# filter to retain only polygon geometries
intersects_polygons <- intersects %>%
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))

# label
intersects_polygons$ref <- paste0( 
  rep(1: nrow(my.grid.2193), each = 4), 
  c("c", "a", "b", "d"))

ggplot()+
  theme_bw()+
  geom_sf(data = intersects_polygons)+
  geom_sf(data = my.grid.2193, colour = "purple", 
         fill = NA, lwd = 1)+
  geom_sf(data = aoi, fill = NA, colour = "red", lwd = 1) +
  ggtitle("36 cells = 15 x 15 m grid layout &\n144 cells = 7.5 x 7.5m finer scale grid")+
  annotation_scale(location = "bl")+
  labs(x = "", y ="")+
  geom_sf_text(data = my.grid.2193, aes(label = my.label), fontface = 2, size = 4)


ggsave("Nested scale grid.png", scale = 1.4)
  