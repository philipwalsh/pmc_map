# philip walsh
# philipwalsh.ds@gmail.com
# 2020-02-27
# map the routes, facet by routeid


library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)  


library(tidyverse)

routes <- read.csv("data/routes.csv", header = T)
segments <- read.csv("data/segments.csv", header = T)
routes_segments <- read.csv("data/routes_segments.csv", header = T)
mappoints <- read.csv("data/mappoints.csv", header = T)
segments_mappoints <- read.csv("data/segments_mappoints.csv", header = T)

# routes > route_segment > segments > segment_waterstop > waterstops

routes_to_segments <- left_join(routes, routes_segments, by="RouteID")
routes_to_segments_mappoints = left_join(routes_to_segments, segments_mappoints, by="SegmentID")
routes_mappoints = left_join(routes_to_segments_mappoints, mappoints, by='MapPointID')

routes

babson_to_babson <- subset(routes_mappoints, RouteID==9)
babson_to_babson %>% select(RouteName, MapPointType, lat, lon ) %>% head()

waterstops <- subset(babson_to_babson, MapPointType=='waterstop')
waypoints <- subset(babson_to_babson, MapPointType=='waypoint')
hubs <- subset(babson_to_babson, MapPointType=='hub')

## setup the sate base map
map_state = "massachusetts"
states <- map_data("state")
states %>% head(3)
single_state_df <- subset(states, (region == map_state) & (subregion != "martha's vineyard") & (subregion != "nantucket") )
counties <- map_data("county")
state_counties <- subset(counties, region == map_state)

head(state_counties)

babson_counties <- subset(state_counties, subregion=='norfolk')

head(babson_counties)

state_base <- ggplot(data = babson_counties, mapping = aes(x = long, y = lat)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")


# just waterstops
state_base + theme_void() +
  geom_point(data = hubs, aes(lon,lat), color="green", size=3)

# just waterstops
state_base + theme_void() +
  geom_point(data = waterstops, aes(lon,lat), color="blue", size=2)



# just waypoints
state_base + theme_void() +
  geom_path(data = waypoints, aes(lon,lat), color="red", size=1)

# all together, hubs, waterstops, waypoints
state_base + theme_void() +
  geom_path(data = waypoints, aes(lon,lat), color="red", size=1)+
  geom_point(data = waterstops, aes(lon,lat), color="blue", size=2)+
  geom_point(data = hubs, aes(lon,lat), color="darkgreen", size=5)
  







