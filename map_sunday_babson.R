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



routes_and_segments_temp = left_join(routes, routes_segments, by="RouteID")
routes_and_segments = right_join(routes_and_segments_temp, segments, by="SegmentID")
segments_and_mappoints = left_join(segments, segments_mappoints, by="SegmentID")
all_routes_and_segmentspoints <- left_join(routes_and_segments, segments_mappoints, by="SegmentID")
all_routes <- left_join(all_routes_and_segmentspoints, mappoints, by="MapPointID")




#routes

babson_to_babson <- subset(all_routes, RouteID==9)
babson_to_babson %>% select(RouteName, MapPointType, lat, lon ) %>% head()

waterstops <- subset(mappoints, MapPointType=='waterstop')
waterstops <- waterstops %>% filter(MapPointID==15 | MapPointID==13)
waterstops


waypoints <- subset(babson_to_babson, MapPointType=='waypoint')

hubs <- subset(mappoints, MapPointType=='hub')
hubs <- hubs %>% filter(MapPointID==12 | MapPointID==14)
hubs

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


# just county hubs
state_base + theme_void() +
  geom_point(data = hubs, aes(lon,lat), color="darkgreen", size=3)

# add in waterstops
state_base + theme_void() +
  geom_point(data = hubs, aes(lon,lat), color="darkgreen", size=3)+
  geom_point(data = waterstops, aes(lon,lat), color="blue", size=2)

# all together, hubs, waterstops, waypoints
state_base + theme_void() +
  geom_path(data = waypoints, aes(lon,lat), color="red", size=1)+
  geom_point(data = waterstops, aes(lon,lat), color="blue", size=2)+
  geom_point(data = hubs, aes(lon,lat), color="darkgreen", size=5)
  







