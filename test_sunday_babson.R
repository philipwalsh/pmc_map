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
#routes %>% head(3)
segments <- read.csv("data/segments.csv", header = T)
#segments %>% head(3)
routes_segments <- read.csv("data/routes_segments.csv", header = T)
#route_segment %>% head(3)
waypoints <- read.csv("data/waypoints.csv", header = T)
#waypoints %>% head(3)



segments_waypoints <- read.csv("data/segments_waypoints.csv", header = T)
#segments_waypoints %>% head(3)
# routes > route_segment > segments > segment_waterstop > waterstops

routes_to_segments <- left_join(routes, routes_segments, by="RouteID")
routes_to_segments_waypoints = left_join(routes_to_segments, segments_waypoints, by="SegmentID")
routes_waypoints = left_join(routes_to_segments_waypoints, waypoints, by='WaypointID')

babson_to_babson <- subset(routes_waypoints, RouteID==9)

babson_to_babson
waterstops <- subset(babson_to_babson, WaypointType=='waterstop')
non_waterstops <- subset(babson_to_babson, WaypointType=='waypoint')
hubs <- subset(babson_to_babson, WaypointType=='hub')

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
  geom_path(data = non_waterstops, aes(lon,lat), color="red", size=1)

# all together, hubs, waterstops, waypoints
state_base + theme_void() +
  geom_path(data = non_waterstops, aes(lon,lat), color="red", size=1)+
  geom_point(data = waterstops, aes(lon,lat), color="blue", size=2)+
  geom_point(data = hubs, aes(lon,lat), color="darkgreen", size=5)
  







