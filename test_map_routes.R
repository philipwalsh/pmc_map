# philip walsh
# philipwalsh.ds@gmail.com
# 2020-02-26
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

waterstops <- subset(waypoints, WaypointType=='waterstop')

segments_waypoints <- read.csv("data/segments_waypoints.csv", header = T)
#segments_waypoints %>% head(3)


# routes > route_segment > segments > segment_waterstop > waterstops
routes_to_segments <- full_join(routes, routes_segments)
routes_to_segments_waypoints = full_join(routes_to_segments, segments_waypoints)
routes_waypoints = full_join(routes_to_segments_waypoints, waypoints)


## setup the sate base map
map_state = "massachusetts"
states <- map_data("state")
states %>% head(3)
single_state_df <- subset(states, (region == map_state) & (subregion != "martha's vineyard") & (subregion != "nantucket") )
counties <- map_data("county")
state_counties <- subset(counties, region == map_state)


# just waterstops
state_base <- ggplot(data = single_state_df, mapping = aes(x = long, y = lat)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

state_base + theme_void() +
  geom_point(data = waterstops, aes(lon,lat), color="blue", size=2)



# all waypoints
state_base <- ggplot(data = single_state_df, mapping = aes(x = long, y = lat)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

state_base + theme_void() +
  geom_point(data = waypoints, aes(lon,lat), color="red", size=2)



 
#single route style
single_route = filter(routes_waypoints, RouteID==1) %>% arrange(RouteSegmentSequence, SegmentWaterstopSequence)
single_route_waterstops = filter(single_route, WaypointType=='waterstop')
select(single_route, RouteName, WaypointName,WaypointType, lat, lon) # ,WaterStopName,lat,lon)) 

state_base <- ggplot(data = single_state_df, mapping = aes(x = long, y = lat)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

state_base + theme_void() +
  geom_path(data = single_route, aes(lon,lat), size=1)+
  geom_point(data = single_route_waterstops, aes(lon,lat), size=1)


#single route style
single_route = filter(routes_waypoints, RouteID==2) %>% arrange(RouteSegmentSequence, SegmentWaterstopSequence)
single_route_waterstops = filter(single_route, WaypointType=='waterstop')
select(single_route, RouteName, WaypointName,WaypointType, lat, lon) # ,WaterStopName,lat,lon)) 

state_base <- ggplot(data = single_state_df, mapping = aes(x = long, y = lat)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

state_base + theme_void() +
  geom_path(data = single_route, aes(lon,lat), size=1)+
  geom_point(data = single_route_waterstops, aes(lon,lat), size=1)


## all routes, facet wrap
#single route style
all_routes = routes_waypoints %>% arrange(RouteID, RouteSegmentSequence, SegmentWaterstopSequence)
all_waterstops = filter(all_routes, WaypointType=='waterstop')
select(all_routes, RouteID, RouteName, WaypointName,WaypointType, lat, lon) # ,WaterStopName,lat,lon)) 

state_base <- ggplot(data = single_state_df, mapping = aes(x = long, y = lat)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

state_base + theme_void() +
  geom_path(data = all_routes, aes(lon,lat), size=1)+
  geom_point(data = all_waterstops, aes(lon,lat), size=1)+
  facet_wrap(~RouteID, ncol=3)

select(all_routes, RouteID, RouteName)




