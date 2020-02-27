# philip walsh
# philipwalsh.ds@gmail.com
# 2020-02-26
# map the routes, facet by routeid
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)  
library(sf)
library(tidyverse)


routes <- read.csv("data/routes.csv", header = T)
segments <- read.csv("data/segments.csv", header = T)
routes_segments <- read.csv("data/routes_segments.csv", header = T)
mappoints <- read.csv("data/mappoints.csv", header = T)
segments_mappoints <- read.csv("data/segments_mappoints.csv", header = T)


hubs <- subset(mappoints, MapPointType=='hub')
write.csv(hubs,"data\\_hubs.csv", row.names = FALSE)

waterstops <- subset(mappoints, MapPointType=='waterstop')
write.csv(waterstops,"data\\_waterstops.csv", row.names = FALSE)

waypoints <- subset(mappoints, MapPointType=='waypoint')
write.csv(waypoints,"data\\_waypoints.csv", row.names = FALSE)


# routes > route_segment > segments > routes_segments > mappoints


routes_to_segments <- left_join(routes, routes_segments, by="RouteID")
write.csv(routes_to_segments,"data\\_routes_to_segments.csv", row.names = FALSE)

routes_to_segments_waypoints = left_join(routes_to_segments, segments_mappoints, by="SegmentID")
write.csv(routes_to_segments_waypoints,"data\\_routes_to_segments_waypoints.csv", row.names = FALSE)


routes_waypoints = left_join(routes_to_segments_waypoints, waypoints, by='MapPointID')
write.csv(routes_waypoints,"data\\_routes_waypoints.csv", row.names = FALSE)

map_state = "massachusetts"
counties <- map_data("county")
state_counties <- subset(counties, region == map_state)

worcester_county <- subset(state_counties, subregion == 'worcester')
norfolk_county <- subset(state_counties, subregion == 'norfolk')
bristol_county <- subset(state_counties, subregion == 'bristol')
plymouth_county <- subset(state_counties, subregion == 'plymouth')
barnstable_county <- subset(state_counties, subregion == 'barnstable')
#PMC does not pass through the next 3, but the state map looks funny without them

essex_county <- subset(state_counties, subregion == 'essex')
middlesex_county <- subset(state_counties, subregion == 'middlesex')
suffolk_county <- subset(state_counties, subregion == 'suffolk')

route_sturbridge_to_ptown = subset(routes_waypoints, RouteID==1)
write.csv(route_sturbridge_to_ptown,"data//_route_sturbridge_to_ptown.csv", row.names=F)

route_sturbridge_to_babson = subset(routes_waypoints, RouteID==3)
write.csv(route_sturbridge_to_babson,"data//_route_sturbridge_to_babson.csv", row.names=F)

route_babson_to_ptown = subset(routes_waypoints, RouteID==4)
route_babson_to_babson = subset(routes_waypoints, RouteID==9)





# map it all
ggplot()+
  coord_fixed(1.3)+
  theme_void()+
  geom_polygon(data = worcester_county, mapping = aes(x = long, y = lat), fill="grey") + 
  geom_polygon(data = norfolk_county, mapping = aes(x = long, y = lat), fill="grey") + 
  geom_polygon(data = bristol_county, mapping = aes(x = long, y = lat), fill="grey") + 
  geom_polygon(data = plymouth_county, mapping = aes(x = long, y = lat), fill="grey") + 
  geom_polygon(data = barnstable_county, mapping = aes(x = long, y = lat), fill="grey") + 
  geom_polygon(data = essex_county, mapping = aes(x = long, y = lat), fill="grey") + 
  geom_polygon(data = middlesex_county, mapping = aes(x = long, y = lat), fill="grey") + 
  geom_polygon(data = suffolk_county, mapping = aes(x = long, y = lat), fill="grey") + 
  geom_path(data = route_babson_to_babson, aes(lon,lat), color="red", size=1)+
  geom_path(data = route_sturbridge_to_ptown, aes(lon,lat), color="red", size=1)+
  geom_path(data = route_sturbridge_to_babson, aes(lon,lat), color="red", size=1)+
  geom_path(data = route_babson_to_ptown, aes(lon,lat), color="red", size=1)+
  geom_point(data = waterstops, aes(lon,lat), color="blue", size=2)+
  geom_point(data = hubs, aes(lon,lat), color="darkgreen", size=3)#+facet_wrap("RouteType")
  




ggplot()+
  coord_fixed(1.3)+
  theme_void()+
  geom_polygon(data = worcester_county, mapping = aes(x = long, y = lat), fill="grey") + 
  geom_polygon(data = norfolk_county, mapping = aes(x = long, y = lat), fill="grey") + 
  geom_polygon(data = bristol_county, mapping = aes(x = long, y = lat), fill="grey") + 
  geom_polygon(data = plymouth_county, mapping = aes(x = long, y = lat), fill="grey") + 
  geom_polygon(data = barnstable_county, mapping = aes(x = long, y = lat), fill="grey") + 
  geom_polygon(data = essex_county, mapping = aes(x = long, y = lat), fill="grey") + 
  geom_polygon(data = middlesex_county, mapping = aes(x = long, y = lat), fill="grey") + 
  geom_polygon(data = suffolk_county, mapping = aes(x = long, y = lat), fill="grey") + 
  geom_point(data = waterstops, aes(lon,lat), color="blue", size=2)+
  geom_point(data = hubs, aes(lon,lat), color="darkgreen", size=5)+
  geom_path(data = routes_waypoints, aes(lon,lat), color="red", size=1)+
  facet_wrap("RouteName")
  


