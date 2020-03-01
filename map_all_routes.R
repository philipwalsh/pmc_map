# philip walsh
# philipwalsh.ds@gmail.com
# 2020-02-26
# map the routes, facet by routeid
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)  
library(tidyverse)
library(geosphere)




routes <- read.csv("data/routes.csv", header = T)
segments <- read.csv("data/segments.csv", header = T)
routes_segments <- read.csv("data/routes_segments.csv", header = T)
mappoints <- read.csv("data/mappoints.csv", header = T)
segments_mappoints <- read.csv("data/segments_mappoints.csv", header = T)


hubs <- subset(mappoints, MapPointType=='hub')
write.csv(hubs,"excluded\\_hubs.csv", row.names = FALSE)

waterstops <- subset(mappoints, MapPointType=='waterstop')
write.csv(waterstops,"excluded\\_waterstops.csv", row.names = FALSE)

waypoints <- subset(mappoints, MapPointType=='waypoint')
write.csv(waypoints,"excluded\\_waypoints.csv", row.names = FALSE)


# routes > route_segment > segments > routes_segments > mappoints


routes_to_segments <- left_join(routes, routes_segments, by="RouteID")
write.csv(routes_to_segments,"excluded\\_routes_to_segments.csv", row.names = FALSE)

routes_to_segments_waypoints = left_join(routes_to_segments, segments_mappoints, by="SegmentID")
write.csv(routes_to_segments_waypoints,"excluded\\_routes_to_segments_waypoints.csv", row.names = FALSE)


routes_waypoints = left_join(routes_to_segments_waypoints, waypoints, by='MapPointID')
write.csv(routes_waypoints,"excluded\\_routes_waypoints.csv", row.names = FALSE)

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
write.csv(route_sturbridge_to_ptown,"excluded//_route_sturbridge_to_ptown.csv", row.names=F)

route_sturbridge_to_ptown
str(route_sturbridge_to_ptown)
route_sturbridge_to_babson = subset(routes_waypoints, RouteID==3)
write.csv(route_sturbridge_to_babson,"excluded//_route_sturbridge_to_babson.csv", row.names=F)

route_babson_to_ptown = subset(routes_waypoints, RouteID==4)
route_babson_to_babson = subset(routes_waypoints, RouteID==9)





# map it all on a single plot
ggplot()+
  coord_fixed(1.3)+
  theme_void()+
  geom_polygon(data = worcester_county, mapping = aes(x = long, y = lat), fill="grey")+ 
  geom_polygon(data = norfolk_county, mapping = aes(x = long, y = lat), fill="grey")+ 
  geom_polygon(data = bristol_county, mapping = aes(x = long, y = lat), fill="grey")+ 
  geom_polygon(data = plymouth_county, mapping = aes(x = long, y = lat), fill="grey")+ 
  geom_polygon(data = barnstable_county, mapping = aes(x = long, y = lat), fill="grey")+
  geom_polygon(data = essex_county, mapping = aes(x = long, y = lat), fill="grey")+ 
  geom_polygon(data = middlesex_county, mapping = aes(x = long, y = lat), fill="grey")+
  geom_polygon(data = suffolk_county, mapping = aes(x = long, y = lat), fill="grey")+
  geom_path(data = route_babson_to_babson, aes(lon,lat), color="red", size=1)+
  geom_path(data = route_sturbridge_to_ptown, aes(lon,lat), color="red", size=1)+
  geom_path(data = route_sturbridge_to_babson, aes(lon,lat), color="red", size=1)+
  geom_path(data = route_babson_to_ptown, aes(lon,lat), color="red", size=1)+
  geom_point(data = waterstops, aes(lon,lat), color="blue", size=2)+
  geom_point(data = hubs, aes(lon,lat), color="darkgreen", size=3)#+facet_wrap("RouteType")
  



# map it all, facet wrap by route
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
  facet_wrap("RouteName", ncol=3)
  





# map just a single route
ggplot()+
  coord_fixed(1.3)+
  theme_void()+
  geom_polygon(data = norfolk_county, mapping = aes(x = long, y = lat), fill="grey")+ 
  geom_path(data = route_babson_to_babson, aes(lon,lat), color="red", size=1)+
  geom_point(data = waterstops, aes(lon,lat), color="blue", size=2)+
  geom_point(data = hubs, aes(lon,lat), color="darkgreen", size=3)#+facet_wrap("RouteType")



##
## TODO: do not display waterstops or hubs that are not on the route
##

## heres the situation, when i wnt to display a route, i have waypoints for the route
## then there are hubs, not specific to route, just hubs that routes may pass through
## then there are waterstops.  same situation, the waypoints happen to pass very close
## so I want to use distance formula to choose which waterstops and hubs i need to display
## 



lat1=route_babson_to_babson$lat[1]
lon1=route_babson_to_babson$lon[1]
lat2=route_babson_to_babson$lat[55]
lon2=route_babson_to_babson$lon[55]
distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
#[1,] 22586.44


lat1=route_babson_to_babson$lat[50]
lon1=route_babson_to_babson$lon[50]
lat2=route_babson_to_babson$lat[55]
lon2=route_babson_to_babson$lon[55]
distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
#[1,] 1778.322


abs(hubs$lat - lat_avg)

hubs

# patriot place lat lon should have a short distance to first point on segment pat_to_bab
# or the last 
hubs$MapPointName[5]

segments #8 is the one
route_babson_to_babson

nrow(subset(route_babson_to_babson, SegmentID==8))

lat1=subset(route_babson_to_babson, SegmentID==8)$lat[nrow(subset(route_babson_to_babson, SegmentID==8))]
lon1=subset(route_babson_to_babson, SegmentID==8)$lon[nrow(subset(route_babson_to_babson, SegmentID==8))]

lat1=subset(route_babson_to_babson, SegmentID==8)$lat[nrow(subset(route_babson_to_babson, SegmentID==8))]
lon1=subset(route_babson_to_babson, SegmentID==8)$lon[nrow(subset(route_babson_to_babson, SegmentID==8))]
lat2=hubs$lat[5]
lon2=hubs$lon[5]
distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)


