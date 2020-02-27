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
#routes %>% head(3)
segments <- read.csv("data/segments.csv", header = T)
#segments %>% head(3)
routes_segments <- read.csv("data/routes_segments.csv", header = T)
#route_segment %>% head(3)
waypoints <- read.csv("data/waypoints.csv", header = T)
#waypoints %>% head(3)

hubs <- subset(waypoints, WaypointType=='hub')
waterstops <- subset(waypoints, WaypointType=='waterstop')
non_waterstops <- subset(waypoints, WaypointType=='waypoint')
segments_waypoints <- read.csv("data/segments_waypoints.csv", header = T)
#segments_waypoints %>% head(3)
# routes > route_segment > segments > segment_waterstop > waterstops

routes_to_segments <- left_join(routes, routes_segments, by="RouteID")
routes_to_segments_waypoints = left_join(routes_to_segments, segments_waypoints, by="SegmentID")
routes_waypoints = left_join(routes_to_segments_waypoints, waypoints, by='WaypointID')



## setup the sate base map
map_state = "massachusetts"
states <- map_data("state")
states %>% head(3)
single_state_df <- subset(states, (region == map_state) & (subregion != "martha's vineyard") & (subregion != "nantucket") )
counties <- map_data("county")
state_counties <- subset(counties, region == map_state)


worcester_county <- subset(state_counties, subregion == 'worcester')
norfolk_county <- subset(state_counties, subregion == 'norfolk')
bristol_county <- subset(state_counties, subregion == 'bristol')
plymouth_county <- subset(state_counties, subregion == 'plymouth')
barnstable_county <- subset(state_counties, subregion == 'barnstable')

essex_county <- subset(state_counties, subregion == 'essex')
middlesex_county <- subset(state_counties, subregion == 'middlesex')
suffolk_county <- subset(state_counties, subregion == 'suffolk')


# pmc_counties <- subset(state_counties, subregion == 'worcester' | subregion == 'norfolk' | subregion == 'bristol' | subregion == 'plymouth' | subregion == 'barnstable')
#worcester
#norfolk
#bristol
#plymouth
#barnstable

babson_to_babson = subset(routes_waypoints, RouteID==9)
head(babson_to_babson)

sturbridge_to_ptown_inn = subset(routes_waypoints, RouteID==1)
sturbridge_to_ptown_family = subset(routes_waypoints, RouteID==2)
strubridge_to_babson = subset(routes_waypoints, RouteID==3)
babson_to_ptown_inn = subset(routes_waypoints, RouteID==5)
babson_to_ptown_family = subset(routes_waypoints, RouteID==6)



routes

# just waterstops
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
  geom_path(data = babson_to_babson, aes(lon,lat), color="red", size=1)+
  geom_path(data = sturbridge_to_ptown_inn, aes(lon,lat), color="red", size=1)+
  geom_path(data = sturbridge_to_ptown_family, aes(lon,lat), color="red", size=1)+
  geom_path(data = strubridge_to_babson, aes(lon,lat), color="red", size=1)+
  geom_path(data = babson_to_ptown_inn, aes(lon,lat), color="red", size=1)+
  geom_path(data = babson_to_ptown_family, aes(lon,lat), color="red", size=1)
  




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
  


