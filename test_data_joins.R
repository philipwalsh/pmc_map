# philip walsh
# philipwalsh.ds@gmail.com
# 2020-02-26
# join data and see what it looks like



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
routes_to_segments <- full_join(routes, routes_segments, by="RouteID")
routes_to_segments_waypoints = full_join(routes_to_segments, segments_waypoints, by="SegmentID")
routes_waypoints = full_join(routes_to_segments_waypoints, waypoints, by='WaypointID')
summary(routes_waypoints)
str(routes_waypoints)
write.csv(routes_waypoints,"data\\flat_routes_waypoints_full_join.csv", row.names = FALSE)


routes_to_segments <- left_join(routes, routes_segments, by="RouteID")
routes_to_segments_waypoints = left_join(routes_to_segments, segments_waypoints, by="SegmentID")
routes_waypoints = left_join(routes_to_segments_waypoints, waypoints, by='WaypointID')
summary(routes_waypoints)
str(routes_waypoints)
write.csv(routes_waypoints,"data\\flat_routes_waypoints_left_join.csv", row.names = FALSE)



