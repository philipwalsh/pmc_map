# philip walsh
# philipwalsh.ds@gmail.com
# 2020-03-01
# exploratory data analysis
library(tidyverse)


# all data stored in .csv files in the data/ dir

list.dirs(recursive=F)
#[1] "./.git"        "./.Rproj.user" "./.vs"         "./data"        "./excluded"

list.files('data/')
#[1] "mappoints.csv" "routes.csv" "routes_segments.csv" "segments.csv" "segments_mappoints.csv"
mappoints <- read.csv("data/mappoints.csv", header = T)
routes <- read.csv("data/routes.csv", header = T)
routes_segments <- read.csv("data/routes_segments.csv", header = T)
segments <- read.csv("data/segments.csv", header = T)
segments_mappoints <- read.csv("data/segments_mappoints.csv", header = T)


##
## explore mappoints
##

mappoints %>% head()
#  MapPointID          MapPointName MapPointType      lat       lon
#1          1            Sturbridge          hub 42.11296 -72.08805
#2          2          Whitinsville    waterstop 42.10892 -71.66112
#3          3              Franklin    waterstop 42.05620 -71.42097
#4          4      Dighton Rehoboth          hub 41.85193 -71.20301
#5          5               Wareham    waterstop 41.75448 -70.69883
#6          6 Mass Maritime Academy          hub 41.74085 -70.62206

# check for missing data
sapply(mappoints, function(x) sum(is.na(x)))

#MapPointID MapPointName MapPointType          lat          lon 
#0            0            0            0            0

# good to go, no misisng data here


mappoints %>% 
  group_by(MapPointType) %>% 
  summarise(map_point_type_count = n())

#1 hub                             6
#2 waterstop                       9
#3 waypoint                      184

# so a few hubs, a few water stops and lot of way points

mappoints %>% 
  group_by(MapPointType) %>% 
  summarise(map_point_type_count = n()) %>% 
  ggplot(aes(x = MapPointType, y = map_point_type_count, fill = MapPointType)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x="Map Point Types", y="Count", title="types within the data")


mappoints %>% filter(MapPointType=='hub')
#MapPointID          MapPointName MapPointType      lat       lon
#1          1            Sturbridge          hub 42.11296 -72.08805
#2          4      Dighton Rehoboth          hub 41.85193 -71.20301
#3          6 Mass Maritime Academy          hub 41.74085 -70.62206
#4          9                P-Town          hub 42.03704 -70.19849
#5         12         Patriot Place          hub 42.08836 -71.26162
#6         14                Babson          hub 42.29598 -71.26289

mappoints %>% filter(MapPointType=='waterstop')
#MapPointID MapPointName MapPointType      lat       lon
#1          2 Whitinsville    waterstop 42.10892 -71.66112
#2          3     Franklin    waterstop 42.05620 -71.42097
#3          5      Wareham    waterstop 41.75448 -70.69883
#4          7   Barnstable    waterstop 41.69328 -70.28678
#5          8     Brewster    waterstop 41.77566 -70.03285
#6         11   Middleboro    waterstop 41.90734 -70.88962
#7         13      Walpole    waterstop 42.16322 -71.23789
#8         15     Medfield    waterstop 42.18563 -71.29756
#9         16     Wrentham    waterstop 42.08357 -71.32636

mappoints %>% filter(MapPointType=='waypoint') %>% head()
#  MapPointID  MapPointName MapPointType      lat       lon
#1         18 brew_to_ptown     waypoint 41.78652 -69.99322
#2         19 brew_to_ptown     waypoint 41.83669 -69.97425
#3         20 brew_to_ptown     waypoint 41.91554 -69.98828
#4         21 brew_to_ptown     waypoint 42.03451 -70.08034
#5         22 brew_to_ptown     waypoint 42.06577 -70.15304
#6         23    bab_to_med     waypoint 42.29857 -71.26071


#what are the names within the waypoints
mappoints %>% 
  filter(MapPointType=='waypoint') %>%
  group_by(MapPointName) %>% 
  summarise(count_by_name = n())

#1 bab_to_med                 33
#2 bab_to_wren                27
#3 barn_to_brew                2
#4 brew_to_ptown               6
#5 dr_to_mma                   2
#6 frank_to_lunch              2
#7 med_to_pat                 24
#8 mma_to_barn                 5
#9 mma_to_pat                  2
#10 pat_to_wal                29
#11 stur_to_whit               2
#12 wal_to_bab                17
#13 whit_to_frank              2
#14 wren_to_lunch             31

# the name of the waypoint indicates the starting waterstop/hub and finishing waterstop/hub
# some of the gaps, from point a to point b have many waypoints
# and some of them are lacking.  we will see this lack of definition 
# more clearly when we map the routes
# some of the routes will follow a winding path while others will be a straight line


mappoints %>% 
  group_by(MapPointName) %>% 
  filter(MapPointType=='waypoint') %>%
  summarise(map_point_name_count = n()) %>% 
  ggplot(aes(x = MapPointName, y = map_point_name_count, fill = MapPointName)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x="Way Point Names", y="Count", title="waypoint names within the data")


# i will create a seperate varable for all 3 types
# and I like to save each out to the excluded folder
# the excluded folder will not be checked into github via the .gitignore
hubs <- subset(mappoints, MapPointType=='hub')
write.csv(hubs,"excluded\\_hubs.csv", row.names = FALSE)

waterstops <- subset(mappoints, MapPointType=='waterstop')
write.csv(waterstops,"excluded\\_waterstops.csv", row.names = FALSE)

waypoints <- subset(mappoints, MapPointType=='waypoint')
write.csv(waypoints,"excluded\\_waypoints.csv", row.names = FALSE)



##
## explore routes
##

routes

#  RouteID RouteType                         RouteName                  RouteDescription
#1       1     2 Day          Sturbridge to P-Town Inn          Sturbridge to P-Town Inn
#2       3     2 Day              Sturbridge to Babson              Sturbridge to Babson
#3       4     2 Day                  Babson to P-Town                  Babson to P-Town
#4       6     1 Day                 Sturbridge to MMA                 Sturbridge to MMA
#5       7     1 Day                     Babson to MMA                     Babson to MMA
#6       8     1 Day                     MMA to Babson                     MMA to Babson
#7       9     1 Day Babson to Patriot Place to Babson Babson to Patriot Place to Babson
#8      10     1 Day           Babson to Patriot Place           Babson to Patriot Place



# the data frou routes is tiny, and i can see there are no NAs
sapply(routes, function(x) sum(is.na(x)))
#         RouteID        RouteType        RouteName RouteDescription 
#               0                0                0                0



# up to this point, we have 
# mappoints with a MapPointID
# routes with a RouteID
# the remainder of the data will tie the routes to the mappoints


##
## explore Segements
## 

nrow(segments)
#[1] 7

segments 

#  SegmentID                            SegmentName                     SegmentDescription Day
#1         1 Sturbridge to Dighton Rehoboth (Lunch) Sturbridge to Dighton Rehoboth (Lunch)   1
#2         2                              DR to MMA                              DR to MMA   1
#3         4                          MMA to P-Town               Brerewster to P-Town Inn   2
#4         6     Babson to Dighton Rehoboth (Lunch)     Babson to Dighton Rehoboth (Lunch)   1
#5         7                   MMA to Patriot Place                   MMA to Patriot Place   2
#6         8                Babson to Patriot Place                Babson to Patriot Place   2
#7         9                Patriot Place to Babson                Patriot Place to Babson   2

# one occular pat down and we see all the data with no NAs




##
## explore routes_segments
##

nrow(routes_segments)
#[1] 19

routes_segments %>% head()
#  RouteID RouteSegmentSequence SegmentID
#1       1                    1         1
#2       1                    2         2
#3       1                    3         4
#4       3                    1         1
#5       3                    2         2
#6       3                    3         7

# one to many relationship, routes >> routes_segments

routes_and_segments_temp = left_join(routes, routes_segments, by="RouteID")
routes_and_segments_temp %>% head()
#  RouteID RouteType                RouteName         RouteDescription RouteSegmentSequence SegmentID
#1       1     2 Day Sturbridge to P-Town Inn Sturbridge to P-Town Inn                    1         1
#2       1     2 Day Sturbridge to P-Town Inn Sturbridge to P-Town Inn                    2         2
#3       1     2 Day Sturbridge to P-Town Inn Sturbridge to P-Town Inn                    3         4
#4       3     2 Day     Sturbridge to Babson     Sturbridge to Babson                    1         1
#5       3     2 Day     Sturbridge to Babson     Sturbridge to Babson                    2         2
#6       3     2 Day     Sturbridge to Babson     Sturbridge to Babson                    3         7



# and there is a segmentid, many to one relationship
routes_and_segments = right_join(routes_and_segments_temp, segments, by="SegmentID")




routes_and_segments %>% head() 

routes_and_segments %>% 
  select(RouteID, RouteType, RouteName, RouteSegmentSequence,SegmentID,SegmentName) %>% 
  arrange(RouteID) %>% 
  head()

#  RouteID RouteType                RouteName RouteSegmentSequence SegmentID                            SegmentName
#1       1     2 Day Sturbridge to P-Town Inn                    1         1 Sturbridge to Dighton Rehoboth (Lunch)
#2       1     2 Day Sturbridge to P-Town Inn                    2         2                              DR to MMA
#3       1     2 Day Sturbridge to P-Town Inn                    3         4                          MMA to P-Town
#4       3     2 Day     Sturbridge to Babson                    1         1 Sturbridge to Dighton Rehoboth (Lunch)
#5       3     2 Day     Sturbridge to Babson                    2         2                              DR to MMA
#6       3     2 Day     Sturbridge to Babson                    3         7                   MMA to Patriot Place


routes_and_segments %>%
  select(RouteID, RouteType, RouteName, RouteSegmentSequence,SegmentID,SegmentName) %>%
  arrange(RouteID) %>%
  filter(RouteID<5) 
#   RouteID RouteType                RouteName RouteSegmentSequence SegmentID                            SegmentName
#1        1     2 Day Sturbridge to P-Town Inn                    1         1 Sturbridge to Dighton Rehoboth (Lunch)
#2        1     2 Day Sturbridge to P-Town Inn                    2         2                              DR to MMA
#3        1     2 Day Sturbridge to P-Town Inn                    3         4                          MMA to P-Town
#4        3     2 Day     Sturbridge to Babson                    1         1 Sturbridge to Dighton Rehoboth (Lunch)
#5        3     2 Day     Sturbridge to Babson                    2         2                              DR to MMA
#6        3     2 Day     Sturbridge to Babson                    3         7                   MMA to Patriot Place
#7        3     2 Day     Sturbridge to Babson                    4         9                Patriot Place to Babson
#8        4     2 Day         Babson to P-Town                    2         2                              DR to MMA
#9        4     2 Day         Babson to P-Town                    3         4                          MMA to P-Town
#10       4     2 Day         Babson to P-Town                    1         6     Babson to Dighton Rehoboth (Lunch)


routes_and_segments %>% group_by(RouteID) %>% 
  summarise(count_of_route_segmnets=n())

#  RouteID count_of_route_segmnets
#    <int>                   <int>
#1       1                       3
#2       3                       4
#3       4                       3
#4       6                       2
#5       7                       2
#6       8                       2
#7       9                       2
#8      10                       1

# varying segments per route, a high of 4 for route 3, and a low of 1 for route 10


# how many times are each segment in play
# group by ID
routes_and_segments %>% 
  group_by(SegmentID) %>% 
  summarise(segment_count=n())
#  SegmentID segment_count
#      <int>         <int>
#1         1             3
#2         2             5
#3         4             2
#4         6             2
#5         7             2
#6         8             2
#7         9             3

#same as abouve, group by name
routes_and_segments %>% 
  group_by(SegmentName) %>% 
  summarise(segment_count=n())
#  SegmentName                            segment_count
#  <fct>                                          <int>
#1 Babson to Dighton Rehoboth (Lunch)                 2
#2 Babson to Patriot Place                            2
#3 DR to MMA                                          5
#4 MMA to P-Town                                      2
#5 MMA to Patriot Place                               2
#6 Patriot Place to Babson                            3
#7 Sturbridge to Dighton Rehoboth (Lunch)             3

# same again, group by id and name
routes_and_segments %>% 
  group_by(SegmentID,SegmentName) %>% 
  summarise(segment_count=n())
#  SegmentID SegmentName                            segment_count
#      <int> <fct>                                          <int>
#1         1 Sturbridge to Dighton Rehoboth (Lunch)             3
#2         2 DR to MMA                                          5
#3         4 MMA to P-Town                                      2
#4         6 Babson to Dighton Rehoboth (Lunch)                 2
#5         7 MMA to Patriot Place                               2
#6         8 Babson to Patriot Place                            2
#7         9 Patriot Place to Babson                            3


# so the Dighton Rehoboth to Mass Maritime segment is in use by 5 routes
# Babson to DR is in use by 2 routes, hmmmm, thats peculiar, I will need to investigate this

write.csv(all_routes_and_segments,"excluded\\_all_routes_investigate.csv", row.names = FALSE)

# so there are 2 official routes that pas through the segment Babson to DR
#  1 day, Babson to MMA
#  2 day, Babson to MMA to PTown
#  its redundant a bit but when riders register,the sign up for wither of the 2 routs, not an ala carte type of selection.


##
## explore segments map points
##

nrow(segments_mappoints)
#[1] 184

# ah we are now getting to the meat of the data


segments_mappoints %>% head()
#  SegmentID SegmentMapPointSequence MapPointID
#1         1                       1        184
#2         1                       2        185
#3         1                       3        186
#4         1                       4        187
#5         1                       5        188
#6         1                       6        189
# SegmentId is the foreign key, with a sequence to mke it unique
# and a join out to the map point, witch we can assume may be a one to many, dont assume a point inst used more than once



segments_and_mappoints = left_join(segments, segments_mappoints, by="SegmentID")
nrow(test_join_3)
test_join_3 %>% head()








#routes_to_segments <- left_join(routes, routes_segments, by="RouteID")
#write.csv(routes_to_segments,"excluded\\_routes_to_segments.csv", row.names = FALSE)
#routes_to_segments_waypoints = left_join(routes_to_segments, segments_mappoints, by="SegmentID")
#write.csv(routes_to_segments_waypoints,"excluded\\_routes_to_segments_waypoints.csv", row.names = FALSE)
#routes_waypoints = left_join(routes_to_segments_waypoints, waypoints, by='MapPointID')
#write.csv(routes_waypoints,"excluded\\_routes_waypoints.csv", row.names = FALSE)


