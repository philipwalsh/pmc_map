# philip walsh
# philipwalsh.ds@gmail.com
# 2020-03-01
# exploratory data analysis
library(tidyverse)


# all data stored in .csv files in the data/ dir

list.dirs(recursive=F)
#[1] "./.git"        "./.Rproj.user" "./.vs"         "./altdata"     "./data"        "./excluded"    "./images"

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



# and there is a segmentid, many to one relationship to the segments table
routes_and_segments = right_join(routes_and_segments_temp, segments, by="SegmentID")



#lets take a peek at the first 10 rows, a few of the columns, ordered by RouteID, RouteSegmentSeuqence
routes_and_segments %>% 
  select(RouteID, RouteType, RouteName, RouteSegmentSequence,SegmentID,SegmentName) %>% 
  arrange(RouteID, RouteSegmentSequence) %>% 
  head(10)

#   RouteID RouteType                RouteName RouteSegmentSequence SegmentID                            SegmentName
#1        1     2 Day Sturbridge to P-Town Inn                    1         1 Sturbridge to Dighton Rehoboth (Lunch)
#2        1     2 Day Sturbridge to P-Town Inn                    2         2                              DR to MMA
#3        1     2 Day Sturbridge to P-Town Inn                    3         4                          MMA to P-Town
#4        3     2 Day     Sturbridge to Babson                    1         1 Sturbridge to Dighton Rehoboth (Lunch)
#5        3     2 Day     Sturbridge to Babson                    2         2                              DR to MMA
#6        3     2 Day     Sturbridge to Babson                    3         7                   MMA to Patriot Place
#7        3     2 Day     Sturbridge to Babson                    4         9                Patriot Place to Babson
#8        4     2 Day         Babson to P-Town                    1         6     Babson to Dighton Rehoboth (Lunch)
#9        4     2 Day         Babson to P-Town                    2         2                              DR to MMA
#10       4     2 Day         Babson to P-Town                    3         4                          MMA to P-Town



# lets have a look at all routes with RouteID>4
# ordered properly of course
routes_and_segments %>%
  select(RouteID, RouteType, RouteName, RouteSegmentSequence,SegmentID,SegmentName) %>%
  arrange(RouteID, RouteSegmentSequence) %>%
  filter(RouteID>4) 
#  RouteID RouteType                         RouteName RouteSegmentSequence SegmentID                            SegmentName
#1       6     1 Day                 Sturbridge to MMA                    1         1 Sturbridge to Dighton Rehoboth (Lunch)
#2       6     1 Day                 Sturbridge to MMA                    2         2                              DR to MMA
#3       7     1 Day                     Babson to MMA                    1         6     Babson to Dighton Rehoboth (Lunch)
#4       7     1 Day                     Babson to MMA                    2         2                              DR to MMA
#5       8     1 Day                     MMA to Babson                    1         7                   MMA to Patriot Place
#6       8     1 Day                     MMA to Babson                    2         9                Patriot Place to Babson
#7       9     1 Day Babson to Patriot Place to Babson                    1         8                Babson to Patriot Place
#8       9     1 Day Babson to Patriot Place to Babson                    2         9                Patriot Place to Babson
#9      10     1 Day           Babson to Patriot Place                    1         8                Babson to Patriot Place



# let's count up the number of segments grouped by the RouteID
routes_and_segments %>% group_by(RouteID, RouteName) %>% 
  summarise(count_of_route_segments=n())

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


# how many times are each segment in play, total (regardless of routes)
# group by ID

# we can add in segment name to the group by just to see the name, it wont add any extra rows to the results
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


# DR to MMA (aka Dighton Rehoboth to Mass Maritime) segment is in use by 5 routes
# Babson to Dighton Rehoboth is in use by 2 routes, hmmmm, thats peculiar, I will need to investigate this

write.csv(all_routes_and_segments,"excluded\\_all_routes_investigate.csv", row.names = FALSE)

# 
# so there are 2 official routes that pass through the segment Babson to DR
#  1 day, Babson to MMA
#  2 day, Babson to MMA to PTown
#  its redundant a bit but when riders register,they sign up for either of the 2 routes, not an ala carte type of selection.
#  similar to the menu options special-#1: spam and bacon or special-#2: spam, spam and bacon



##
## explore segments map points
##

nrow(segments_mappoints)
#[1] 184

# ah we are now getting to the meat (or glue) of the data


segments_mappoints %>% head()
#  SegmentID SegmentMapPointSequence MapPointID
#1         1                       1        184
#2         1                       2        185
#3         1                       3        186
#4         1                       4        187
#5         1                       5        188
#6         1                       6        189
# SegmentId is a foreign key back to segments
# MapPointID is a foreign key back to mappoints
# and sequence keeps all the data nice and unique



segments_and_mappoints = left_join(segments, segments_mappoints, by="SegmentID")
nrow(segments_and_mappoints)
segments_and_mappoints %>% head()


# a sanity check goup by and count, we have seen this count before, 
# when we were exploring the segments_mappoints table by itself
# we should see similar numbers after the join

segments_and_mappoints %>% 
  group_by(SegmentID) %>% 
  summarise(my_count=n())
#  SegmentID my_count
#      <int>    <int>
#1         1        6
#2         2        2
#3         4       13
#4         6       58
#5         7        2
#6         8       57
#7         9       46

# ok, so it all jives with previous group by and count
# a few segmnets defined with many map points
# a few segments with only 2 points, a straight line
# so unless you are on a flying bike, good luck following thos 2 points.


# we should be able to bring it all together now
# joing routes to segments to map points

all_routes_and_segmentspoints <- left_join(routes_and_segments, segments_mappoints, by="SegmentID")
all_routes_and_segmentspoints %>% nrow()
#[1] 426

all_routes_and_segmentspoints %>% 
  select(RouteID, RouteName, SegmentID,RouteSegmentSequence, SegmentName, SegmentMapPointSequence, MapPointID) %>% 
  arrange(RouteID, RouteSegmentSequence, SegmentMapPointSequence) %>% head()


#  RouteID                RouteName SegmentID RouteSegmentSequence                            SegmentName SegmentMapPointSequence MapPointID
#1       1 Sturbridge to P-Town Inn         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       1        184
#2       1 Sturbridge to P-Town Inn         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       2        185
#3       1 Sturbridge to P-Town Inn         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       3        186
#4       1 Sturbridge to P-Town Inn         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       4        187
#5       1 Sturbridge to P-Town Inn         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       5        188
#6       1 Sturbridge to P-Town Inn         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       6        189

# we need to join in the map points as the last step

all_routes <- left_join(all_routes_and_segmentspoints, mappoints, by="MapPointID")
all_routes %>% nrow()
#[1] 426

all_routes %>% 
  select(RouteID, RouteName, SegmentID,RouteSegmentSequence, SegmentName, SegmentMapPointSequence, MapPointID, lat, lon) %>% 
  arrange(RouteID, RouteSegmentSequence, SegmentMapPointSequence) %>% head()

#  RouteID                RouteName SegmentID RouteSegmentSequence                            SegmentName SegmentMapPointSequence MapPointID      lat       lon
#1       1 Sturbridge to P-Town Inn         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       1        184 42.11189 -72.08719
#2       1 Sturbridge to P-Town Inn         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       2        185 42.10617 -71.68397
#3       1 Sturbridge to P-Town Inn         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       3        186 42.09567 -71.64397
#4       1 Sturbridge to P-Town Inn         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       4        187 42.05627 -71.41965
#5       1 Sturbridge to P-Town Inn         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       5        188 42.05518 -71.42257
#6       1 Sturbridge to P-Town Inn         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       6        189 41.85192 -71.19944


# lets see what routes are defined the best, the best meaning the most map points (assuming the person that created the points i snot a raving lunatic)
all_routes %>% 
  group_by(RouteID, RouteName) %>% 
  summarise(mappointcount=n())

#RouteID RouteName                         mappointcount
#    <int> <fct>                                     <int>
#1       1 Sturbridge to P-Town Inn                     21
#2       3 Sturbridge to Babson                         56
#3       4 Babson to P-Town                             73
#4       6 Sturbridge to MMA                             8
#5       7 Babson to MMA                                60
#6       8 MMA to Babson                                48
#7       9 Babson to Patriot Place to Babson           103
#8      10 Babson to Patriot Place                      57

# so route 9 has the most
# route 6 is not defined well at all

# what are the speicifc types of map points, included in the route

all_routes %>%
  group_by(MapPointType) %>% 
  summarise(point_type_count=n())
#   MapPointType point_type_count
#   <fct>                   <int>
# 1 waypoint                  426

# only waypoints included in the data
# so when we want to draw the routes
# we will place routes, waterstops and hubs onto the map seperately
# the data could have been defined differently, i could have placed hubs into its own file
# waterstops as well could have been broken out into its own file
# but given each type of mappoint was just a map point, i decided to
# keep it al into a single file with the type attribute
# I think i may want to add in a file for route -> waterstop
# and route -> hub



# the route with the least amount of map points
all_routes %>% 
  select(RouteID, RouteName, SegmentID,RouteSegmentSequence, SegmentName, SegmentMapPointSequence, MapPointID, lat, lon) %>% 
  arrange(RouteID, RouteSegmentSequence, SegmentMapPointSequence) %>% 
  filter(RouteID == 6)

#  RouteID         RouteName SegmentID RouteSegmentSequence                            SegmentName SegmentMapPointSequence MapPointID      lat       lon
#1       6 Sturbridge to MMA         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       1        184 42.11189 -72.08719
#2       6 Sturbridge to MMA         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       2        185 42.10617 -71.68397
#3       6 Sturbridge to MMA         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       3        186 42.09567 -71.64397
#4       6 Sturbridge to MMA         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       4        187 42.05627 -71.41965
#5       6 Sturbridge to MMA         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       5        188 42.05518 -71.42257
#6       6 Sturbridge to MMA         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       6        189 41.85192 -71.19944
#7       6 Sturbridge to MMA         2                    2                              DR to MMA                       1        190 41.85109 -71.19523
#8       6 Sturbridge to MMA         2                    2                              DR to MMA                       2        191 41.74258 -70.61869


# RouteID 1 is the most popular route, it is the classic 2 day route, sturbridge to ptown
all_routes %>% 
  select(RouteID, RouteName, SegmentID,RouteSegmentSequence, SegmentName, SegmentMapPointSequence, MapPointID, lat, lon) %>% 
  arrange(RouteID, RouteSegmentSequence, SegmentMapPointSequence) %>% 
  filter(RouteID == 1)

#    RouteID                RouteName SegmentID RouteSegmentSequence                            SegmentName SegmentMapPointSequence MapPointID      lat       lon
# 1        1 Sturbridge to P-Town Inn         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       1        184 42.11189 -72.08719
# 2        1 Sturbridge to P-Town Inn         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       2        185 42.10617 -71.68397
# 3        1 Sturbridge to P-Town Inn         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       3        186 42.09567 -71.64397
# 4        1 Sturbridge to P-Town Inn         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       4        187 42.05627 -71.41965
# 5        1 Sturbridge to P-Town Inn         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       5        188 42.05518 -71.42257
# 6        1 Sturbridge to P-Town Inn         1                    1 Sturbridge to Dighton Rehoboth (Lunch)                       6        189 41.85192 -71.19944
# 7        1 Sturbridge to P-Town Inn         2                    2                              DR to MMA                       1        190 41.85109 -71.19523
# 8        1 Sturbridge to P-Town Inn         2                    2                              DR to MMA                       2        191 41.74258 -70.61869
# 9        1 Sturbridge to P-Town Inn         4                    3                          MMA to P-Town                       1        194 41.74519 -70.61558
#10        1 Sturbridge to P-Town Inn         4                    3                          MMA to P-Town                       2        195 41.75192 -70.59226
#11        1 Sturbridge to P-Town Inn         4                    3                          MMA to P-Town                       3        196 41.74354 -70.58639
#12        1 Sturbridge to P-Town Inn         4                    3                          MMA to P-Town                       4        197 41.76804 -70.52304
#13        1 Sturbridge to P-Town Inn         4                    3                          MMA to P-Town                       5        198 41.69602 -70.34429
#14        1 Sturbridge to P-Town Inn         4                    3                          MMA to P-Town                       6        199 41.70510 -70.22397
#15        1 Sturbridge to P-Town Inn         4                    3                          MMA to P-Town                       7        200 41.75626 -70.09041
#16        1 Sturbridge to P-Town Inn         4                    3                          MMA to P-Town                       8         18 41.78652 -69.99322
#17        1 Sturbridge to P-Town Inn         4                    3                          MMA to P-Town                       9         19 41.83669 -69.97425
#18        1 Sturbridge to P-Town Inn         4                    3                          MMA to P-Town                      10         20 41.91554 -69.98828
#19        1 Sturbridge to P-Town Inn         4                    3                          MMA to P-Town                      11         21 42.03451 -70.08034
#20        1 Sturbridge to P-Town Inn         4                    3                          MMA to P-Town                      12         22 42.06577 -70.15304
#21        1 Sturbridge to P-Town Inn         4                    3                          MMA to P-Town                      13        201 42.03924 -70.19997


write.csv(all_routes,"excluded\\_all_routes.csv", row.names = FALSE)




##
## sanity check #1/3
##

## do we have any mappoints of type waypoint, that are not being used in a segment?

# use anti_join 
# 
#
# anti_join ( table1, table2, by="")
#  or
# anti_join ( table1, table2, by=c("",""))
#
# what records are in table1 that are not in table2
# what map points are not being referenced in the segments_mappoints table.
#  note - it wouldnt be the worst thing to have a map point (waypoint) defined but not in play.  an orphaned record here may
#         mean we are still building the watpoint data nad we didnt tie it inot the many to many joining table yet


anti_join(mappoints, segments_mappoints, by="MapPointID")
#    MapPointID          MapPointName MapPointType      lat       lon
# 1           1            Sturbridge          hub 42.11296 -72.08805
# 2           2          Whitinsville    waterstop 42.10892 -71.66112
# 3           3              Franklin    waterstop 42.05620 -71.42097
# 4           4      Dighton Rehoboth          hub 41.85193 -71.20301
# 5           5               Wareham    waterstop 41.75448 -70.69883
# 6           6 Mass Maritime Academy          hub 41.74085 -70.62206
# 7           7            Barnstable    waterstop 41.69328 -70.28678
# 8           8              Brewster    waterstop 41.77566 -70.03285
# 9           9                P-Town          hub 42.03704 -70.19849
#10          11            Middleboro    waterstop 41.90734 -70.88962
#11          12         Patriot Place          hub 42.08836 -71.26162
#12          13               Walpole    waterstop 42.16322 -71.23789
#13          14                Babson          hub 42.29598 -71.26289
#14          15              Medfield    waterstop 42.18563 -71.29756
#15          16              Wrentham    waterstop 42.08357 -71.32636

# so the results above are super clean.  no waypoints show up as missing.


##
## sanity check #2/3
##
## are there any routes defined, without a map to a segment?

anti_join(routes, routes_segments, by="RouteID")
#[1] RouteID          RouteType        RouteName        RouteDescription
#<0 rows> (or 0-length row.names)

# good to go, all routes included in the routes_segments


##
## sanity check #3/3
##

# do we have any segments, that are not being used in a route?
anti_join(segments, routes_segments, by="SegmentID")
#[1] SegmentID          SegmentName        SegmentDescription Day               
#<0 rows> (or 0-length row.names)




# this is a little too clean for my liking.  i need to test the case where the data is not so clean
# 
# TODO:  create an alternative set of data, with missing data and errors to ferret out my script bugs
#


