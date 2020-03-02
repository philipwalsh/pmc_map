# philip walsh
# philipwalsh.ds@gmail.com
# 2020-03-02
# exploratory data analysis
# as mentioned in script eda.R, the data was way too clean
# this eda_alt.R script will read from the altdata\ dir
# the data in this file is not clean.  missing values, orphaned records and such and such

library(tidyverse)


# all data stored in .csv files in the data/ dir

list.dirs(recursive=F)
#[1] "./.git"        "./.Rproj.user" "./.vs"         "./data"        "./excluded"

list.files('altdata/')
#[1] "mappoints.csv" "routes.csv" "routes_segments.csv" "segments.csv" "segments_mappoints.csv"
mappoints <- read.csv("altdata/mappoints.csv", header = T)
routes <- read.csv("altdata/routes.csv", header = T)
routes_segments <- read.csv("altdata/routes_segments.csv", header = T)
segments <- read.csv("altdata/segments.csv", header = T)
segments_mappoints <- read.csv("altdata/segments_mappoints.csv", header = T)


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
#         0            0            0            0            1

# one misisng value in the lon col, lets see it
mappoints[is.na(mappoints$lon),]
#MapPointID  MapPointName MapPointType      lat lon
#19         21 brew_to_ptown     waypoint 42.03451  NA


#how many other brew_to_ptown points do we hace
mappoints %>% filter(MapPointName=="brew_to_ptown")
#  MapPointID  MapPointName MapPointType      lat       lon
#1         18 brew_to_ptown     waypoint 41.78652 -69.99322
#2         19 brew_to_ptown     waypoint 41.83669 -69.97425
#3         20 brew_to_ptown     waypoint 41.91554 -69.98828
#4         21 brew_to_ptown     waypoint 42.03451        NA
#5         22 brew_to_ptown     waypoint 42.06577 -70.15304
#6        201 brew_to_ptown     waypoint 42.03924 -70.19997


# its a waypoint.  what segment is that in?
segments_mappoints %>% filter(MapPointID==21)


# segment 4 - lets have a look
segments_mappoints %>% 
  filter(SegmentID==4) %>% 
  arrange(SegmentMapPointSequence)
# the waypoint is not a book end, and this route segment has plenty of other waypoints

str(mappoints)
#'data.frame':	199 obs. of  5 variables:
#$ MapPointID  : int  1 2 3 4 5 6 7 8 9 11 ...
#$ MapPointName: Factor w/ 29 levels "bab_to_med","bab_to_wren",..: 22 27 11 8 25 12 5 7 18 15 ...
#$ MapPointType: Factor w/ 3 levels "hub","waterstop",..: 1 2 2 1 2 1 2 2 1 2 ...
#$ lat         : num  42.1 42.1 42.1 41.9 41.8 ...
#$ lon         : num  -72.1 -71.7 -71.4 -71.2 -70.7 ...


#drop the na record (all nas but in this case only the one)
mappoints <- na.omit(mappoints)
#cleanup the segments_mapoints
segments_mappoints[(segments_mappoints$MapPointID==21),]

segments_mappoints = segments_mappoints[!(segments_mappoints$MapPointID==21),]



str(mappoints)
#'data.frame':	198 obs. of  5 variables:
#$ MapPointID  : int  1 2 3 4 5 6 7 8 9 11 ...
#$ MapPointName: Factor w/ 29 levels "bab_to_med","bab_to_wren",..: 22 27 11 8 25 12 5 7 18 15 ...
#$ MapPointType: Factor w/ 3 levels "hub","waterstop",..: 1 2 2 1 2 1 2 2 1 2 ...
#$ lat         : num  42.1 42.1 42.1 41.9 41.8 ...
#$ lon         : num  -72.1 -71.7 -71.4 -71.2 -70.7 ...


# and now cleanup the segments_mappoints


mappoints %>% 
  group_by(MapPointType) %>% 
  summarise(map_point_type_count = n())

#  MapPointType map_point_type_count
#<fct>                       <int>
#1 hub                             6
#2 waterstop                       9
#3 waypoint                      183

# so a few hubs, a few water stops and lot of way points


# lets dig a little deper into the mappoints
# do we have any that are orphaned?
# we know about 21, we just orphaned it due to the NA
# but are there any other that need investigating?

anti_join(mappoints, segments_mappoints) %>% filter(MapPointType=="waypoint")
#MapPointID  MapPointName MapPointType     lat       lon
#1       1740 wren_to_lunch     waypoint 41.9439 -71.22081

# interesting, a mappoint not listed in the segments_mappoints table
# lets see what other map points with same name
mappoints %>% filter(MapPointName=="wren_to_lunch") %>% arrange(MapPointID)
# there are plenty of map points with same name,
# so shouldnt be harm in dropping this orphan.  if i needed the data i would have to
# do some analysis, 
#  whats the nearest point to this one?
#  does it belong in the wren_to_lunch route? if so where does it fit?
# its actually mapointid 174 typoed to 1740, and you see in the data the ids jump from 173 to 175
#  todo: circle back later and do the distance calse to see whats what


mappoints %>% nrow()
#[1] 198
mappoints = mappoints[!(mappoints$MapPointID==1740),]
mappoints %>% nrow()
#[1] 197


anti_join(mappoints, segments_mappoints) %>% filter(MapPointType=="waypoint")
#Joining, by = "MapPointID"
#[1] MapPointID   MapPointName MapPointType lat          lon         
#<0 rows> (or 0-length row.names)

# got rid of the nulls and the orphans from mappoints




##
## explore routes
##

routes

#RouteID RouteType                         RouteName                  RouteDescription
#1       1     2 Day          Sturbridge to P-Town Inn          Sturbridge to P-Town Inn
#2       3     2 Day              Sturbridge to Babson              Sturbridge to Babson
#3       4     2 Day                  Babson to P-Town                  Babson to P-Town
#4       6     1 Day                 Sturbridge to MMA                 Sturbridge to MMA
#5       7     1 Day                     Babson to MMA                     Babson to MMA
#6       8     1 Day                     MMA to Babson                     MMA to Babson
#7       9     1 Day Babson to Patriot Place to Babson Babson to Patriot Place to Babson
#8      10     1 Day           Babson to Patriot Place           Babson to Patriot Place
#9      11     3 Day            MA/NY Border to P-Town              Sturbridge to Babson

# occular pat down - no NA(s)

#are there any routes with no segments, basically a router header with no detail?

anti_join(routes, routes_segments)
#Joining, by = "RouteID"
#RouteID RouteType              RouteName     RouteDescription
#1      11     3 Day MA/NY Border to P-Town Sturbridge to Babson

# sure enough, i added this one in as a bogus record for cleaning up
# there is a PMC Team that rides from the New York border on the Friday before the official PMC weekend

# Team Huckleberry
# http://profile.pmc.org/TH0207


# so they turn a 2 day event into a 3 day event for themselves and make it a TRUE pan mass ride
# okay, so i have offically paid my homage to team Huckleberry, but the route is unknown, lets get it out of the data




routes %>% nrow()
#[1] 9
routes <- routes[!(routes$RouteName=="MA/NY Border to P-Town"),]
routes %>% nrow()
#[1] 8

# gone baby, gone




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

# occular pat down - clear for passage - no NA(s)

# any funny business here with related data


# where does the column SegmentID come up in our data
"SegmentID" %in% colnames(mappoints)
#[1] FALSE
"SegmentID" %in% colnames(routes)
#[1] FALSE
"SegmentID" %in% colnames(routes_segments)
#[1] TRUE
"SegmentID" %in% colnames(segments)
#[1] TRUE
"SegmentID" %in% colnames(segments_mappoints)
#[1] TRUE

# we just did the occular patdown on the segments data, it clean, lets check the other for bad relationships


anti_join(segments_mappoints, segments, joinby="SegmentID")
#Joining, by = "SegmentID"
#[1] SegmentID               SegmentMapPointSequence MapPointID             
#<0 rows> (or 0-length row.names)
#clean

anti_join(routes_segments, segments, joinby="SegmentID")
#Joining, by = "SegmentID"
#[1] RouteID              RouteSegmentSequence SegmentID           
#<0 rows> (or 0-length row.names)
#clean!



anti_join(segments, segments_mappoints, joinby="SegmentID")
#Joining, by = "SegmentID"
#[1] SegmentID          SegmentName        SegmentDescription Day               
#<0 rows> (or 0-length row.names)

anti_join(segments, routes_segments, joinby="SegmentID")
#Joining, by = "SegmentID"
#[1] SegmentID          SegmentName        SegmentDescription Day               
#<0 rows> (or 0-length row.names)


##
## explore routes_segments
##

nrow(routes_segments)
#[1] 20

routes_segments %>% head()
#  RouteID RouteSegmentSequence SegmentID
#1       1                    1         1
#2       1                    2         2
#3       1                    3         4
#4       3                    1         1
#5       3                    2         2
#6       3                    3         7

# we just gave the table a check against segments, so thats good, what about routes?
# and we already checked routes to be sure routes missing segments were removed
# do we have any route_segments referring to an unknown route?
anti_join(routes_segments, routes)
#  RouteID RouteSegmentSequence SegmentID
#1      12                    1         1

# yes, a bogus record, the is no route 12, never has been
# lets do a quick check, is segment id = 1 in play on another segment?

routes_segments %>% filter(SegmentID==1)
#  RouteID RouteSegmentSequence SegmentID
#1       1                    1         1
#2       3                    1         1
#3       6                    1         1
#4      12                    1         1

left_join(routes_segments, routes, by="RouteID") %>% 
  filter(SegmentID==1) %>% 
  select(RouteID, RouteSegmentSequence, SegmentID, RouteName)

left_join(routes_segments, segments, by="SegmentID") %>% 
  filter(SegmentID==1) %>% 
  select(RouteID, RouteSegmentSequence, SegmentID, SegmentName)


# i believe the RouteID 12 is bogus
# we have checked Routes, and it does not exist
# where does the column SegmentID come up in our data
"RouteID" %in% colnames(mappoints)
#[1] FALSE
"RouteID" %in% colnames(routes) # already chedked this table - does not exust
#[1] TRUE
"RouteID" %in% colnames(routes_segments) # this is the table we know we have the orphaned RouteID12
#[1] TRUE
"RouteID" %in% colnames(segments)
#[1] FALSE
"RouteID" %in% colnames(segments_mappoints)
#[1] FALSE

# the RouteID column does not exist in any other data frame, we have done our due diligence


routes_segments %>% nrow()
#[1] 20
routes_segments <- routes_segments[!(routes_segments$RouteID==12),]
routes_segments %>% nrow()
#[1] 19





## TODO: resume here on exploring the alt_data










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


