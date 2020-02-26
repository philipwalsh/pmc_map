# philip walsh
# philipwalsh.ds@gmail.com
# 2020-02-26
# create a test pmc map with routes plotted


library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)  






map_state = "massachusetts"

#map_county = "bristol"

# load all map data
states <- map_data("state")
states %>% head(3)

# filter out only mass
single_state_df <- subset(states, region == map_state)
single_state_df %>% head(3)

# get all counties
counties <- map_data("county")
counties %>% head()

# fliter out a conties only for mass
state_counties <- subset(counties, region == map_state)
head(state_counties)

# get only a single county
#single_county <- subset(state_counties, subregion==map_county)
#single_county %>% head()

#state_base <- ggplot(data = single_state_df, mapping = aes(x = long, y = lat, group = group)) +
#  coord_fixed(1.3) +
#  geom_polygon(color = "black", fill = "gray")


day_1_waterstops <- read.csv("day_1_waterstops.csv", header = T)
head(day_1_waterstops)
tail(day_1_waterstops)


state_base <- ggplot(data = single_state_df, mapping = aes(x = long, y = lat)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray")

state_base + theme_void() +
  geom_polygon(data = state_counties, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA) +
  #geom_line(data = day_1_waterstops, aes(lon,lat), color="red", size=1)+
  geom_point(data = day_1_waterstops, aes(lon,lat), color="blue", size=2)+
  geom_text(data = day_1_waterstops,aes(x=lon, y=lat, label=WaterstopName), vjust=-1)


