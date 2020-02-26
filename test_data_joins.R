# philip walsh
# philipwalsh.ds@gmail.com
# 2020-02-26
# join data and see what it looks like



library(tidyverse)

routes <- read.csv("data/routes.csv", header = T)
routes %>% head(3)

segments <- read.csv("data/segments.csv", header = T)
segments %>% head(3)

route_segment <- read.csv("data/route_segment.csv", header = T)
route_segment %>% head(3)

waterstops <- read.csv("data/waterstops.csv", header = T)
waterstops %>% head(3)

segment_waterstop <- read.csv("data/segment_waterstop.csv", header = T)
segment_waterstop %>% head(3)


# routes > route_segment > segments > segment_waterstop > waterstops

route_segments <- full_join(routes, route_segment)


View(route_segments)

segment_waterstops = full_join(route_segments, segment_waterstop)

route_waterstops = full_join(segment_waterstops, waterstops)

View(route_waterstops)



