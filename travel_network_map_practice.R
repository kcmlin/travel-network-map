#######################################
# Practice Mapping Travel Netowrks using ggplot2
#
# Katherine Schaughency
# 19 Jun 2023
# ----------------------------------- #
# Reference: 
# https://www.r-bloggers.com/2018/05/three-ways-of-visualizing-a-graph-on-a-map/
# https://ggrepel.slowkow.com/articles/examples.html#do-not-repel-labels-from-data-points-1
#######################################


# --------------------------------- #
# load R package

library(assertthat)
library(dplyr)
library(purrr)
library(igraph)
library(ggplot2)
library(ggraph)
library(ggmap)
library(ggpubr)
library(ggrepel)


# --------------------------------- #
# For countries that I have been,
# create a dataset with geo-coordinates (country centroids; latitude/longitude). 
# These centroids will become nodes on the map.

nodes <- read.delim(text = " 1   120.960515   23.69781  Taiwan
                             2   127.766922   35.90775  Korea
                             3   138.252924   36.20482  Japan
                             4   104.195397   35.86166  China
                             5   100.992541   15.87003  Thailand
                             6   103.819836    1.35208  Singapore
                             7    90.356331   23.68499  Bangladesh
                             8    84.124008   28.39485  Nepal
                             9    78.962880   20.59368  India
                            10   174.885971  -40.90055  'New Zealand'
                            11    40.489673    9.14500 Ethiopia
                            12    14.550072   47.51623 Austria
                            13    10.451526   51.16569 Germany
                            14     8.227512   46.81818 Switzerland
                            15     5.291266   52.13263 Netherlands
                            16     4.469936   50.50388 Belgium
                            17     6.129583   49.81527 Luxembourg
                            18    12.567380   41.87194 Italy
                            19     2.213749   46.22763 France
                            20    -3.435973   55.37805 UK
                            21   -19.020835   64.96305 Iceland
                            22   -95.712891   37.09024 USA
                            23  -106.346771   56.13036  Canada
                            ", 
                    header = FALSE,
                    quote = "'", sep = "",
                    col.names = c('id', 'lon', 'lat', 'name'))


# --------------------------------- #
# Create connections (edges) between our nodes.
# These connections represent the to/from locations of each trip
#
# visited 1 time  : assigned weight 0.6
# visited 2 times : assigned weight 0.75
# visited 3 times : assigned weight 0.8
# visited 4 times : assigned weight 0.9
# visited 5 times : assigned weight 1.0


edges <- read.delim(text = " 1  3 0.80 'Solo Trip(s) + Trip(s) with Family'
                            22  3 0.60 'Trip(s) with Family'
                            22  2 0.60 'Solo Trip(s)'
                             1  4 0.80 'Solo Trip(s) + Trip(s) with Family'
                             1  5 0.60 'Trip(s) with Family'
                             1  6 0.75 'Solo Trip(s) + Trip(s) with Family'
                            22  7 0.60 'Solo Trip(s)'
                             7  8 0.60 'Solo Trip(s)'
                             1  9 0.60 'Trip(s) with Family'
                            22  10  0.60 'Trip(s) with Family'
                            22  11  0.60 'Solo Trip(s)'
                             1  13  0.75 'Solo Trip(s)'
                            13  12  0.60 'Solo Trip(s)'
                            12  14  0.60 'Solo Trip(s)'
                            14  17  0.60 'Solo Trip(s)'
                            17  16  0.60 'Solo Trip(s)'
                            16  15  0.60 'Solo Trip(s)'
                            15  13  0.60  'Solo Trip(s)'
                            22  18  0.60 'Trip(s) with Family'
                             1  20  0.60 'Trip(s) with Family'
                            20  19  0.60 'Trip(s) with Family'
                            19  1 0.60 'Trip(s) with Family'
                            22  21  0.60 'Trip(s) with Family'
                             1  22  1.00 'Solo Trip(s) + Trip(s) with Family'
                            22  1 0.90 'Solo Trip(s) + Trip(s) with Family'
                            22  23  0.80 'Solo Trip(s) + Trip(s) with Family'
                    ", 
                    header = FALSE,
                    quote = "'", sep = "",
                    col.names = c('from', 'to', 'weight', 'category')) %>% 
  
             mutate(category = as.factor(category))


# --------------------------------- #
# Create a graph structure (nodes, edges) using the igraph library. 
# This is necessary for fast calculation of the degree or other properties of each node later.

g <- graph_from_data_frame(edges, 
                           directed = FALSE, 
                           vertices = nodes)


# --------------------------------- #
# Create a data structure for all plots.
# Build from the edges dataset and include the centroid lat/log of the to and from countries

edges_for_plot <- edges %>%
  inner_join(nodes %>% select(id, lon, lat), by = c('from' = 'id')) %>%
  rename(x = lon, y = lat) %>%
  inner_join(nodes %>% select(id, lon, lat), by = c('to' = 'id')) %>%
  rename(xend = lon, yend = lat)

assert_that(nrow(edges_for_plot) == nrow(edges))


# --------------------------------- #
# Use "degree" metric (how many times a country is connected through either "to" or "from" location) 
# to give each node (country) a weight. The weight will reflect by the node size in the graph 

# preview data
nodes
degree(g)

# assign weight (size of a node)
nodes$weight <- degree(g)


# --------------------------------- #
# Create an overall map
#     NOTE: For each geom, define aesthetic mappings 
#           that “describe how variables in the data are mapped to visual properties” in the plot 



  
  # read visited country list with coordinates and weights
  ggplot(nodes) + 
  
  # draw country polygons from the world map using map_data('world') (geom_polygon)
  geom_polygon(aes(x = long, y = lat, group = group),
               data = map_data('world'),
               fill = "#CECECE", color = "#515151",
               linewidth = 0.15) +
  
  # draw edges between nodes as arcs (geom_curve)
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend,    
                 color = category, 
                 size = weight),
             data = edges_for_plot, curvature = 0.33,
             alpha = 0.8) +
  
  # scale for edge widths
  scale_size_continuous(guide = "none", range = c(0.25, 2)) + 
  
  # draw nodes as points (geom_point)
  geom_point(aes(x = lon, y = lat, size = weight),           
             shape = 21, fill = 'white',
             color = 'black', stroke = 0.5) +
  
  # scale for node size
  scale_size_continuous(guide = "none", range = c(1, 6)) +   
  
  # write text labels and allow labels to shift its position
  geom_text_repel(aes(x = lon, y = lat, label = name), 
                  
                  # text label
                  size = 4,                   # font size
                  
                  color = "white",            # text and label border color
                  
                  fontface = "bold",          # “plain”, “bold”, “italic”, “bold.italic”
                  
                  box.padding = 0.5,          # Amount of padding around bounding box, as unit or number. Defaults to 0.25. (Default unit is lines, but other units can be specified by passing unit(x, "units")).
                  
                  # overlap
                  max.overlaps = Inf,         # Some text labels will be discarded if they have too many overlaps.
                                              # "Inf" to override this behavior and always show all labels.
                  
                  force = 2,                  # Force of repulsion between overlapping text labels. Defaults to 1.
                  
                  # move labels from edges of the plot
                  xlim = c(NA, NA),           # Repel away from the left and right edges
                  ylim = c(NA, NA),           # Repel away from the top and bottom edges
                  
                  # Expand the scale to make room for labels
                  # (The units for nudge_x and nudge_y are the same as for the data units on the x-axis and y-axis.)
                  
                  nudge_x = 2,                # Horizontal adjustments to nudge the starting position of each text label. 
                  
                  nudge_y = 4,                # Vertical adjustments to nudge the starting position of each text label. 
                  
                  direction = "y",            # move text labels “both” (default), “x”, or “y” directions
                  
                  # Make curved line segments
                  segment.curvature = -0.1,   # positive values would increases right-hand curvature, 
                                              # negative values would increase left-hand curvature, 
                                              # 0 makes straight lines
                  
                  segment.ncp = 3,            # gives 3 control points for the curve
                  
                  segment.angle = 20,         # 0-180, less than 90 skews control points toward the start point
                  
                  segment.color = "white",    # line segment color
                  
                  segment.inflect = FALSE,    # FALSE = curve inflection at the midpoint
                  
                  segment.square = TRUE       # TRUE to place control points in city-block fashion, 
                                              # FALSE for oblique placement
                  ) +
  
  # set the same fixed ratio coordinate system that specifies the limits of the longitude and latitude coordinates.
  coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80)) +
  
  # apply map theme
  theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))

