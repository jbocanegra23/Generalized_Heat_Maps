## Heat Density Mapping Product
# packages
library(devtools)
# ggmap has a couple bug problems with ggplot2 - need to use this version of ggmap
devtools::install_github("dkahle/ggmap")
library(readxl)
library(dplyr)
library(ggplot2)
library(maps)
library(ggmap)

density_map <- function(df, lon, lat, zoom = a, title = b) {
  left_bottom <- c(min(lon), min(lat))
  right_top <- c(max(lon), max(lat))
  map <- get_map(c(left_bottom, right_top), maptype = "roadmap", zoom)
  ggmap(map) + geom_density2d(data = df, 
                                     aes(x = lon, y = lat), size = 0.3) + 
    stat_density2d(data = df, aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                   bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
    scale_alpha(range = c(0, 0.3), guide = FALSE) + ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
}

alpha_density_map <- function(df, lon, lat, a, zoom, title) {
    left_bottom <- c(min(lon), min(lat))
    right_top <- c(max(lon), max(lat))
    Map <- get_map(location = c(left_bottom, right_top), maptype = "roadmap", zoom)
    ggmap(Map) + geom_point(aes(x = lon, y = lat), 
                              colour = "red",
                              alpha = a,
                              size = 1,
                              data = df) +
                ggtitle(title) +
               theme(plot.title = element_text(hjust = 0.5))
}


  
  
