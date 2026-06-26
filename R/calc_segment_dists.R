library(qs2)
library(sf)
library(tidyverse)
library(cppRouting)

tcl <- read_sf("data/raw/Centreline - Version 2 - 2952.gpkg")
segments <- read_sf("data/raw/bluetooth-travel-time-segments-wgs84")




