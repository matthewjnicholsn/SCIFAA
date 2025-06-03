library(haven)
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(tidyr)

#read in data
HR <- read_dta("") #load in household microdata
SF <- read_sf("") #load in country shapefile with state-level data
RS <- raster("") #load in raster of desired spatial covariate

