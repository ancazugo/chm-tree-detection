
library(raster)
library(terra)
library(lidR)
library(tidyterra)
library(sf)
library(tidyr)
library(dplyr)
library(ggplot2)

source("constants.R")

## Data Paths

city <- 'London'
lidar_dir <- here(POINT_CLOUD_INPUT_DIR, "EDINA", city)