
# Libraries ---------------------------------------------------------------

library(dplyr)
library(stringr)
library(sf)
library(terra)
library(lidR)

source("constants.R")

# Variables ---------------------------------------------------------------

city_name <- 'London'

# Data Paths --------------------------------------------------------------

vom_dir <- here(RASTER_INPUT_DIR, "DEFRA", "VOM", city_name)

tif_paths <- list.files(vom_dir, pattern = "\\.tif$", 
                        full.names = T, recursive = T)


hillshade_paths <- tif_paths[str_detect(tif_paths, 'VOM_HS')]
chm_paths <- tif_paths[!str_detect(tif_paths, 'VOM_HS')]

# TQ27nw/2075 from 2018 doesn't have a chm tile
# TQ16se/1560 is invalid format
# chm_paths |> str_extract("[A-Z][A-Z][0-9][0-9][0-9][0-9]")
# hillshade_paths |> str_extract("[A-Z][A-Z][0-9][0-9][0-9][0-9]")

# CHM Processing with lidR ------------------------------------------------

safe_rast <- function(file) {
    tryCatch(
        {
            r <- rast(file)
            return(r)
        },
        error = function(e) {
            message("Error reading file: ", file)
            message("Error message: ", e$message)
            return(NULL)
        },
        warning = function(w) {
            message("Warning when reading file: ", file)
            message("Warning message: ", w$message)
            return(NULL)
        }
    )
}

# Read all valid tif files
valid_rasters <- list()
for (file in chm_paths) {
    r <- safe_rast(file)
    if (!is.null(r)) {
        valid_rasters[[length(valid_rasters) + 1]] <- r
    }
}

# Combine valid rasters into a single SpatRaster object
chm_spat_rast <- merge(sprc(valid_rasters[1:16]))

kernel <- matrix(1,3,3)
chm_smoothed <- focal(chm_spat_rast, w = kernel, fun = median, na.rm = T)

vrt_file <- tempfile(fileext = ".vrt")
terra::writeRaster(chm_smoothed, vrt_file, filetype = "VRT", overwrite = TRUE)

# Read the VRT file as a RasterLayer (which lidR can work with)
chm_smoothed_raster <- raster::raster(vrt_file)

ttops_chm_smoothed <- locate_trees(chm_smoothed, lmf(5))
