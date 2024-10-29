
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
vom_crowns_dir <- here(VECTOR_OUTPUT_DIR, "DEFRA", "crowns", city_name)

tif_paths <- list.files(vom_dir, pattern = "\\.tif$", 
                        full.names = T, recursive = T)

# hillshade_paths <- tif_paths[str_detect(tif_paths, 'VOM_HS')]
chm_paths <- tif_paths[!str_detect(tif_paths, 'VOM_HS')]

crowns_path <- here(vom_crowns_dir, paste0(city_name,'_crowns.gpkg'))

# TQ27nw/2075 from 2018 doesn't have a chm tile (changed for 2021)
# TQ16se/1560 from 2018 is invalid format (changed for 2021)

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
print('Merging Rasters')
# Combine valid rasters into a single SpatRaster object
chm_spat_rast <- merge(sprc(valid_rasters))

kernel <- matrix(1,3,3)
chm_smoothed <- focal(chm_spat_rast, w = kernel, fun = median, na.rm = T)
current_crs <- terra::crs(chm_smoothed)

chm_smoothed_rast <- raster::raster(chm_smoothed)
terra::crs(chm_smoothed_rast) <- current_crs

f <- function(x) {
    y <- 2.6 * (-(exp(-0.08 * (x - 2)) - 1)) + 3
    y[x < 2] <- 3
    y[x > 20] <- 5
    return(y)
}

print('Locating Trees')
ttops_chm_smoothed <- locate_trees(chm_smoothed, lmf(f))
ttops_chm_smoothed_spat_vect <- vect(ttops_chm_smoothed)
names(ttops_chm_smoothed_spat_vect)[2] <- 'height'

print('Segmenting Trees')
algo <- dalponte2016(chm_smoothed_rast, ttops_chm_smoothed)
crowns <- algo()
crowns_spat_rast <- as(crowns, 'SpatRaster')

crowns_vect <- as.polygons(crowns_spat_rast)
names(crowns_vect) <- 'treeID'

print('Saving Crowns')
crowns_vect <- merge(crowns_vect, ttops_chm_smoothed_spat_vect, by = 'treeID')
crowns_vect$area <- expanse(crowns_vect, unit = "m")
writeVector(crowns_vect, crowns_path, overwrite = T)
