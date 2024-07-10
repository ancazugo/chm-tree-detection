
# Libraries ---------------------------------------------------------------

library(here)
library(chmloader)
library(sf)
library(terra)

source("constants.R")

# Paths -------------------------------------------------------------------

cambridge_boundary_path <- here(VECTOR_INPUT_DIR, "cambridge_boundaries/city_outline.shp")
meta_chm_dir <- here(RASTER_INPUT_DIR, "meta_chm", "tiles")
meta_chm_tile_path <- here(meta_chm_dir, "cam_tile_.tif")

cambridge_boundary_sf <- read_sf(cambridge_boundary_path) |> 
    st_transform(3857)

cambridge_chm <- download_chm(
    cambridge_boundary_sf,
    filename = tempfile(fileext = ".tif")
)

terra::plot(cambridge_chm, col = hcl.colors(256, "viridis"))

chm_tiles <- makeTiles(cambridge_chm, c(1024, 1024), filename = meta_chm_tile_path)