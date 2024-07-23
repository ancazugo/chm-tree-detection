# devtools::install_github("TESS-Laboratory/chmloader")

# Libraries ---------------------------------------------------------------

library(here)
library(chmloader)
library(lidR)
library(terra)

source("constants.R")

# Processing --------------------------------------------------------------

cambridge_chm_mem <- rast(ncol = ncol(cambridge_chm), nrow = nrow(cambridge_chm),
          crs = crs(cambridge_chm), ext = ext(cambridge_chm))
values(cambridge_chm_mem) <- as.matrix(values(cambridge_chm))

f <- function(x) {
    y <- 2.6 * (-(exp(-0.08*(x-2)) - 1)) + 15
    y[x < 2] <- 15
    y[x > 20] <- 25
    return(y)
}

# out <- sapply(chm_tiles[1:50], \(tile) {
#     
#     x <- rast(tile)
#     taos <- locate_trees(x, lmf(f), uniqueness = 'bitmerge')
#     vect(taos)
# })
# 
# out <- vect(out) |> 
#     st_as_sf()
# 
# out$treeID <- 1:length(out$treeID)

ttops_cam <- locate_trees(cambridge_chm, lmf(f), uniqueness = 'bitmerge')

algo <- dalponte2016(cambridge_chm_mem, ttops_cam)
result <- algo()
crowns <- as.polygons(result)

writeVector(crowns, here(VECTOR_OUTPUT_DIR, "meta_chm_trees", "meta_cambridge_crowns.gpkg"), overwrite = T)
