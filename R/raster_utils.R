

coord2raster <- function(coord, res = 1, padding = 0.01) {
    # create edges of the raster
    lonrange <- range(coord[, 1]) + c(-1, 1) * diff(range(coord[, 1])) * padding # range coords ± 1% of study area (accomodate resolution errors)
    latrange <- range(coord[, 2]) + c(-1, 1) * diff(range(coord[, 2])) * padding # range coords ± 1% of study area
    #  create raster
    baser <- raster(resolution = res,
                    xmn = lonrange[1], xmx = lonrange[2],
                    ymn = latrange[1], ymx = latrange[2])
    rr <- rasterize(coord, baser, fun = "count")
    print(rr)
    return(rr)
}

cell2inds <- function(r2i, cells) {
    return(which(r2i %in% cells))
}



