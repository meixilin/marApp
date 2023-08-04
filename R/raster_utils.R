

coord2raster <- function(coord, res = 1) {
    # create edges of the raster
    xmn <- range(coord[, 1])[1]
    xmx <- range(coord[, 1])[2]
    ymn <- range(coord[, 2])[1]
    ymx <- range(coord[, 2])[2]
    #  create raster
    baser <- raster(resolution = res, xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx)
    rr <- rasterize(coord, baser, fun = "count")
    return(rr)
}

step_raster <- function(rr, prop = 0.1) {
    notna = cellStats(!is.na(rr), 'sum')
    stepsize = ceiling(notna*prop)
    return(stepsize)
}


