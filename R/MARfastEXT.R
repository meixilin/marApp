# calculate quick MAR extinction statistics
extinct_geno <- function(values) {
    # calculate the number of mutations that are in the remaining individuals
    reminds = setdiff(1:nrow(values$ggsub), values$extinds)
    subgeno = values$ggsub[reminds, ]
    M_left = calc_M(subgeno)
    mext = 1-M_left/values$M_all
    return(mext)
}




MARfastEXT <- function(coord, rasterco, prop = 0.1, type = 'random') {
    # assign raster cellid to the coord database
    rasterid
    # get the  present locations
    gridpresent <- which(apply(values(raster_mutmaps), 1, function(x) any(!is.na(x))) == TRUE)
    A <- length(gridpresent)
    Astart <- A
    xstep <- ceiling(xfrac * A)
    # iterate
    listres <- list()
    # calculate original diversity
    listres <- c(
        listres,
        list(mutdiv(raster_samples, raster_mutmaps, rest_mutmaps))
    )
    # extract the original rasterN
    rasterN <- listres[[1]]$rasterN
    while (A > 1) {
        # extinct some grids
        toextinct <- sample(gridpresent, xstep, replace = TRUE)
        values(raster_mutmaps)[toextinct, ] <- NA
        values(raster_samples)[toextinct] <- NA
        values(rest_mutmaps)[toextinct, ] <- NA
        # calculate diversity
        listres <- c(
            listres,
            list(mutdiv(raster_samples, raster_mutmaps, rest_mutmaps, rasterN))
        )
        # recalculate area remaining
        gridpresent <- which(apply(values(raster_mutmaps), 1, function(x) any(!is.na(x))) == TRUE)
        A <- A - xstep
    }
    res <- listres %>%
        do.call(rbind, .) %>%
        data.frame() %>%
        mutate(
            ax = 1 - (asub / max(asub, na.rm = T)),
            mx = 1 - (M / max(M, na.rm = T))
        )
}
