ext_list <- function(a_ext, nstep) {
    vec = vector('list', length = nstep)
    names(vec) = as.character(a_ext)
    return(vec)
}

ext_ref <- function() {
    # Add reference lines for extinction
    aa = seq(0,1,0.01)
    sar_f <- function(x, z) {1-(1-x)^z}
    nosar_f <- function(x, N) {-log(1-x)/log(N)}
    df1 <- lapply(c(0.3,0.4,0.5), function(z) {
        data.frame(aa = aa, type = 'mar', param = z, dm = sar_f(aa, z)) %>%
            dplyr::mutate(id = paste(type, param))
    })
    df2 <- lapply(c(1e+4,1e+9), function(N){
        data.frame(aa = aa, type = 'nomar', param = N, dm = nosar_f(aa, N)) %>%
            dplyr::mutate(id = paste(type, param))
    })
    output = rbind(dplyr::bind_rows(df1), dplyr::bind_rows(df2))
    output$dm[is.infinite(output$dm)] <- 1
    return(output)
}


MARfastEXT <- function(coord, geno, rasterco, extstep, type = 'random') {
    # get starting mutation values
    M_all = calc_M(geno)
    # get starting number of rasters
    A_all = raster::cellStats(!is.na(rasterco), 'sum')
    # dictionary from raster cellid to individualids
    raster2inds = raster::cellFromXY(rasterco, coord)
    # set up list to store the cells and individual extincted at each step
    a_ext = seq(0,100,by=extstep)
    nstep = length(a_ext)
    extinds = ext_list(a_ext, nstep)
    extcells = ext_list(a_ext, nstep)
    # set up data frame to store the mutations
    extdf = data.frame(a_ext_in = a_ext, m_ext = NA, a_ext = NA)

    for (a in a_ext) {
        # toextsize = the current habitat - the expected habitat with a_ext
        toextsize = raster::cellStats(!is.na(rasterco), 'sum') - A_all*(100-a)/100
        if (toextsize > 0) {
            toext = raster::sampleRandom(rasterco, size = toextsize, cells = TRUE)[,'cell']
            extinds[[as.character(a)]] = cell2inds(raster2inds, toext)
            extcells[[as.character(a)]] = toext
            rasterco[toext] <- NA # operate on the raster to run extinction
        }
        # calculate leftover individuals by unpacking extinct individuals
        reminds = setdiff(1:nrow(geno), unlist(unname(extinds)))
        M_left = calc_M(geno[reminds, ])
        A_left = raster::cellStats(!is.na(rasterco), 'sum')
        extdf[which(extdf$a_ext_in == a), 'm_ext'] = 1-M_left/M_all
        extdf[which(extdf$a_ext_in == a), 'a_ext'] = 1-A_left/A_all
    }

    output = list(extinds,extcells,extdf)
    names(output) = c('extinds', 'extcells', 'extdf')
    return(output)
}


