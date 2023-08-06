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
    output = data.frame(
        aa = aa,
        sar_m = sar_f(aa, 0.3),
        nosar_m1 = nosar_f(aa, 1e+4),
        nosar_m2 = nosar_f(aa, 1e+9)
    )
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
    extdf = data.frame(a_ext = a_ext, m_ext = NA)

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
        extdf[which(extdf$a_ext == a), 'm_ext'] = 1-M_left/M_all
    }

    output = list(extinds,extcells,extdf)
    names(output) = c('extinds', 'extcells', 'extdf')
    return(output)
}


extcells = unlist(unname(EXTdata()$extcells[1:which(names(EXTdata()$extcells) == as.character(input$a_ext))]))
extdf = EXTdata()$extdf %>%
    dplyr::filter(a_ext <= input$a_ext) %>%
    dplyr::mutate(pera_ext = a_ext/100)

