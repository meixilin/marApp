library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(markdown) # to fix deployment issues

library(leaflet)
library(DT)
library(plotly)

library(ggplot2)
library(dplyr)

library(terra)
library(mar)

sidewidth <- 300
smallcrs <- "EPSG:4326" # WGS84 longitude/latitude
largecrs <- "EPSG:8857" # Equal Earth Greenwich
spanmax <- 10 # degrees

mode0_choices <- c("estimate loss", "build goals")
con_reports <- c("report_loss.Rmd", "report_goal.Rmd")
names(con_reports) <- mode0_choices

Mchoices <- mar:::.Mtype
names(Mchoices) <- c("Number of mutations", "Number of endemic mutations", "Watterson's theta", "Nucleotide diversity")
Achoices <- mar:::.Atype
names(Achoices) <- c("Area of cells (km^2)", "Area of squares (km^2)", "Number of individuals sampled")
Achoices_ext <- Achoices[c("Area of cells (km^2)", "Number of individuals sampled")]

options(shiny.maxRequestSize = 30 * 1024^2) # maximum 30 MB upload

theme_set(theme_bw(base_size = 10))

quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
}

# bold text
bt <- function(x) {
    shiny::p(shiny::strong(x))
}

# get name by value
get_name <- function(vec, x) {
    names(vec[vec == x])
}

# run a file-parsing expression and surface a friendly validation message
# (instead of a raw crash) if the file does not match the expected format
safe_parse <- function(expr, filelabel) {
    result <- tryCatch(expr, error = function(e) e)
    shiny::validate(shiny::need(
        !inherits(result, "error"),
        paste0(
            "Failed to parse the ", filelabel, ". Please check that it follows the ",
            "format described above. Details: ",
            if (inherits(result, "error")) conditionMessage(result) else ""
        )
    ))
    return(result)
}

# read a coordinate file, picking the map projection from how far the samples
# spread. Uploaded coordinates are degrees (EPSG:4326), so the span is read
# straight off the file before any map is built, and the equal-area projection is
# used only when the samples spread widely enough for the degree grid to distort
# area.
lonlat_parser_autocrs <- function(lonlat.fn) {
    lonlatdf <- mar:::.read_lonlat(lonlat.fn)
    spans <- apply(as.matrix(lonlatdf[, 2:3]), 2, function(xx) diff(range(xx)))
    mapcrs <- if (any(spans > spanmax)) largecrs else smallcrs
    mar::lonlat_parser(lonlat.fn, mapres = NULL, mapcrs = mapcrs)
}

# marmaps stores coordinates in the map CRS (e.g. EPSG:8857 for gm1001g), but
# leaflet markers must be given longitude/latitude degrees. Reproject onto
# EPSG:4326 using the CRS carried by the samplemap raster. Returns NULL when the
# coordinates sit on an arbitrary plane (empty CRS), so callers can skip markers.
marmaps_lonlat_wgs84 <- function(maps) {
    samplemap <- mar:::.get_samplemap(maps) # force before S4 dispatch below
    mapcrs <- terra::crs(samplemap)
    if (is.na(mapcrs) || !nzchar(mapcrs)) {
        return(NULL)
    }
    pts <- terra::vect(maps$lonlat, type = "points", crs = mapcrs)
    xy <- terra::geom(terra::project(pts, "EPSG:4326"))[, c("x", "y"), drop = FALSE]
    data.frame(ID = maps$sample.id, LON = xy[, "x"], LAT = xy[, "y"])
}
