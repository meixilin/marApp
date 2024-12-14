library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(markdown) # to fix deployment issues

library(leaflet)
library(DT)
library(plotly)

library(ggplot2)
library(dplyr)
library(reshape2)

# library(sads)
library(SeqArray)
library(sf) # Need this for crs() call to work
# library(raster) # Had to be commented out otherwise cannot load package
library(mar)

sidewidth = 300
mycrs = "+proj=longlat +datum=WGS84"

sadchoices = mar:::.sad_models
names(sadchoices) = c("Broken stick", "Geometric", "Lognormal", "Log-series", "Neutral metacommunity", "Weibull")
Mchoices = mar:::.Mtype
names(Mchoices) = c("Segregating sites", "Endemic segregating sites", "Watterson's theta", "Nucleotide diversity")
Achoices = mar:::.Atype
names(Achoices) = c("Area of cells (km^2)", "Area of squares (degree^2)")

options(shiny.maxRequestSize=30*1024^2) # maximum 30 MB upload

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



