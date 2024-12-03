library(shiny)
library(shinydashboard)
library(shinyWidgets)

library(sads)
library(SeqArray)
library(raster)
library(mar)

library(dplyr)
library(leaflet)
library(DT)
library(ggplot2)
library(plotly)
library(maps)

sidewidth = 300
nsnps = 1000
extstep = 1
myseed = 123
mycrs = "+proj=longlat +datum=WGS84"
sadchoices = mar:::.sad_models
names(sadchoices) = c("Broken stick", "Geometric", "Lognormal", "Log-series", "Neutral metacommunity", "Weibull")
Mchoices = mar:::.Mtype
names(Mchoices) = c("Segregating sites", "Endemic segregating sites", "Watterson's theta", "Nucleotide diversity")
Achoices = mar:::.Atype
names(Achoices) = c("Area of cells (km^2)", "Area of squares (degree^2)")

options(shiny.maxRequestSize=30*1024^2) # maximum 30 MB upload

theme_set(theme_bw(base_size = 12))

helper_files = list.files('R', full.names = TRUE)
sapply(helper_files, source)

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



