library(dplyr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
# library(shinydashboardPlus)
library(leaflet)
# library(mar) # cannot load this package in shinyapps
library(DT)
library(ggplot2)
library(plotly)
library(sars)
library(raster)
library(maps)

sidewidth = 300
nsnps = 1000
extstep = 1
myseed = 123
options(shiny.maxRequestSize=30*1024^2) # maximum 30 MB upload

theme_set(theme_bw(base_size = 12))

helper_files = list.files('R', full.names = TRUE)
sapply(helper_files, source)

