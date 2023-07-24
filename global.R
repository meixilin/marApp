library(dplyr)
library(shinydashboard)
library(leaflet)
# library(mar) # cannot load this package in shinyapps
library(DT)
library(ggplot2)
library(plotly)
library(sars)

sidewidth = 350

theme_set(theme_bw(base_size = 12))

helper_files = list.files('R', full.names = TRUE)
sapply(helper_files, source)

