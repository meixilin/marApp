# Title: Generate coral example input for marApp
# Author: Meixi Lin
# Date: Fri Jun 20 14:09:36 2025
# Before running this script, please download and unzip the data from
# https://zenodo.org/records/14887583.
# In the data-raw folder there should be a folder named `CoralGenomicVulnerability_Data_and_Scripts`
# containing the data downloaded from the above Zenodo record.

# preparation --------
rm(list = ls())
cat("\014")
options(echo = TRUE, stringsAsFactors = FALSE, width = 120)

sessionInfo()

setwd("data-raw/")

# def functions --------

# def variables --------
indir = 'CoralGenomicVulnerability_Data_and_Scripts/R/'
outdir = 'coral_example/'

dir.create(outdir)

Datasets = c('Cooke','Drury','Matz','Selmoni','Shinzato','Torquato')

# Use the florida example
D = 'Drury'

# load data --------
# sample metadata
load(paste0(indir,'/8_Filter_GT_matrix/8_samples_meta_filtered_manual.rda'))

# genotype
load(paste0(indir,'/7_Prepare_genomic_data/', D,'_GTO.rda'))

# main --------
# sample subsetting
META = samples_meta_filtered_manual[samples_meta_filtered_manual$Dataset_AF==D,]
### Create a coordinate matrix
coords = as.matrix(cbind(META$LON, META$LAT))

# genotype cleanup
GTS = GTO[,colnames(GTO) %in% META$ID]
dim(GTS)
table(GTS, useNA = 'always')

# filter for 20% missingness as the previous paper
mnSNP = apply(GTS, 1, function(x) {mean(is.na(x))})
GTS = GTS[mnSNP < 0.20, ]
dim(GTS)
# fillin the NA sites as 0 after 20% missingness filter
GTS[is.na(GTS)] = 0
# remove invariant sites
GTS = GTS[apply(GTS, 1, function(x) {!all(x == 0)}), ]
GTS = GTS[apply(GTS, 1, function(x) {!all(x == 2)}), ]

# check if it is a valid genotype
mar:::.valid_genotype(GTS, ploidy = 2)
dim(GTS)

# check that META sample ids matched GTS
all(colnames(GTS) == META$ID)
# sanitize sample id as it cannot be inputted in marApp
META$ID = 1:nrow(META)

# cleanup --------
write.csv(META[,c('ID', 'LON', 'LAT')], file = paste0(outdir, "coords.csv"), quote = FALSE, row.names = FALSE)
write.table(GTS, file = paste0(outdir, "genome.txt"), quote = FALSE, sep = '\t', row.names = FALSE, col.names = FALSE)

