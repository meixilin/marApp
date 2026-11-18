# Title: Examine how the estimates behave as the number of replicates increases in the Shiny app.
# Author: Meixi Lin
# Date: Tue Nov 10 05:54:08 2026

# preparation --------
rm(list = ls())
cat("\014")
options(echo = TRUE, stringsAsFactors = FALSE)

library(ggplot2)
library(dplyr)
library(cowplot)
library(mar)

theme_set(theme_cowplot(font_size = 11))

# def functions --------

# def variables --------
set.seed(123)
nrep = c(5, 10, 25, 50, 100)

# load data --------
maps <- lonlat_parser('coords.csv', mapcrs = 'EPSG:4326')
geno <- text_parser('genome.txt.gz')

gm <- genomaps(geno, maps)

# main --------
outlist <- lapply(nrep, function(nr) {
    lapply(sample.int(2e8, size = 10), function(ss) {
        mardf = MARsampling(gm, scheme = 'random', nrep = nr, myseed = ss)
        marfitsum = summary(MARcalc(mardf))$Parameters
        out = data.frame(
            seed = ss,
            nrep = nr,
            c = marfitsum['c', 'Estimate'],
            c_l = marfitsum['c', '2.5%'],
            c_h = marfitsum['c', '97.5%'],
            z = marfitsum['z', 'Estimate'],
            z_l = marfitsum['z', '2.5%'],
            z_h = marfitsum['z', '97.5%']
        )
        return(out)
    })
})

outdf = do.call(rbind, unlist(outlist, recursive = FALSE))

# the mean estimates
outdf %>%
    group_by(nrep) %>%
    summarise_all(., mean)

ppA <- ggplot(outdf, aes(x = factor(nrep), y = c)) +
    geom_pointrange(aes(ymin = c_l, ymax = c_h, group = seed),
                    position = position_dodge(width = 0.6),
                    alpha = 0.6, size = 0.4) +
    stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") +
    labs(x = "Number of Replicates (nrep)",
         y = "c estimate (with 95% CI)")

ppB <- ggplot(outdf, aes(x = factor(nrep), y = z)) +
    geom_pointrange(aes(ymin = z_l, ymax = z_h, group = seed),
                    position = position_dodge(width = 0.6),
                    alpha = 0.6, size = 0.4) +
    stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") +
    labs(x = "Number of Replicates (nrep)",
         y = "z estimate (with 95% CI)")

pp <- plot_grid(ppA, ppB, align = 'hv', axis = 'tblr', labels = 'AUTO', nrow = 2)

write.csv(outdf, file = 'coral_uncertainty.csv')
ggsave(filename = 'FigSX_coral_uncertainty.pdf', width = 6.5, height = 7, plot = pp)

# cleanup --------
date()
closeAllConnections()

