# sample geno
samp_geno <- function(geno, nsnps) {
   geno[, sample(1:ncol(geno), nsnps, F)]
}

# count number of mutations
calc_M <- function(geno, ploid = 2) {
    ac = colSums(geno, na.rm = TRUE)
    sum(ac > 0 & ac < ploid*nrow(geno))
}

# # old version
# calc_M0 <- function(geno) {
#     sum(apply(geno, 2, function(x) { # for every column of SNP matrix...
#         # count major and minor alleles
#         ac <- c((sum(x == 0, na.rm = T) * 2) + sum(x == 1, na.rm = T), (sum(x == 2, na.rm = T) * 2) + sum(x == 1, na.rm = T))
#         # return: if both alleles are >0, there is a mutation
#         return(sum(ac > 0) == 2)
#     }))
# }
