Mutations-area relationship (MAR) studies genetic mutation richness ($M$), defined as the number of polymorphic sites in a population (also known as allelic richness). It is inspired from the classical ecological concept, the species-area relationship (SAR).
Individual mutations in a population, similar to species in a community, also typically remain rare and some become abundant due to genetic drift or natural selection. 

Therefore, the more individuals ($N$) and areas ($A$, `A_sq` in the table) sampled, the more allelic richness will be found. We described this scaling-relationship as MAR, and have tested that it follows a power-law function across species (Exposito-Alonso et al. 2022): 

$$
M = cA^z
$$

In this demo using the data from [1001 genomes](https://1001genomes.org/) of *Arabidopsis thaliana*, 1001 individuals/ecotypes ($N_\text{max} = 1001$) were selected for the mutations-area relationship analysis. To speed up the calculation, for each replicate, we randomly subsetted 1000 genotypes in the whole genome dataset. 

The scaling parameters can be calculated using the `sar_power` function:
