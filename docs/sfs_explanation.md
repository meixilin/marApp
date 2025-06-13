The **mutations-area relationship (MAR)** describes how genetic diversity, measured as the number of segregating sites $M$, scales with the geographic area sampled. It typically follows a power-law form of $M = cA^z$. 

This relationship arises from a core principle: the *commonness of rarity*. Most mutations in a population are rare, and only a few reach high frequencies.

The **site frequency spectrum (SFS)** captures this distribution of allele frequencies and is a fundamental summary statistic in population genetics. It represents the number of mutations observed at different frequencies within a sample. The shape of the SFS, which often skewed toward low-frequency variants, is what gives rise to the scaling behavior observed in MAR.

This relationship mirrors a well-known pattern in ecology: just as the **species-area relationship (SAR)** emerges from the **species abundance distribution (SAD)**, the **mutations-area relationship (MAR)** is a consequence of the **site frequency spectrum (SFS)**, both rooted in the statistical structure of rarity in biological systems.

To illustrate this connection, we plot the empirical SFS from the dataset uploaded and fit several ecological species abundance models, including the **log-series** and **log-normal** distributions. Despite being developed for species abundance data, these models show similarities to the shape of the SFS. We also overlay the **neutral expectation under the Wright-Fisher model**, which exhibits a characteristic $1/x$ decay.

However, we emphasize that this is not a formal population genetics model fit. For rigorous inference based on SFS, we refer readers to specialized tools such as *∂a∂i* and *fastsimcoal*.

