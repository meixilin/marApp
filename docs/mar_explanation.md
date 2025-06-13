The **mutations-area relationship (MAR)** and **genetic-diversity-area relationship (GDAR)** describe the scaling relationships between genetic diversity and area, providing theoretical foundations for genetic diversity estimates and predictions in conservation. 
Researchers have discovered that genetic diversity, either measured as the number of segregating sites $M$, or the nucleotide diversity ($\theta_\pi$ or $GD$), increases with the area surveyed ($A$). 
This scaling relationship emerges from the principle of "commonness of rarity" - demonstrated in the tab "Site-frequency spectrum". 
Analogous to the **species-area relationship (SAR)**, the shape of the MAR/GDAR often follows a power-law function: 

$$
M/GD = cA^z
$$

To empirically estimate MAR, here we spatially subsample individuals across the population's range and estimate genetic diversity at each step. 
Starting from the finest resolution (single raster cells) to broadest resolution (full range), we generate replicated square areas and compute genetic diversity within each. 
Finally, power-law models are fitted to the genetic diversity and area, yielding estimates of the scaling exponent $z$. 

Users can choose from five sampling designs: 
1. random, 
2. directional (south-to-north and north-to-south), 
3. focal point-based (inward and outward)

Area can be quantified via two methods:
1. total square area (Asq,  degree^2) 
2. occupied grid-cell area (A, km^2)

For genetic diversity sampling, we offer four different genetic diversity statistics:
1. Number of segregating sites ($M$)
2. Endemic segregating sites ($E$)
3. Watterson's theta ($\theta_w$)
4. Nucleotide diversity ($\theta_\pi$)
