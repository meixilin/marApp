The **mutations-area relationship (MAR)** and **genetic-diversity-area relationship (GDAR)** can be applied in reverse to project immediate genetic diversity loss associated with rapid habitat reduction. 
Recall that the MAR follows the equation $M = cA^z$, if we note the proportion of area lost as $a$, and the proportion of genetic diversity lost as $m$, the MAR still holds for the remaining genetic diversity $M(1-m) = c[A(1-a)]^z$. Dividing the two equations yields the genetic diversity loss prediction: 

$$
1-m = (1-a)^z
$$

This equation enables conservation scientists to forecast the impact of habitat loss on genetic variation, which is critical for species survival and adaptive potential.

To simulate these scenarios, the marApp applies a spatial extinction modeling process. Raster grid cells are sequentially removed from a species' range to mimic habitat loss. We sample the remaining individuals and recalculate genetic diversity metrics during this process.

The sampling design, area calculation, and genetic diversity metrics utilized are the same as the option specified in the "Mutations-area relationship" tab. 

