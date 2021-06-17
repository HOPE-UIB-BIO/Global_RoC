# Global acceleration in rates of vegetation change over the past 18,000 years 

## Authors
Ondřej Mottl, Suzette G.A. Flantua, Kuber P. Bhatta, Vivian A. Felde, Thomas Giesecke, Simon Goring, Eric C. Grimm, Simon Haberle, Henry Hooghiemstra, Sarah Ivory, Petr Kuneš, Steffen Wolters, Alistair W. R. Seddon, John W. Williams

### Corresponding authors
Ondrej Mottl (ondrej.mottl@gmail.com), Suzette Flantua (s.g.a.flantua@gmail.com)

### One sentence summary
A compilation of over 1000 fossil pollen sequences shows that global vegetation change accelerated several thousand years ago.

## Abstract
Global vegetation over the last 18,000 years was transformed first by the climate changes accompanying the last deglaciation and again by increasing human pressures, but the magnitude and patterns of rates of vegetation change are poorly understood globally. Using a compilation of 1181 fossil pollen sequences and new statistical methods, we detect a worldwide acceleration in rates of vegetation compositional change beginning between 4.6 and 2.9 ka that is globally unprecedented over the last 18,000 years in magnitude and extent. Late Holocene rates of change equal or exceed deglacial rates for all continents, suggesting that the scale of human impacts on terrestrial ecosystems exceeds even the climate-driven transformations of the last deglaciation. The acceleration of biodiversity change demonstrated in last-century ecological datasets began millennia ago.

## Correction
After the publication of the research article “Global acceleration in rates of vegetation change over the past 18,000 years”, a bug was detected in the R-Ratepol code that affected the assignment of ages to individual time bins. The code should have assigned the average age estimate of the bin to each bin, but instead assigned the average of the ages for the selected sample in that same bin. Correcting this error tends to further amplify late-Holocene rates of change relative to earlier periods, while having no effect upon the main conclusions drawn in this paper.  Updated versions of Figures 2-4 are included with this correction. We further conducted an alternative analysis with unevenly (v-1-1) versus evenly (v-1-1-1) spaced time bins and showed that this had no substantive effects on the results shown here.

Description of branches in the repository:

### Branch v-1
Original code containing code, data, and figures used in the published paper from 21-05-2021

### Branch v-1-1
Results generated using R-Ratepol with the bug corrected. All other analytical procedures are the same as those published in the original paper (original site clustering is kept). Tables S1-2 and Figures 2-4 are recreated. Corresponding figures are published in the paper correction (link) 

### Branch v-1-1-1
Results generated using R-Ratepol with the bug corrected. In addition, we investigated the impact of the results of restricting Rate of Change calculations to only adjacent time bins. A discussion of this option is also included in the corresponding R-Ratepol manuscript. Tables S1-2 and Figures 2-4 are recreated with these settings. 
