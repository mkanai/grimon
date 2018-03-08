# Grimon
**Gr**aphical **i**nterface to visualize **m**ulti-**o**mics **n**etworks.

## Overview
Grimon (**Gr**aphical **i**nterface to visualize **m**ulti-**o**mics **n**etworks) visualizes high-dimensional multi-layered data sets in three-dimensional parallel coordinates. It enables users to intuitively and interactively explore their data, helping their understanding of multiple inter-layer connections embedded in high-dimensional complex data.


## Installation
```{r}
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("mkanai/grimon")
```

## Example usage
```{r}
library(grimon)
data("grimon.example")
grimon(x, col = col, label = 1:6,
       optimize_coordinates = TRUE, maxiter = 1e3,
       segment_alpha = 0.5)
```

## Contact
Masahiro Kanai (mkanai@g.harvard.edu)

http://mkanai.github.io/
