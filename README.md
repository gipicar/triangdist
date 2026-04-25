
# triangdist

<!-- badges: start -->
<!-- badges: end -->

An R package implementing the Triangular Distribution, including density,
distribution, quantile, and random generation functions.

## Installation

You can install the development version of triangdist from GitHub with:
```r
# install.packages("remotes")
remotes::install_github("gipicar/triangdist")
```

## Example

```r
library(triangdist)

# Density at x = 0.5
dtriang(0.5, min = 0, max = 1, mode = 0.5)

# Cumulative probability at q = 0.5
ptriang(0.5, min = 0, max = 1, mode = 0.5)

# Quantile for p = 0.25
qtriang(0.25, min = 0, max = 1, mode = 0.5)

# Generate 10 random values
rtriang(10, min = 0, max = 1, mode = 0.5)
```
