  <!-- badges: start -->
  [![Travis build status](https://travis-ci.com/Timmyanna/stat302Package.svg?branch=master)](https://travis-ci.com/Timmyanna/stat302Package)
  [![Codecov test coverage](https://codecov.io/gh/Timmyanna/stat302Package/branch/master/graph/badge.svg)](https://codecov.io/gh/Timmyanna/stat302Package?branch=master)
  <!-- badges: end -->

## Use

The vignette demonstrates example usage of all main functions. You can see the vignette by using the following code (note that this requires a TeX installation to view properly):

``` r
# install.packages("devtools")
devtools::install_github("Timmyanna/stat302Package", build_vignette = TRUE, build_opts = c())
library(stat302Package)
# Use this to view the vignette in the stat302Package HTML help
help(package = "stat302Package", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "stat302Package")
```
