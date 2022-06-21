
# gtfstools <img align="right" src="man/figures/logo.png?raw=true" alt="logo" width="180">

[![CRAN
status](https://www.r-pkg.org/badges/version/gtfstools)](https://CRAN.R-project.org/package=gtfstools)
[![gtfstools status
badge](https://dhersz.r-universe.dev/badges/gtfstools)](https://dhersz.r-universe.dev)
[![B
status](https://github.com/ipeaGIT/gtfstools/workflows/check/badge.svg)](https://github.com/ipeaGIT/gtfstools/actions?query=workflow%3Acheck)
[![Codecov test
coverage](https://codecov.io/gh/ipeaGIT/gtfstools/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ipeaGIT/gtfstools?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN/METACRAN Total
downloads](http://cranlogs.r-pkg.org/badges/grand-total/gtfstools?color=yellow)](https://CRAN.R-project.org/package=gtfstools)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6577028.svg)](https://doi.org/10.5281/zenodo.6577028)

**gtfstools** offers a set of convenient tools for editing and analysing
transit feeds in GTFS format. Feeds are read as a `list` of
`data.table`s, allowing for easy and fast data manipulation. Many of
this package’s features are based on functions from other packages,
especially [`{tidytransit}`](https://github.com/r-transit/tidytransit)
and [`{gtfs2gps}`](https://github.com/ipeaGIT/gtfs2gps).

## Installation

Stable version:

``` r
install.packages("gtfstools")
```

Development version:

``` r
# either
install.packages("gtfstools", repos = "https://dhersz.r-universe.dev")

# or
# install.packages("remotes")
remotes::install_github("ipeaGIT/gtfstools")
```

This package requires a working installation of
[`{sf}`](https://github.com/r-spatial/sf). Please check [this
link](https://github.com/r-spatial/sf#installing) for more information
on how to install it.

## Usage

Please read **gtfstools** vignettes for more on the package usage:

  - Basic usage: reading, analysing, manipulating and writing feeds. Run
    `vignette("gtfstools")` or check it on the website ([Introduction to
    gtfstools](https://ipeagit.github.io/gtfstools/articles/gtfstools.html)).
  - Filtering GTFS feeds. Run `vignette("filtering", package =
    "gtfstools")` or check it on the website ([Filtering GTFS
    feeds](https://ipeagit.github.io/gtfstools/articles/filtering.html)).

## Related packages

  - [`{tidytransit}`](https://github.com/r-transit/tidytransit)
  - [`{gtfs2gps}`](https://github.com/ipeaGIT/gtfs2gps)
  - [`{gtfsrouter}`](https://github.com/ATFutures/gtfs-router)

## Citation

``` r
citation("gtfstools")
#> 
#> To cite gtfstools in publications use:
#> 
#>   Daniel Herszenhut, Rafael H. M. Pereira, Pedro R.
#>   Andrade & Joao Bazzo. (2022, May). gtfstools:
#>   General Transit Feed Specification (GTFS) Editing
#>   and Analysing Tools (Version v1.1.0). Zenodo.
#>   http://doi.org/10.5281/zenodo.6577028
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {gtfstools: General Transit Feed Specification (GTFS) Editing and Analysing Tools},
#>     author = {Daniel Herszenhut and Rafael H. M. Pereira and Pedro R. Andrade and Joao Bazzo},
#>     month = {may},
#>     year = {2022},
#>     publisher = {Zenodo},
#>     version = {v1.1.0},
#>     doi = {10.5281/zenodo.6577028},
#>     url = {https://doi.org/10.5281/zenodo.6577028},
#>   }
```

## Acknowledgement <a href="https://www.ipea.gov.br"><img align="right" src="man/figures/ipea_logo.png" alt="IPEA" width="300" /></a>

**gtfstools** is developed by a team at the Institute for Applied
Economic Research (Ipea), Brazil.
