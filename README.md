
<!-- README.md is generated from README.Rmd. Please edit that file -->

## BMEmapping

<!-- badges: start -->
<!-- badges: end -->

### Spatial Interpolation for data comprising hard and soft-interval forms

Bayesian maximum entropy (BME) is a generalized spatial interpolation
method that processes both hard and soft data simultaneously to
effectively account for both spatial uncertainty and measurement
imprecision.

## Installation

You can install the development version of BMEmapping from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("KinsprideDuah/BMEmapping")
```

## Functions

*bme_predict* - predicts the posterior mean or mode with its
accompanying variance estimate of an unobserved location.

*bme_cv* - performs a cross-validation to check model performance.

*posterior_plot* - plots the the posterior density of an unobserved
location.

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/KinsprideDuah/BMEmapping/issues).

## Author

Kinspride Duah

## License

MIT + file LICENSE
