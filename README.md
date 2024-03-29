
# TangledFeatures  <img src="man/figures/logo.png" align="right" height="139" />
 
<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/TangledFeatures)](https://CRAN.R-project.org/package=TangledFeatures)
<!-- badges: end -->

Note: This website will be updated in the coming days. Please drop me an email at allensunny1242@gmail.com if there are any issues.

TangledFeatures is a feature selection method that extracts needed variables in highly interrelated spaces. It does not alter the nature of the variables and is hence an alternative to traditional dimensionality reduction techniques. 

Features extracted are meant to be input into easily explainable models such as linear/logistic regressions or shallow decision trees. TangledFeatures attempts to provide highly accurate and interpretable models as opposed to current black box solutions.

## Installation

The TangledFeatures Package is now available on CRAN

You can install the development version of TangledFeatures like so:

| Type        | Source     | Command                                                                       |
|-------------|------------|-------------------------------------------------------------------------------|
| Release     | CRAN       | `install.packages("TangledFeatures")`                                             |
| Development | Github     | `install_github("TangledFeatures/TangledFeatures")` |

Once you have downloaded the package, you can then load it using:

``` r
library("TangledFeatures")
```

## Usage
``` r
Result = TangledFeatures(Data, Y_var, Focus_variables = list(), corr_cutoff = 0.7, RF_coverage = 0.95, num_features = 5,  plot = FALSE, fast_calculation = FALSE, cor1 = 'pearson', cor2 = 'PointBiserial', cor3 = 'cramersV')
``` 

## Documentation

This is a basic example which shows you how to solve a common problem:

``` r
library(TangledFeatures)
## basic example code

Result = TangledFeatures(Data, Y_var, Focus_variables = list(), corr_cutoff = 0.7, RF_coverage = 0.95, num_features = 5,  plot = FALSE, fast_calculation = FALSE, cor1 = 'pearson', cor2 = 'PointBiserial', cor3 = 'cramersV')

Variables <- Result$Groups 
```

  
