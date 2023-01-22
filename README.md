
# TangledFeatures  <img src="man/figures/logo.png" align="right" height="139" />
 
<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/TangledFeatures)](https://CRAN.R-project.org/package=TangledFeatures)
<!-- badges: end -->

TangledFeatures is a feature selection method that extracts needed variables in highly interrelated spaces. It does not alter the nature of the variables and is hence an alternative to traditional dimensionality reduction techniques. 

<<<<<<< HEAD
Features extracted are meant to be inputted into easily explainable models such as linear/logistic regressions or shallow decision trees. TangledFeatures attempts to provide highly accurate and interpretable models as opposed to current black box solutions.  
=======
Please note that the package is currently submitted to CRAN and awaiting approval. CRAN submissions unfortunately take a long time.

>>>>>>> f47629f09819bb93cd94fc3b9a877428dcdd74d6

## Installation

You can install the development version of TangledFeatures like so:

``` r
package(TangledFeatures)
install_github("TangledFeatures/TangledFeatures")
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

  
