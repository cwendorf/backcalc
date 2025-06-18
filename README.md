
# `backcalc` 

## Reconstruct Missing Inferential Statistics

[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.6.2-6666ff.svg)](https://cran.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

### Overview

**backcalc** is an R package that provides a set of functions to reconstruct key inferential statistics such as standard errors, confidence intervals, test statistics, and p-values from partial or incomplete summary information commonly reported in scientific literature. The functions support a variety of data types and designs, including single-sample, two-sample (independent or paired), one- and two-sided tests, and accommodate both z and t distributions.

Specifically, the package offers tailored functions for:
- Means (`backcalc_means`)
- Correlations (`backcalc_corrs`)
- Regression coefficients (`backcalc_coeffs`)
- Proportions (`backcalc_props`)
- Ratio measures (`backcalc_ratios`)

These tools facilitate meta-analysts, researchers, and students in recovering missing inferential details from published results to enable accurate synthesis, reporting, or further analysis.

### Installation

This package is not currently on CRAN, but can be installed and loaded using these R commands

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("cwendorf/backcalc")
library(backcalc)
```

If you do not wish a full install, the latest functions can be made available using these R commands:

```r
source("http://raw.githubusercontent.com/cwendorf/backcalc/main/source-backcalc.R")
```

### Usage

This package contains a set of examples to demonstrate its use:

- [backcalc Means Examples](./docs/BackcalcMeansExamples.md) - Means and mean differences
- [backcalc Corrs Examples](./docs/BackcalcCorrsExamples.md) - Correlations and correlation differences
- [backcalc Coeffs Examples](./docs/BackcalcCoeffsExamples.md) - Raw and standardized regresssion coefficients
- [backcalc Props Examples](./docs/BackcalcPropsExamples.md) - Proportions and proportion differences
- [backcalc Ratios Examples](./docs/BackcalcRatiosExamples.md) - Ratios and ratio differences

### Contact Me

- GitHub Issues: [https://github.com/cwendorf/backcalc/issues](https://github.com/cwendorf/CALM/backcalc) 
- Author Email: [cwendorf@uwsp.edu](mailto:cwendorf@uwsp.edu)
- Author Homepage: [https://github.com/cwendorf](https://github.com/cwendorf)

### Citation

Wendorf, C.A. (2025). *backcalc: Reconstruct missing inferential statsitics* [R Package]. [https://github.com/cwendorf/backcalc](https://github.com/cwendorf/backcalc)
