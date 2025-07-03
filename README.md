
# `backcalc` 

## Reconstruct Missing Inferential Statistics

[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.6.2-6666ff.svg)](https://cran.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

### Overview

`backcalc` is an R package designed to help researchers and students reconstruct key inferential statistics —- such as standard errors, confidence intervals, test statistics, and p-values —- from partial or incomplete summary data commonly found in scientific literature. Supporting various data types and designs, including single- and two-sample (independent or paired) tests with z or t distributions, it offers tailored functions for means, correlations, regression coefficients, proportions, and ratio measures. 

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

- [Means Examples](./docs/MeansExamples.md) - Means and mean differences
- [Correlations Examples](./docs/CorrelationsExamples.md) - Correlations and correlation differences
- [Coefficients Examples](./docs/CoefficientsExamples.md) - Raw and standardized regresssion coefficients
- [Medians Examples](./docs/MediansExamples.md) - Medians and median differences
- [Proportions Examples](./docs/ProportionsExamples.md) - Proportions and proportion differences
- [Ratios Examples](./docs/RatiosExamples.md) - Ratios and ratio differences

### Contact Me

- GitHub Issues: [https://github.com/cwendorf/backcalc/issues](https://github.com/cwendorf/CALM/backcalc) 
- Author Email: [cwendorf@uwsp.edu](mailto:cwendorf@uwsp.edu)
- Author Homepage: [https://github.com/cwendorf](https://github.com/cwendorf)

### Citation

Wendorf, C.A. (2025). *backcalc: Reconstruct missing inferential statsitics* [R Package]. [https://github.com/cwendorf/backcalc](https://github.com/cwendorf/backcalc)
