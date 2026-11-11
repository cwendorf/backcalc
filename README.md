# [`backcalc`](https://github.com/cwendorf/backcalc)

## Reconstruct Missing Inferential Statistics

[![Minimal R Version](https://img.shields.io/badge/R%3E%3D-3.6.2-blue?style=for-the-badge)](https://cran.r-project.org/)
[![GitHub Release](https://img.shields.io/github/v/release/cwendorf/backcalc?style=for-the-badge)](https://github.com/cwendorf/backcalc/releases/latest)
[![Project Status](https://img.shields.io/badge/Status-Active-success?style=for-the-badge)](https://www.repostatus.org/#active)

### Overview

`backcalc` is an R package designed to help researchers and students reconstruct key inferential statistics — such as standard errors, confidence intervals, test statistics, and p-values — from partial or incomplete summary data commonly found in scientific literature. Supporting various data types and designs, including single- and two-sample (independent or paired) tests with z or t distributions, it offers tailored functions for means, medians, correlations, regression coefficients, proportions, and ratio measures.

### Installation

This package is not currently available on CRAN. It can be installed directly from GitHub:

``` r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("cwendorf/backcalc")
library(backcalc)
```

If you prefer not to install the package, you can load the current functions directly:

```r
source("https://raw.githubusercontent.com/cwendorf/backcalc/main/source-backcalc.R")
```

### Usage

This package includes the following documentation:

- [Introduction](./docs/README.md): A quick overview and summary of the package.
- [Reference](./docs/man/README.md): Descriptions of all exported functions, including usage, arguments, and return values.
- [Articles](./docs/vignettes/README.md): Examples demonstrating how to use the package.

### Citation

Wendorf, C.A. (2025). *backcalc: Reconstruct missing inferential statistics* [R Package]. [https://github.com/cwendorf/backcalc](https://github.com/cwendorf/backcalc)
