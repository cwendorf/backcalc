# [`backcalc`](https://github.com/cwendorf/backcalc)

## Backcalculate Correlations

### Description

This function reconstructs missing inferential statistics for correlation coefficients
using Fisher's z-transformation. It supports both one-sample and two-sample correlation comparisons
and allows flexible combinations of inputs to infer standard errors, test statistics, p-values,
confidence intervals, and degrees of freedom.

### Usage

```r
backcalc_corrs(
  r = NULL,
  se = NULL,
  n = NULL,
  df = NULL,
  p = NULL,
  ci = NULL,
  one_sided = FALSE,
  digits = 3,
  statistic = NULL,
  conf.level = 0.95,
  attr = TRUE
)
```

### Arguments

- **`r`**: Numeric. Correlation coefficient. For one-sample cases, a single value. For two-sample comparisons, a vector of length 2.
- **`se`**: Numeric. Standard error of the Fisher z-transformed correlation.
- **`n`**: Numeric. Sample size. For two-sample comparisons, provide a vector of length 2. Used to infer se and df when missing.
- **`df`**: Numeric. Degrees of freedom (optional). If not provided and n is available, it will be inferred.
- **`p`**: Numeric. p-value (optional). If not provided, it will be inferred from other inputs.
- **`ci`**: Numeric vector of length 2. Confidence interval for the correlation coefficient (on the raw correlation scale, not Fisher z).
- **`one_sided`**: Logical. Whether the test is one-sided (default is FALSE). Affects p-value and confidence interval calculation.
- **`digits`**: Integer. Number of significant digits for rounding results (default = 3).
- **`statistic`**: Numeric. Test statistic (usually a t- or z-value). Can be used to infer se if missing.
- **`conf.level`**: Numeric between 0 and 1. Confidence level for the confidence interval (default is 0.95).
- **`attr`**: Logical; if TRUE, attaches approximation messages as attributes (default TRUE).

### Details

The function uses Fisher's r-to-z transformation (atanh()) for inferential computations and back-transforms using tanh().
If only a subset of values is provided (e.g., only r and statistic), the function attempts to infer the rest.

In two-sample cases, the difference between Fisher z-transformed correlations is used as the estimate, and the standard error is derived accordingly.

Informative notes are printed to indicate any approximations or inferred values used during the calculation.

### Value

A data.frame with the back-calculated statistics including Estimate, SE,
test statistic (t or z), degrees of freedom (df), p-value, and confidence interval bounds.
The output has class "backcalc" and contains attribute
"Approximations" if attr = TRUE.

### Examples

```r
# One-sample: r + p-value + df (infer se, CI)
backcalc_corrs(r = 0.52, p = 0.02, df = 18)

# Two-sample: Two correlations + unequal n (infer difference, se, df, p, CI)
backcalc_corrs(r = c(0.70, 0.50), n = c(40, 25))

# Insufficient information: Two correlations but missing sample sizes (error)
backcalc_corrs(r = c(0.3, 0.5))
```

