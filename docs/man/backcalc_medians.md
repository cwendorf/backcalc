# [`backcalc`](https://github.com/cwendorf/backcalc)

## Backcalculate Missing Inferential Statistics for Medians

**Aliases:**

- `backcalc_medians`

### Description

This function estimates standard errors, test statistics, p-values, confidence intervals,
and degrees of freedom for median-based statistics using summary-level data.
It supports robust measures of spread such as interquartile range (IQR), median absolute deviation (MAD),
and range, applying normal approximation for inference without relying on parametric assumptions.
When parametric inputs (e.g., standard deviations) are provided, t-distribution based inference is used.

### Usage

```r
backcalc_medians(
  m = NULL,
  se = NULL,
  sd = NULL,
  n = NULL,
  df = NULL,
  p = NULL,
  ci = NULL,
  statistic = NULL,
  iqr = NULL,
  mad = NULL,
  range = NULL,
  paired = FALSE,
  one_sided = FALSE,
  conf.level = 0.95,
  digits = 3,
  attr = TRUE
)
```

### Arguments

- **`m`**: Numeric scalar or vector. Median(s) of the group(s). For two-sample comparisons, provide a length-2 vector.
- **`se`**: Numeric scalar or vector. Standard error(s) of the estimate(s).
- **`sd`**: Numeric scalar or vector. Standard deviation(s) of the group(s).
- **`n`**: Numeric scalar or vector. Sample size(s).
- **`df`**: Numeric scalar. Degrees of freedom for t-distribution inference.
- **`p`**: Numeric scalar. p-value for the test statistic.
- **`ci`**: Numeric vector of length 2. Confidence interval bounds (lower, upper).
- **`statistic`**: Numeric scalar. Test statistic (z or t).
- **`iqr`**: Numeric scalar or vector. Interquartile range(s).
- **`mad`**: Numeric scalar or vector. Median absolute deviation(s).
- **`range`**: Numeric scalar or vector. Range(s) of the data.
- **`paired`**: Logical. Whether the data are paired (default FALSE).
- **`one_sided`**: Logical. Whether the test is one-sided (default FALSE).
- **`conf.level`**: Numeric scalar. Confidence level for intervals (default 0.95).
- **`digits`**: Integer. Number of decimal places to round results (default 3).
- **`attr`**: Logical; if TRUE, attaches approximation messages as an attribute (default TRUE).

### Details

The function supports inference for medians using IQR, MAD, or range to approximate SE
with normal-based confidence intervals and z-tests. When parametric inputs like SD and sample size
are given without robust measures, Welch's t-test approximation is used.

### Value

A data.frame with the back-calculated statistics including Estimate, SE,
test statistic (t or z), degrees of freedom (df), p-value, and confidence interval bounds.
The output has class "backcalc" and contains attribute
"Approximations" if attr = TRUE.

### Examples

```r
# One-sample: Median, IQR, and sample size
backcalc_medians(m = 50, iqr = 20, n = 30)

# Two-sample: Group medians, MAD, and sample size
backcalc_medians(m = c(75, 68), mad = 9, n = 40)

# Insufficient info: Only median and sample size, no dispersion
backcalc_medians(m = c(52, 49), n = 30)
```

