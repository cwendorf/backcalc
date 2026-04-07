# [`backcalc`](https://github.com/cwendorf/backcalc)

## Backcalculate Missing Inferential Statistics for Multiple Regression Coefficients

**Aliases:**

- `backcalc_multreg`

### Description

This function attempts to infer regression coefficients, standard errors, test statistics,
p-values, and confidence intervals for multiple regression predictors given partial input.
It supports a variety of input forms, including unstandardized coefficients (b and se),
standardized betas (std_beta and se_std), confidence intervals, p-values, and statistics.
It can also standardize coefficients if standard deviations of predictors (sd_x) and outcome (sd_y) are provided.

### Usage

```r
backcalc_multreg(
  intercept = NULL,
  intercept_se = NULL,
  b = NULL,
  se = NULL,
  std_beta = NULL,
  se_std = NULL,
  sd_x = NULL,
  sd_y = NULL,
  df = NULL,
  n = NULL,
  p = NULL,
  ci = NULL,
  statistic = NULL,
  one_sided = FALSE,
  conf.level = 0.95,
  digits = 3,
  attr = TRUE
)
```

### Arguments

- **`intercept`**: Numeric, estimate of the intercept.
- **`intercept_se`**: Numeric, standard error of the intercept.
- **`b`**: Numeric vector of unstandardized regression coefficients for predictors.
- **`se`**: Numeric vector of standard errors corresponding to b.
- **`std_beta`**: Numeric vector of standardized regression coefficients.
- **`se_std`**: Numeric vector of standard errors for standardized coefficients.
- **`sd_x`**: Numeric vector of standard deviations of predictor variables (used for standardizing).
- **`sd_y`**: Numeric scalar, standard deviation of outcome variable.
- **`df`**: Numeric degrees of freedom for t-distribution; if NULL, normal distribution assumed.
- **`n`**: Numeric sample size; used to infer df if df not provided.
- **`p`**: Numeric vector of p-values.
- **`ci`**: List of numeric vectors of length 2 giving confidence intervals for predictors.
- **`statistic`**: Numeric vector of test statistics (t or z).
- **`one_sided`**: Logical, whether p-values/statistics are one-sided (default FALSE).
- **`conf.level`**: Numeric confidence level for intervals (default 0.95).
- **`digits`**: Integer, number of decimal places for output rounding (default 3).
- **`attr`**: Logical, whether to attach approximation notes as an attribute (default TRUE).

### Details

The function returns a data.frame with rows for the intercept and each predictor, and columns:
Estimate, SE, Statistic, p, LL, and UL.
Approximation notes describing inferred values are attached as an attribute and printed by a
compatible print method.

The function tries to infer missing values using various rules:

 Standardized betas can be computed from unstandardized betas and SDs.
 SEs can be approximated from confidence intervals.
 Test statistics and p-values can be derived from each other.
 Confidence intervals can be reconstructed from estimates and SEs.

If insufficient information is provided to compute estimates or SEs, the function prints a message and returns NULL.

### Value

A data.frame with rows for Intercept and predictors named X1, X2, ....
Columns include Estimate, SE, Statistic, df, p, LL, and UL.
Approximation notes are attached as the "Approximations" attribute.

### Examples

```r
# Example 1: Intercept plus unstandardized betas with SEs and sample size
backcalc_multreg(
  intercept = 2.5, intercept_se = 0.4,
  b = c(0.7, -0.3), se = c(0.15, 0.1),
  n = 100
)

# Example 2: Standardized betas with SEs and degrees of freedom
backcalc_multreg(
  intercept = 1.2, intercept_se = 0.3,
  std_beta = c(0.25, 0.4), se_std = c(0.05, 0.07),
  df = 50
)

# Example 3: Confidence intervals for predictors and sample size
backcalc_multreg(
  intercept = 3.0, intercept_se = 0.5,
  ci = list(c(0.1, 0.5), c(-0.2, 0.4)),
  n = 80
)
```

