# [`backcalc`](https://github.com/cwendorf/backcalc)

## Backcalculate Regression Coefficients

### Description

This function reconstructs inferential statistics for a regression coefficient.
It allows for partial input of summary or inferential statistics and infers missing values
such as standard errors, test statistics, confidence intervals, p-values, and degrees of freedom.
It supports both unstandardized and standardized coefficients.

### Usage

```r
backcalc_coeffs(
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

- **`b`**: Numeric. Unstandardized regression coefficient.
- **`se`**: Numeric. Standard error of the unstandardized coefficient.
- **`std_beta`**: Numeric. Standardized regression coefficient.
- **`se_std`**: Numeric. Standard error of the standardized coefficient.
- **`sd_x, sd_y`**: Numeric. Standard deviations of the predictor and outcome variables. Used to convert between standardized and unstandardized forms.
- **`df`**: Numeric. Degrees of freedom for a t-distribution. If omitted but n is provided, it is inferred as df = n - 1.
- **`n`**: Integer. Sample size. If df is missing, df is inferred as n - 1. If df is provided but n is missing, n = df + 1.
- **`p`**: Numeric. P-value associated with the test statistic.
- **`ci`**: Numeric vector of length 2. Confidence interval bounds (lower, upper).
- **`statistic`**: Numeric. Test statistic (t or z value), if already known.
- **`one_sided`**: Logical. Whether the hypothesis test is one-sided. Default is FALSE (two-sided).
- **`conf.level`**: Numeric between 0 and 1. Confidence level for the confidence interval. Default is 0.95.
- **`digits`**: Integer. Number of decimal digits to round the output. Default is 3.
- **`attr`**: Logical; if TRUE, attaches approximation messages as an attribute (default TRUE).

### Details

The function accepts a flexible combination of inputs. If sufficient information is not provided,
it attempts to approximate missing values where logically possible. Standardized coefficients can be
derived from unstandardized ones using the provided standard deviations. Missing SEs or test statistics
may be inferred from confidence intervals or p-values. If both n and df are missing,
a z-distribution is assumed.

### Value

A data.frame with the back-calculated statistics including Estimate, SE,
test statistic (t or z), degrees of freedom (df), p-value, and confidence interval bounds.
The output has class "backcalc" and contains attribute
"Approximations" if attr = TRUE.

### Examples

```r
# Unstandardized beta example: coefficient and SE, z-test assumed
backcalc_coeffs(b = 0.5, se = 0.1)

# Standardized beta and conversion example: unstandardized beta and SE with SDs to infer standardized beta
backcalc_coeffs(b = 1.1, se = 0.3, sd_x = 2.5, sd_y = 5)

# Insufficient information example: only p-value provided, no estimate or SE (function returns NULL with warning)
backcalc_coeffs(p = 0.05)
```

