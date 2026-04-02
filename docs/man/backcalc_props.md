# [`backcalc`](https://github.com/cwendorf/backcalc)

## Backcalculate Missing Inferential Statistics for Proportions

**Aliases:**

- `backcalc_props`

### Description

This function reconstructs inferential statistics related to proportions from partial information.
It can estimate standard errors, confidence intervals, p-values, test statistics (t or z), degrees of freedom,
and point estimates when some components are missing, supporting one-sample, paired, and two-sample cases.

### Usage

```r
backcalc_props(
  prop = NULL,
  se = NULL,
  n = NULL,
  x = NULL,
  ci = NULL,
  p = NULL,
  one_sided = FALSE,
  digits = 3,
  interval_type = "wald",
  statistic = NULL,
  df = NULL,
  conf.level = 0.95,
  method = "wald",
  continuity = FALSE,
  attr = TRUE
)
```

### Arguments

- **`prop`**: Numeric vector of proportion(s). For one-sample, a single value; for two-sample, a vector of length 2.
- **`se`**: Standard error of the estimate, if known.
- **`n`**: Sample size(s). A single value for one-sample tests, or a vector of length 2 for two-sample tests.
- **`x`**: Count(s) of "successes" (used to compute proportions). A single value or vector of length 2.
- **`ci`**: Confidence interval (numeric vector of length 2: lower and upper limits), if already known.
- **`p`**: P-value for the test statistic. If not provided, it will be computed when possible.
- **`one_sided`**: Logical. Is the test one-sided? Defaults to FALSE (i.e., two-sided).
- **`digits`**: Number of decimal places to round results. Default is 3.
- **`interval_type`**: Type of confidence interval. Currently unused (placeholder).
- **`statistic`**: Observed test statistic (z or t), if known.
- **`df`**: Degrees of freedom, required if using a t-statistic.
- **`conf.level`**: Confidence level for the confidence interval. Default is 0.95.
- **`method`**: Character. Either "wald" (default) for normal approximations or "exact" for exact binomial CI (one-sample only).
- **`continuity`**: Logical. Whether to apply continuity correction in two-sample Wald test. Default is FALSE.
- **`attr`**: Logical; if TRUE, attaches approximation messages as attributes (default TRUE).

### Details

This function is designed to handle partial or minimal inputs by inferring missing values when possible.
For instance, if only a proportion and p-value are provided, it can back-calculate the sample size or test statistic.

### Value

A data.frame with the back-calculated statistics including Estimate, SE,
test statistic (t or z), degrees of freedom (df), p-value, and confidence interval bounds.
The output has class "backcalc" and contains attribute
"Approximations" if attr = TRUE.

### Examples

```r
# One-sample: Provide proportion and sample size only (basic one-sample proportion)
backcalc_props(prop = 0.4, n = 100)

# Two-sample: Provide two proportions and sample sizes only (difference in proportions)
backcalc_props(prop = c(0.55, 0.4), n = c(150, 130))

# Insufficient info: Provide n only, no prop, counts, or se
backcalc_props(n = 100)
```

