# [`backcalc`](https://github.com/cwendorf/backcalc)

## Backcalculate Standardized Mean Differences

### Description

This function reconstructs standardized mean differences and related effect size statistics
from various combinations of inputs. It supports Cohen's d, Hedges' g, and Glass's delta,
along with confidence intervals and test statistics for one-sample, paired, and two-sample designs.

### Usage

```r
backcalc_standard(
  d = NULL,
  m = NULL,
  sd = NULL,
  n = NULL,
  se = NULL,
  statistic = NULL,
  p = NULL,
  ci_d = NULL,
  paired = FALSE,
  one_sided = FALSE,
  type = "d",
  control_sd = 1,
  conf.level = 0.95,
  digits = 3,
  attr = TRUE
)
```

### Arguments

- **`d`**: Numeric. Standardized mean difference (Cohen's d or similar). For two-sample cases,
provide a single value representing the difference between groups.
- **`m`**: Numeric vector of means or mean differences. For two-sample cases, provide
a vector of length 2 representing group means.
- **`sd`**: Numeric vector of standard deviations for each group.
- **`n`**: Numeric vector of sample sizes for each group.
- **`se`**: Numeric. Standard error of the mean difference (unstandardized).
- **`statistic`**: Numeric. Test statistic value (e.g., t-statistic).
- **`p`**: Numeric. P-value associated with the test statistic.
- **`ci_d`**: Numeric vector of length 2. Confidence interval for the effect size (Cohen's d).
- **`paired`**: Logical. Whether the comparison is paired (default is FALSE).
- **`one_sided`**: Logical. Whether a one-sided test is used (default is FALSE).
- **`type`**: Character. Type of effect size to compute: "d" (Cohen's d, default),
"g" (Hedges' g, bias-corrected), or "delta" (Glass's delta, uses control group SD).
- **`control_sd`**: Numeric. For type = "delta", specify which group's SD to use as control.
Default is 1 (first group).
- **`conf.level`**: Numeric. Confidence level for intervals (default 0.95).
- **`digits`**: Integer. Number of digits to round the output (default 3).
- **`attr`**: Logical. If TRUE, attaches approximation messages as attributes (default TRUE).

### Details

The function supports multiple effect size types:

 Cohen's d: Standardized by the pooled standard deviation
(or average SD for unequal variances).
 Hedges' g: Bias-corrected version of Cohen's d, adjusting for small samples.
 Glass's delta: Standardized by the control or reference group's SD only.

Confidence intervals for effect sizes use noncentral t-distributions when appropriate.

### Value

A data.frame with effect size statistics including the standardized estimate,
standard error (of the effect size), test statistic, degrees of freedom, p-value,
and confidence interval.
The output has class "backcalc" and contains attribute
"Approximations" if attr = TRUE.

### Examples

```r
# One-sample: Calculate d from mean, SD, and sample size
backcalc_standard(m = 2.5, sd = 4, n = 25)

# Two-sample: Calculate d from group means, SDs, and sample sizes
backcalc_standard(m = c(15, 12), sd = c(4, 5), n = c(40, 35))

# Backcalculate d from t-statistic and sample size
backcalc_standard(statistic = 2.5, n = 50)

# Two-sample with Hedges' g (bias-corrected)
backcalc_standard(m = c(15, 12), sd = c(4, 5), n = c(40, 35), type = "g")
```

