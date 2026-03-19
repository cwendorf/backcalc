# DESCRIPTION

```
Package: backcalc
Type: Package
Title: Reconstruct Missing Inferential Statistics
Description: This package provides functions to reconstruct key inferential statistics such as standard errors, confidence intervals, test statistics, and p-values from partial or incomplete summary information commonly reported in scientific literature.
Version: 0.1.0
Authors@R: person("Craig", "Wendorf", email = "cwendorf@uwsp.edu", role = c("aut", "cre"))
License: MIT + file LICENSE
URL: https://github.com/cwendorf/backcalc
BugReports: https://github.com/cwendorf/backcalc/issues
Encoding: UTF-8
LazyData: true
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.3.2
Imports: stats, utils
VignetteBuilder: knitr
```

# `backcalc`: backcalc: Reconstruct Missing Inferential Statistics and Effect Sizes

## Description

This package provides functions to reconstruct key inferential statistics such as standard errors,
confidence intervals, test statistics, and p-values from partial or incomplete summary information
commonly reported in scientific literature. It also includes functions to calculate standardized
effect sizes (Cohen's d, Hedges' g, Glass's delta) from various inputs.

## Details

`backcalc` is an R package designed to help researchers and students reconstruct key inferential statistics —-
such as standard errors, confidence intervals, test statistics, and p-values —-
from partial or incomplete summary data commonly found in scientific literature.
Supporting various data types and designs, including single- and two-sample (independent or paired) tests
with z or t distributions, it offers tailored functions for means, medians,
correlations, regression coefficients, proportions, ratio measures, and standardized effect sizes.

## Author

***Maintainer***: Craig Wendorf
[cwendorf@uwsp.edu](mailto:cwendorf@uwsp.edu)

## Seealso

Useful links:

* [https://github.com/cwendorf/backcalc](https://github.com/cwendorf/backcalc)
* Report bugs at [https://github.com/cwendorf/backcalc/issues](https://github.com/cwendorf/backcalc/issues)

# `backcalc_anova`: Backcalculate Missing Inferential Statistics for ANOVA (Vectorised & Design-Aware)

## Description

Handles between-subjects, within-subjects, factorial, and mixed designs, with optional
sphericity corrections. Works with multiple effects at once.

## Usage

```r
backcalc_anova(
  F = NULL,
  df1 = NULL,
  df2 = NULL,
  p = NULL,
  eta2 = NULL,
  f = NULL,
  design = "between",
  n = NULL,
  levels = NULL,
  subjects = NULL,
  epsilon = 1,
  effect = "main",
  labels = NULL,
  conf.level = 0.95,
  digits = 3,
  attr = TRUE
)
```

## Arguments

* `F`: Numeric vector of F-statistics.
* `df1`: Numeric vector of numerator degrees of freedom.
* `df2`: Numeric vector of denominator degrees of freedom.
* `p`: Numeric vector of p-values.
* `eta2`: Numeric vector of partial eta-squared values.
* `f`: Numeric vector of Cohen's f values.
* `design`: Character vector: "between", "within", or "mixed".
* `n`: Numeric vector: total sample size(s) or vector of group Ns.
* `levels`: List: each element is an integer vector of factor levels for the effect.
* `subjects`: Numeric vector: number of subjects (for within/mixed designs).
* `epsilon`: Numeric vector: sphericity correction factors.
* `effect`: Character vector: "main" or "interaction".
* `labels`: Optional character vector of row labels (one per effect). If omitted,
names are derived from `levels` and `effect` type.
* `conf.level`: Confidence level for eta-squared CI.
* `digits`: Rounding digits for output.
* `attr`: Attach approximation notes as attributes.

## Value

A data.frame with one row per effect, containing F, df1, df2, p,
Cohen's f, partial eta-squared, and CI bounds for eta-squared. Attributes store approximation notes.

## Examples

```r
# 1. One-way between-subjects ANOVA (single effect)
backcalc_anova(F = 5.2, df1 = 2, df2 = 30,
               design = "between", levels = list(3), n = 33,
               labels = "Treatment")

# 2. Two separate main effects (e.g., reporting each factor separately)
backcalc_anova(F = c(5.2, NA), df1 = c(2, NA), df2 = c(30, NA),
               p = c(NA, 0.01), design = "between",
               levels = list(3, 4), n = 33,
               labels = c("FactorA", "FactorB"))
```

# `backcalc_coeffs`: Backcalculate Missing Inferential Statistics for Regression Coefficients

## Description

This function reconstructs inferential statistics for a regression coefficient.
It allows for partial input of summary or inferential statistics and infers missing values
such as standard errors, test statistics, confidence intervals, p-values, and degrees of freedom.
It supports both unstandardized and standardized coefficients.

## Usage

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

## Arguments

* `b`: Numeric. Unstandardized regression coefficient.
* `se`: Numeric. Standard error of the unstandardized coefficient.
* `std_beta`: Numeric. Standardized regression coefficient.
* `se_std`: Numeric. Standard error of the standardized coefficient.
* `sd_x, sd_y`: Numeric. Standard deviations of the predictor and outcome variables. Used to convert between standardized and unstandardized forms.
* `df`: Numeric. Degrees of freedom for a t-distribution. If omitted but `n` is provided, it is inferred as `df = n - 1`.
* `n`: Integer. Sample size. If `df` is missing, `df` is inferred as `n - 1`. If `df` is provided but `n` is missing, `n = df + 1`.
* `p`: Numeric. P-value associated with the test statistic.
* `ci`: Numeric vector of length 2. Confidence interval bounds (lower, upper).
* `statistic`: Numeric. Test statistic (t or z value), if already known.
* `one_sided`: Logical. Whether the hypothesis test is one-sided. Default is `FALSE` (two-sided).
* `conf.level`: Numeric between 0 and 1. Confidence level for the confidence interval. Default is `0.95`.
* `digits`: Integer. Number of decimal digits to round the output. Default is `3`.
* `attr`: Logical; if TRUE, attaches approximation messages as an attribute (default TRUE).

## Details

The function accepts a flexible combination of inputs. If sufficient information is not provided,
it attempts to approximate missing values where logically possible. Standardized coefficients can be
derived from unstandardized ones using the provided standard deviations. Missing SEs or test statistics
may be inferred from confidence intervals or p-values. If both `n` and `df` are missing,
a z-distribution is assumed.

## Value

A `data.frame` with the back-calculated statistics including Estimate, SE,
test statistic (t or z), degrees of freedom (df), p-value, and confidence interval bounds.
The output has class `"backcalc"` and contains attribute
`"Approximations"` if `attr = TRUE`.

## Examples

```r
# Unstandardized beta example: coefficient and SE, z-test assumed
backcalc_coeffs(b = 0.5, se = 0.1)

# Standardized beta and conversion example: unstandardized beta and SE with SDs to infer standardized beta
backcalc_coeffs(b = 1.1, se = 0.3, sd_x = 2.5, sd_y = 5)

# Insufficient information example: only p-value provided, no estimate or SE (function returns NULL with warning)
backcalc_coeffs(p = 0.05)
```

# `backcalc_corrs`: Backcalculate Missing Inferential Statistics for Correlations

## Description

This function reconstructs missing inferential statistics for correlation coefficients
using Fisher's z-transformation. It supports both one-sample and two-sample correlation comparisons
and allows flexible combinations of inputs to infer standard errors, test statistics, p-values,
confidence intervals, and degrees of freedom.

## Usage

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

## Arguments

* `r`: Numeric. Correlation coefficient. For one-sample cases, a single value. For two-sample comparisons, a vector of length 2.
* `se`: Numeric. Standard error of the Fisher z-transformed correlation.
* `n`: Numeric. Sample size. For two-sample comparisons, provide a vector of length 2. Used to infer `se` and `df` when missing.
* `df`: Numeric. Degrees of freedom (optional). If not provided and `n` is available, it will be inferred.
* `p`: Numeric. p-value (optional). If not provided, it will be inferred from other inputs.
* `ci`: Numeric vector of length 2. Confidence interval for the correlation coefficient (on the raw correlation scale, not Fisher z).
* `one_sided`: Logical. Whether the test is one-sided (default is `FALSE`). Affects p-value and confidence interval calculation.
* `digits`: Integer. Number of significant digits for rounding results (default = `3`).
* `statistic`: Numeric. Test statistic (usually a t- or z-value). Can be used to infer `se` if missing.
* `conf.level`: Numeric between 0 and 1. Confidence level for the confidence interval (default is `0.95`).
* `attr`: Logical; if TRUE, attaches approximation messages as attributes (default TRUE).

## Details

The function uses Fisher's r-to-z transformation (`atanh()`) for inferential computations and back-transforms using `tanh()`.
If only a subset of values is provided (e.g., only `r` and `statistic`), the function attempts to infer the rest.
In two-sample cases, the difference between Fisher z-transformed correlations is used as the estimate, and the standard error is derived accordingly.
Informative notes are printed to indicate any approximations or inferred values used during the calculation.

## Value

A `data.frame` with the back-calculated statistics including Estimate, SE,
test statistic (t or z), degrees of freedom (df), p-value, and confidence interval bounds.
The output has class `"backcalc"` and contains attribute
`"Approximations"` if `attr = TRUE`.

## Examples

```r
# One-sample: r + p-value + df (infer se, CI)
backcalc_corrs(r = 0.52, p = 0.02, df = 18)

# Two-sample: Two correlations + unequal n (infer difference, se, df, p, CI)
backcalc_corrs(r = c(0.70, 0.50), n = c(40, 25))

# Insufficient information: Two correlations but missing sample sizes (error)
backcalc_corrs(r = c(0.3, 0.5))
```

# `backcalc_means`: Backcalculate Missing Inferential Statistics for Means

## Description

This function reconstructs inferential statistics related to means from partial information.
It can estimate standard errors, confidence intervals, p-values, test statistics (t or z), degrees of freedom,
and point estimates when some components are missing, supporting one-sample, paired, and two-sample cases.

## Usage

```r
backcalc_means(
  m = NULL,
  se = NULL,
  sd = NULL,
  n = NULL,
  df = NULL,
  p = NULL,
  ci = NULL,
  statistic = NULL,
  paired = FALSE,
  one_sided = FALSE,
  conf.level = 0.95,
  digits = 3,
  attr = TRUE
)
```

## Arguments

* `m`: Numeric vector of means or mean differences. For two-sample cases, provide
a vector of length 2 representing group means.
* `se`: Numeric vector of standard errors corresponding to the means or difference.
* `sd`: Numeric vector of standard deviations for each group.
* `n`: Numeric vector of sample sizes for each group.
* `df`: Degrees of freedom associated with the estimate(s).
* `p`: P-value(s) associated with the test statistic.
* `ci`: Numeric vector of length 2 specifying a confidence interval (lower and upper bounds).
* `statistic`: Test statistic value (e.g., t or z statistic).
* `paired`: Logical indicating whether the comparison is paired (default is FALSE).
* `one_sided`: Logical indicating whether a one-sided test is used (default is FALSE).
* `conf.level`: Confidence level for intervals (default 0.95).
* `digits`: Number of digits to round the output statistics (default 3).
* `attr`: Logical; if TRUE, attaches approximation messages as attributes (default TRUE).

## Details

The function handles both one- and two-sample cases and calculates missing values using
standard formulas and approximations. It supports pooled and Welch-Satterthwaite
degrees of freedom, approximation of SE from SD and sample size, estimation from CI
width, and estimation of test statistics and p-values. Messages about assumptions
and approximations are stored as attributes.

## Value

A `data.frame` with the back-calculated statistics including Estimate, SE,
test statistic (t or z), degrees of freedom (df), p-value, and confidence interval bounds.
The output has class `"backcalc"` and contains attribute
`"Approximations"` if `attr = TRUE`.

## Examples

```r
# One-sample example: Mean, SE, and sample size given (uses t-distribution)
backcalc_means(m = 25.4, se = 2.1, n = 30)

# Two-sample example: Means, SDs, and sample sizes given (Welch t-test inference)
backcalc_means(m = c(15, 12), sd = c(4, 5), n = c(40, 35))

# Insufficient input example: Mean provided but no SE, p-value, or CI (warns user)
backcalc_means(m = 25)
```

# `backcalc_medians`: Backcalculate Missing Inferential Statistics for Medians

## Description

This function estimates standard errors, test statistics, p-values, confidence intervals,
and degrees of freedom for median-based statistics using summary-level data.
It supports robust measures of spread such as interquartile range (IQR), median absolute deviation (MAD),
and range, applying normal approximation for inference without relying on parametric assumptions.
When parametric inputs (e.g., standard deviations) are provided, t-distribution based inference is used.

## Usage

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

## Arguments

* `m`: Numeric scalar or vector. Median(s) of the group(s). For two-sample comparisons, provide a length-2 vector.
* `se`: Numeric scalar or vector. Standard error(s) of the estimate(s).
* `sd`: Numeric scalar or vector. Standard deviation(s) of the group(s).
* `n`: Numeric scalar or vector. Sample size(s).
* `df`: Numeric scalar. Degrees of freedom for t-distribution inference.
* `p`: Numeric scalar. p-value for the test statistic.
* `ci`: Numeric vector of length 2. Confidence interval bounds (lower, upper).
* `statistic`: Numeric scalar. Test statistic (z or t).
* `iqr`: Numeric scalar or vector. Interquartile range(s).
* `mad`: Numeric scalar or vector. Median absolute deviation(s).
* `range`: Numeric scalar or vector. Range(s) of the data.
* `paired`: Logical. Whether the data are paired (default FALSE).
* `one_sided`: Logical. Whether the test is one-sided (default FALSE).
* `conf.level`: Numeric scalar. Confidence level for intervals (default 0.95).
* `digits`: Integer. Number of decimal places to round results (default 3).
* `attr`: Logical; if TRUE, attaches approximation messages as an attribute (default TRUE).

## Details

The function supports inference for medians using IQR, MAD, or range to approximate SE
with normal-based confidence intervals and z-tests. When parametric inputs like SD and sample size
are given without robust measures, Welch's t-test approximation is used.

## Value

A `data.frame` with the back-calculated statistics including Estimate, SE,
test statistic (t or z), degrees of freedom (df), p-value, and confidence interval bounds.
The output has class `"backcalc"` and contains attribute
`"Approximations"` if `attr = TRUE`.

## Examples

```r
# One-sample: Median, IQR, and sample size
backcalc_medians(m = 50, iqr = 20, n = 30)

# Two-sample: Group medians, MAD, and sample size
backcalc_medians(m = c(75, 68), mad = 9, n = 40)

# Insufficient info: Only median and sample size, no dispersion
backcalc_medians(m = c(52, 49), n = 30)
```

# `backcalc_props`: Backcalculate Missing Inferential Statistics for Proportions

## Description

This function reconstructs inferential statistics related to proportions from partial information.
It can estimate standard errors, confidence intervals, p-values, test statistics (t or z), degrees of freedom,
and point estimates when some components are missing, supporting one-sample, paired, and two-sample cases.

## Usage

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

## Arguments

* `prop`: Numeric vector of proportion(s). For one-sample, a single value; for two-sample, a vector of length 2.
* `se`: Standard error of the estimate, if known.
* `n`: Sample size(s). A single value for one-sample tests, or a vector of length 2 for two-sample tests.
* `x`: Count(s) of "successes" (used to compute proportions). A single value or vector of length 2.
* `ci`: Confidence interval (numeric vector of length 2: lower and upper limits), if already known.
* `p`: P-value for the test statistic. If not provided, it will be computed when possible.
* `one_sided`: Logical. Is the test one-sided? Defaults to `FALSE` (i.e., two-sided).
* `digits`: Number of decimal places to round results. Default is 3.
* `interval_type`: Type of confidence interval. Currently unused (placeholder).
* `statistic`: Observed test statistic (z or t), if known.
* `df`: Degrees of freedom, required if using a t-statistic.
* `conf.level`: Confidence level for the confidence interval. Default is 0.95.
* `method`: Character. Either `"wald"` (default) for normal approximations or `"exact"` for exact binomial CI (one-sample only).
* `continuity`: Logical. Whether to apply continuity correction in two-sample Wald test. Default is `FALSE`.
* `attr`: Logical; if TRUE, attaches approximation messages as attributes (default TRUE).

## Details

This function is designed to handle partial or minimal inputs by inferring missing values when possible.
For instance, if only a proportion and p-value are provided, it can back-calculate the sample size or test statistic.

## Value

A `data.frame` with the back-calculated statistics including Estimate, SE,
test statistic (t or z), degrees of freedom (df), p-value, and confidence interval bounds.
The output has class `"backcalc"` and contains attribute
`"Approximations"` if `attr = TRUE`.

## Examples

```r
# One-sample: Provide proportion and sample size only (basic one-sample proportion)
backcalc_props(prop = 0.4, n = 100)

# Two-sample: Provide two proportions and sample sizes only (difference in proportions)
backcalc_props(prop = c(0.55, 0.4), n = c(150, 130))

# Insufficient info: Provide n only, no prop, counts, or se
backcalc_props(n = 100)
```

# `backcalc_ratios`: Backcalculate Inferential Statistics for Ratio Measures

## Description

This function reconstructs inferential statistics (e.g., SE, test statistic,
p-value, confidence interval) for ratio-type measures (such as odds ratios or risk ratios)
using the log transformation internally. The function allows flexible input and determines
the appropriate test type (z or t) based on the presence of degrees of freedom.

## Usage

```r
backcalc_ratios(
  ratio = NULL,
  se = NULL,
  n = NULL,
  df = NULL,
  p = NULL,
  ci = NULL,
  statistic = NULL,
  one_sided = FALSE,
  digits = 3,
  conf.level = 0.95,
  attr = TRUE
)
```

## Arguments

* `ratio`: Numeric scalar (for one group) or numeric vector of length 2 (for comparison).
When a vector of two ratios is supplied, the function compares them via their log difference.
* `se`: Numeric. Standard error on the log scale. Can be a scalar or length 2 (for two-group case).
* `n`: Integer. Sample size (used to infer degrees of freedom if `df` not given).
* `df`: Numeric. Degrees of freedom. Scalar (for t-test) or length 2 (for Welch’s approximation).
* `p`: Numeric. p-value (one-sided or two-sided). Used only for display if provided.
* `ci`: Numeric vector of length 2. Confidence interval (on the ratio scale). Used to infer SE if SE not provided.
* `statistic`: Numeric. t or z test statistic. Used to infer SE if not supplied.
* `one_sided`: Logical. Whether the hypothesis test is one-sided. Default is `FALSE`.
* `digits`: Integer. Number of digits to round outputs to. Default is `3`.
* `conf.level`: Numeric. Confidence level used to compute interval. Default is `0.95`.
* `attr`: Logical; if TRUE, attaches approximation messages as attributes (default TRUE).

## Details

This function works on the log-transformed scale of ratios. It supports partial information
input and will infer missing values when possible. In the two-sample case, if both SEs and
dfs are provided, a Welch–Satterthwaite approximation is used to compute the test df.
If insufficient information is provided (e.g., no SE, CI, or statistic), the function will halt.
When possible, missing pieces are backcalculated using available data.

## Value

A `data.frame` with the back-calculated statistics including Estimate, SE,
test statistic (t or z), degrees of freedom (df), p-value, and confidence interval bounds.
The output has class `"backcalc"` and contains attribute
`"Approximations"` if `attr = TRUE`.

## Examples

```r
# One-sample example: Ratio and SE provided (no inference needed)
backcalc_ratios(ratio = 2.5, se = 0.2)

# Two-sample example: Ratios, SEs, and dfs provided (Welch-Satterthwaite df calculation)
backcalc_ratios(ratio = c(3.2, 1.9), se = c(0.25, 0.15), df = c(25, 30))

# Insufficient information example: Ratio provided but no SE, statistic, or CI
backcalc_ratios(ratio = 2.3)
```

# `backcalc_standardized_means`: Backcalculate Standardized Mean Differences (Effect Sizes)

## Description

This function reconstructs standardized mean differences and related effect size statistics
from various combinations of inputs. It supports Cohen's d, Hedges' g, and Glass's delta,
along with confidence intervals and test statistics for one-sample, paired, and two-sample designs.

## Usage

```r
backcalc_standardized_means(
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

## Arguments

* `d`: Numeric. Standardized mean difference (Cohen's d or similar). For two-sample cases,
provide a single value representing the difference between groups.
* `m`: Numeric vector of means or mean differences. For two-sample cases, provide
a vector of length 2 representing group means.
* `sd`: Numeric vector of standard deviations for each group.
* `n`: Numeric vector of sample sizes for each group.
* `se`: Numeric. Standard error of the mean difference (unstandardized).
* `statistic`: Numeric. Test statistic value (e.g., t-statistic).
* `p`: Numeric. P-value associated with the test statistic.
* `ci_d`: Numeric vector of length 2. Confidence interval for the effect size (Cohen's d).
* `paired`: Logical. Whether the comparison is paired (default is FALSE).
* `one_sided`: Logical. Whether a one-sided test is used (default is FALSE).
* `type`: Character. Type of effect size to compute: "d" (Cohen's d, default),
"g" (Hedges' g, bias-corrected), or "delta" (Glass's delta, uses control group SD).
* `control_sd`: Numeric. For type = "delta", specify which group's SD to use as control.
Default is 1 (first group).
* `conf.level`: Numeric. Confidence level for intervals (default 0.95).
* `digits`: Integer. Number of digits to round the output (default 3).
* `attr`: Logical. If TRUE, attaches approximation messages as attributes (default TRUE).

## Details

The function supports multiple effect size types:

* ***Cohen's d***: Standardized by the pooled standard deviation
(or average SD for unequal variances).
* ***Hedges' g***: Bias-corrected version of Cohen's d, adjusting for small samples.
* ***Glass's delta***: Standardized by the control or reference group's SD only.

Confidence intervals for effect sizes use noncentral t-distributions when appropriate.

## Value

A `data.frame` with effect size statistics including the standardized estimate,
standard error (of the effect size), confidence interval, and related statistics.
The output has class `"backcalc"` and contains attribute
`"Approximations"` if `attr = TRUE`.

## Examples

```r
# One-sample: Calculate d from mean, SD, and sample size
backcalc_standardized_means(m = 2.5, sd = 4, n = 25)

# Two-sample: Calculate d from group means, SDs, and sample sizes
backcalc_standardized_means(m = c(15, 12), sd = c(4, 5), n = c(40, 35))

# Backcalculate d from t-statistic and sample size
backcalc_standardized_means(statistic = 2.5, n = 50)

# Two-sample with Hedges' g (bias-corrected)
backcalc_standardized_means(m = c(15, 12), sd = c(4, 5), n = c(40, 35), type = "g")
```

# `print.backcalc`: Custom Print Method for `backcalc` Objects

## Description

This function provides a tailored print method for objects of class `backcalc`.
It optionally displays additional attributes such as notes and approximation messages
alongside the main data output.

## Usage

```r
# S3 method for backcalc
print(x, ...)
```

## Arguments

* `x`: An object of class `backcalc`, typically a matrix or data frame with
attached attributes "Notes" and "Approximations".
* `...`: Additional arguments passed to the base `print` function.

## Details

If the attribute `"attr"` is set to `TRUE` on the object, this method
prints the object along with its "Notes" and "Approximations" attributes.
Otherwise, it prints only the main data content.

## Value

Invisibly returns the original object `x`.

