# [`backcalc`](https://github.com/cwendorf/backcalc)

## Backcalculate Inferential Statistics for Ratio Measures

**Aliases:**

- `backcalc_ratios`

### Description

This function reconstructs inferential statistics (e.g., SE, test statistic,
p-value, confidence interval) for ratio-type measures (such as odds ratios or risk ratios)
using the log transformation internally. The function allows flexible input and determines
the appropriate test type (z or t) based on the presence of degrees of freedom.

### Usage

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

### Arguments

- **`ratio`**: Numeric scalar (for one group) or numeric vector of length 2 (for comparison).
When a vector of two ratios is supplied, the function compares them via their log difference.
- **`se`**: Numeric. Standard error on the log scale. Can be a scalar or length 2 (for two-group case).
- **`n`**: Integer. Sample size (used to infer degrees of freedom if df not given).
- **`df`**: Numeric. Degrees of freedom. Scalar (for t-test) or length 2 (for Welch’s approximation).
- **`p`**: Numeric. p-value (one-sided or two-sided). Used only for display if provided.
- **`ci`**: Numeric vector of length 2. Confidence interval (on the ratio scale). Used to infer SE if SE not provided.
- **`statistic`**: Numeric. t or z test statistic. Used to infer SE if not supplied.
- **`one_sided`**: Logical. Whether the hypothesis test is one-sided. Default is FALSE.
- **`digits`**: Integer. Number of digits to round outputs to. Default is 3.
- **`conf.level`**: Numeric. Confidence level used to compute interval. Default is 0.95.
- **`attr`**: Logical; if TRUE, attaches approximation messages as attributes (default TRUE).

### Details

This function works on the log-transformed scale of ratios. It supports partial information
input and will infer missing values when possible. In the two-sample case, if both SEs and
dfs are provided, a Welch–Satterthwaite approximation is used to compute the test df.

If insufficient information is provided (e.g., no SE, CI, or statistic), the function will halt.
When possible, missing pieces are backcalculated using available data.

### Value

A data.frame with the back-calculated statistics including Estimate, SE,
test statistic (t or z), degrees of freedom (df), p-value, and confidence interval bounds.
The output has class "backcalc" and contains attribute
"Approximations" if attr = TRUE.

### Examples

```r
# One-sample example: Ratio and SE provided (no inference needed)
backcalc_ratios(ratio = 2.5, se = 0.2)

# Two-sample example: Ratios, SEs, and dfs provided (Welch-Satterthwaite df calculation)
backcalc_ratios(ratio = c(3.2, 1.9), se = c(0.25, 0.15), df = c(25, 30))

# Insufficient information example: Ratio provided but no SE, statistic, or CI
backcalc_ratios(ratio = 2.3)
```

