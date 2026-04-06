# [`backcalc`](https://github.com/cwendorf/backcalc)

## Backcalculate ANOVA

### Description

Handles between-subjects, within-subjects, factorial, and mixed designs, with optional
sphericity corrections. Works with multiple effects at once.

### Usage

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

### Arguments

- **`F`**: Numeric vector of F-statistics.
- **`df1`**: Numeric vector of numerator degrees of freedom.
- **`df2`**: Numeric vector of denominator degrees of freedom.
- **`p`**: Numeric vector of p-values.
- **`eta2`**: Numeric vector of partial eta-squared values.
- **`f`**: Numeric vector of Cohen's f values.
- **`design`**: Character vector: "between", "within", or "mixed".
- **`n`**: Numeric vector: total sample size(s) or vector of group Ns.
- **`levels`**: List: each element is an integer vector of factor levels for the effect.
- **`subjects`**: Numeric vector: number of subjects (for within/mixed designs).
- **`epsilon`**: Numeric vector: sphericity correction factors.
- **`effect`**: Character vector: "main" or "interaction".
- **`labels`**: Optional character vector of row labels (one per effect). If omitted,
names are derived from levels and effect type.
- **`conf.level`**: Confidence level for eta-squared CI.
- **`digits`**: Rounding digits for output.
- **`attr`**: Attach approximation notes as attributes.

### Value

A data.frame with one row per effect, containing F, df1, df2, p,
Cohen's f, partial eta-squared, and CI bounds for eta-squared. Attributes store approximation notes.

### Examples

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

