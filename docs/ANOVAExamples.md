## ANOVA Examples

- [Full ANOVA Cases](#full-anova-cases)
- [Minimal Input Cases](#minimal-input-cases)

------------------------------------------------------------------------

The sections below progress from more complete input to less complete
input, moving from miminal required inference to maximal inference on
the part of the function. Similarly, the structure highlights the
flexibility of the function across diverse study designs and input
constraints.

### Full ANOVA Cases

``` r
# 1. One-way between-subjects ANOVA
backcalc_anova(
  F = c(5.2, NA),         # First effect has F, second only p
  df1 = c(2, NA),
  df2 = c(30, NA),
  p = c(NA, 0.01),
  design = "between",
  levels = list(c(3), c(4)),  # 3 levels, 4 levels
  n = 33
)
```


                F   df1    df2     p     f  eta2    LL    UL
    Effect1 5.200 2.000 30.000 0.012 0.589 0.257 0.002 0.218
    Effect2 4.538 3.000 29.000 0.010 0.685 0.319 0.007 0.272

    Note(s):
    F-statistic approximated from p-value.
    Partial eta-squared computed from F, df1, df2.
    Cohen's f computed from partial eta-squared.
    CI for partial eta-squared is approximate.

``` r
# 2. Repeated-measures ANOVA (single factor) with sphericity correction
backcalc_anova(
  F = 4.8,
  df1 = NA, df2 = NA,
  design = "within",
  levels = list(c(4)),
  subjects = 12,
  epsilon = 0.75
)
```


                F   df1    df2     p     f  eta2    LL    UL
    Effect1 4.800 2.250 24.750 0.015 0.661 0.304 0.003 0.272

    Note(s):
    Applied sphericity correction ε = 0.75.
    Partial eta-squared computed from F, df1, df2.
    Cohen's f computed from partial eta-squared.
    CI for partial eta-squared is approximate.

``` r
# 3. Factorial between-subjects ANOVA (2×3)
backcalc_anova(
  F = c(6.4, 3.2, 2.5),
  df1 = NA, df2 = NA,
  design = "between",
  levels = list(c(2, 3), c(2, 3), c(2, 3)),
  n = 36,
  effect = c("main", "main", "interaction")
)
```


                F   df1    df2     p     f  eta2    LL    UL
    Effect1 6.400 1.000 30.000 0.017 0.462 0.176 0.000 0.157
    Effect2 3.200 1.000 30.000 0.084 0.327 0.096 0.000 0.157
    Effect3 2.500 2.000 30.000 0.099 0.408 0.143 0.002 0.218

    Note(s):
    Partial eta-squared computed from F, df1, df2.
    Cohen's f computed from partial eta-squared.
    CI for partial eta-squared is approximate.

``` r
# 4. Mixed design (Between × Within)
backcalc_anova(
  F = c(5.5, 4.2, 1.8),
  df1 = NA, df2 = NA,
  design = c("between", "within", "mixed"),
  levels = list(c(2), c(3), c(2, 3)),
  n = 20,               # per between group
  subjects = 10,        # per condition for within
  epsilon = c(1, 0.85, 0.85),
  effect = c("main", "main", "interaction")
)
```


                F   df1    df2     p     f  eta2    LL    UL
    Effect1 5.500 1.000 18.000 0.031 0.553 0.234 0.000 0.249
    Effect2 4.200 1.700 15.300 0.040 0.683 0.318 0.002 0.359
    Effect3 1.800 1.700 15.300 0.201 0.447 0.167 0.002 0.359

    Note(s):
    Applied sphericity correction ε = 0.85.
    Partial eta-squared computed from F, df1, df2.
    Cohen's f computed from partial eta-squared.
    CI for partial eta-squared is approximate.

### Minimal Input Cases

``` r
# 5. Only η²p and df given, reconstruct F and p
backcalc_anova(
  eta2 = 0.15,
  df1 = 2,
  df2 = 30
)
```


                F   df1    df2  p     f  eta2    LL    UL
    Effect1 2.647 2.000 30.000 NA 0.420 0.150 0.002 0.218

    Note(s):
    F-statistic computed from partial eta-squared.
    Cohen's f computed from partial eta-squared.
    CI for partial eta-squared is approximate.

``` r
# 6. Only p and dfs given, reconstruct F and η²p
backcalc_anova(
  p = 0.012,
  df1 = 1,
  df2 = 28
)
```


                F   df1    df2     p     f  eta2    LL    UL
    Effect1 7.219 1.000 28.000 0.012 0.508 0.205 0.000 0.167

    Note(s):
    F-statistic approximated from p-value.
    Partial eta-squared computed from F, df1, df2.
    Cohen's f computed from partial eta-squared.
    CI for partial eta-squared is approximate.

``` r
# 7. Only F and dfs given, calculate p and η²p
backcalc_anova(
  F = 4.5,
  df1 = 1,
  df2 = 22
)
```


                F   df1    df2     p     f  eta2    LL    UL
    Effect1 4.500 1.000 22.000 0.045 0.452 0.170 0.000 0.208

    Note(s):
    Partial eta-squared computed from F, df1, df2.
    Cohen's f computed from partial eta-squared.
    CI for partial eta-squared is approximate.
