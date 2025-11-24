# [`backcalc`](https://github.com/cwendorf/backcalc/)

## ANOVA Examples

- [Full ANOVA Cases](#full-anova-cases)
- [Minimal Input Cases](#minimal-input-cases)
- [Insufficient Input Cases](#insufficient-input-cases)

------------------------------------------------------------------------

The sections below progress from more complete input to less complete
input, moving from miminal required inference to maximal inference on
the part of the function. Similarly, the structure highlights the
flexibility of the function across diverse study designs and input
constraints.

### Full ANOVA Cases

``` r
# 1. One-way between-subjects ANOVA (single factor)
backcalc_anova(
  F = 5.2,
  df1 = 2,
  df2 = 30,
  design = "between",
  levels = list(3),  # 3 levels for the single factor
  n = 33,
  labels = "Treatment"
)
```

                  F   df1    df2     p     f  eta2    LL    UL
    Treatment 5.200 2.000 30.000 0.012 0.589 0.257 0.002 0.218

    Notes:
    Treatment: Partial eta-squared computed from F, df1, df2.
    Treatment: Cohen's f computed from partial eta-squared.
    Treatment: CI for partial eta-squared is approximate.

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
    Factor(k=4) 4.800 2.250 24.750 0.015 0.661 0.304 0.003 0.272

    Notes:
    Factor(k=4): Applied sphericity correction ε = 0.75.
    Factor(k=4): Partial eta-squared computed from F, df1, df2.
    Factor(k=4): Cohen's f computed from partial eta-squared.
    Factor(k=4): CI for partial eta-squared is approximate.

``` r
# 3. Factorial between-subjects ANOVA (2×3)
backcalc_anova(
  F = c(6.4, 3.2, 2.5),
  df1 = NA, df2 = NA,
  design = "between",
  levels = list(c(2, 3), c(2, 3), c(2, 3)),
  n = 36,
  effect = c("main", "main", "interaction"),
  labels = c("FactorA", "FactorB", "A×B")
)
```

                F   df1    df2     p     f  eta2    LL    UL
    FactorA 6.400 1.000 30.000 0.017 0.462 0.176 0.000 0.157
    FactorB 3.200 1.000 30.000 0.084 0.327 0.096 0.000 0.157
    A×B     2.500 2.000 30.000 0.099 0.408 0.143 0.002 0.218

    Notes:
    FactorA: Partial eta-squared computed from F, df1, df2.
    FactorA: Cohen's f computed from partial eta-squared.
    FactorA: CI for partial eta-squared is approximate.
    FactorB: Partial eta-squared computed from F, df1, df2.
    FactorB: Cohen's f computed from partial eta-squared.
    FactorB: CI for partial eta-squared is approximate.
    A×B: Partial eta-squared computed from F, df1, df2.
    A×B: Cohen's f computed from partial eta-squared.
    A×B: CI for partial eta-squared is approximate.

``` r
# 4. Mixed design (Between × Within)
backcalc_anova(
  F = c(5.5, 4.2, 1.8),
  df1 = NA, df2 = NA,
  design = c("between", "within", "mixed"),
  levels = list(c(2), c(3), c(2, 3)),
  n = 20,               # total sample size between groups
  subjects = 10,        # subjects per cell for within factor
  epsilon = c(1, 0.85, 0.85),
  effect = c("main", "main", "interaction"),
  labels = c("Group", "Time", "Group×Time")
)
```

                   F   df1    df2     p     f  eta2    LL    UL
    Group      5.500 1.000 18.000 0.031 0.553 0.234 0.000 0.249
    Time       4.200 1.700 15.300 0.040 0.683 0.318 0.002 0.359
    Group×Time 1.800 1.700 15.300 0.201 0.447 0.167 0.002 0.359

    Notes:
    Group: Partial eta-squared computed from F, df1, df2.
    Group: Cohen's f computed from partial eta-squared.
    Group: CI for partial eta-squared is approximate.
    Time: Applied sphericity correction ε = 0.85.
    Time: Partial eta-squared computed from F, df1, df2.
    Time: Cohen's f computed from partial eta-squared.
    Time: CI for partial eta-squared is approximate.
    Group×Time: Applied sphericity correction ε = 0.85.
    Group×Time: Partial eta-squared computed from F, df1, df2.
    Group×Time: Cohen's f computed from partial eta-squared.
    Group×Time: CI for partial eta-squared is approximate.

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
    Effect1: F-statistic computed from partial eta-squared.
    Effect1: Cohen's f computed from partial eta-squared.
    Effect1: CI for partial eta-squared is approximate.

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
    Effect1: F-statistic approximated from p-value.
    Effect1: Partial eta-squared computed from F, df1, df2.
    Effect1: Cohen's f computed from partial eta-squared.
    Effect1: CI for partial eta-squared is approximate.

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
    Effect1: Partial eta-squared computed from F, df1, df2.
    Effect1: Cohen's f computed from partial eta-squared.
    Effect1: CI for partial eta-squared is approximate.

### Insufficient Input Cases

``` r
# 8. Only dfs provided (cannot compute F, p, or effect sizes)
backcalc_anova(
  df1 = 2,
  df2 = 30
)
```

    Insufficient Input:
    Effect1: Provide F, p, partial eta-squared (eta2), or Cohen's f along with df1 and df2.

``` r
# 9. Only F provided (missing dfs so p and effect sizes cannot be derived)
backcalc_anova(
  F = 5.2
)
```

    Insufficient Input:
    Effect1: Numerator (df1) and denominator (df2) degrees of freedom required with F.

``` r
# 10. Only eta2 provided (without dfs cannot derive F or CI)
backcalc_anova(
  eta2 = 0.12
)
```

    Insufficient Input:
    Effect1: df1 and df2 required to compute F, p, and CI from partial eta-squared.
