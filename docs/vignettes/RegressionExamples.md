# [`backcalc`](https://github.com/cwendorf/backcalc/)

## Multiple Regression Examples

Each section of examples below progresses from more complete input to
less complete input, moving from minimal required inference to maximal
inference on the part of the function. Similarly, the structure
highlights the flexibility of the function across diverse input
constraints for multiple regression coefficients.

- [Unstandardized Coefficients Cases](#unstandardized-coefficients-cases)
- [Standardized Coefficients Cases](#standardized-coefficients-cases)
- [Insufficient Information Cases](#insufficient-information-cases)

------------------------------------------------------------------------

### Unstandardized Coefficients Cases

``` r
# 1. Full info: unstandardized coefficients, standard errors, and df
backcalc_multreg(
  intercept = 3.2, intercept_se = 0.5,
  b = c(0.4, 0.8), se = c(0.1, 0.15),
  df = 50
)
```


              Estimate    SE     t     df     p    LL    UL
    Intercept    3.200 0.500 6.400 50.000 0.000 2.196 4.204
    X1           0.400 0.100 4.000 50.000 0.000 0.199 0.601
    X2           0.800 0.150 5.333 50.000 0.000 0.499 1.101

    Notes:
    Intercept: p-value computed from statistic.
    X1: p-value computed from statistic.
    X2: p-value computed from statistic. 

``` r
# 2. Unstandardized coefficients, SEs, and standard deviations; infer standardized coefficients
backcalc_multreg(
  intercept = 1.8, intercept_se = 0.4,
  b = c(0.7, 1.2, 0.3),
  se = c(0.15, 0.2, 0.08),
  sd_x = c(1.5, 2.2, 0.8),
  sd_y = 3,
  n = 110
)
```


              Estimate    SE     t      df     p    LL    UL
    Intercept    1.800 0.400 4.500 109.000 0.000 1.007 2.593
    X1           0.350 0.075 4.667 109.000 0.000 0.201 0.499
    X2           0.880 0.147 6.000 109.000 0.000 0.589 1.171
    X3           0.080 0.021 3.750 109.000 0.000 0.038 0.122

    Notes:
    Intercept: p-value computed from statistic.
    X1: Standardized beta approximated from unstandardized beta and SDs. SE of standardized beta approximated from unstandardized SE and SDs. p-value computed from statistic.
    X2: Standardized beta approximated from unstandardized beta and SDs. SE of standardized beta approximated from unstandardized SE and SDs. p-value computed from statistic.
    X3: Standardized beta approximated from unstandardized beta and SDs. SE of standardized beta approximated from unstandardized SE and SDs. p-value computed from statistic. 

``` r
# 3. Unstandardized coefficients with SEs and sample size; infer df and t-test
backcalc_multreg(
  intercept = 3.1, intercept_se = 0.55,
  b = c(0.6, 0.8),
  se = c(0.15, 0.12),
  n = 85
)
```


              Estimate    SE     t     df     p    LL    UL
    Intercept    3.100 0.550 5.636 84.000 0.000 2.006 4.194
    X1           0.600 0.150 4.000 84.000 0.000 0.302 0.898
    X2           0.800 0.120 6.667 84.000 0.000 0.561 1.039

    Notes:
    Intercept: p-value computed from statistic.
    X1: p-value computed from statistic.
    X2: p-value computed from statistic. 

``` r
# 4. Unstandardized coefficients with SEs and test statistics; no df, use z-test
backcalc_multreg(
  intercept = 3.1, intercept_se = 0.55,
  b = c(0.6, 0.8),
  se = c(0.15, 0.12),
  statistic = c(4, 6)
)
```


              Estimate    SE     t df     p    LL    UL
    Intercept    3.100 0.550 5.636 NA 0.000 2.022 4.178
    X1           0.600 0.150 4.000 NA 0.000 0.306 0.894
    X2           0.800 0.120 6.000 NA 0.000 0.565 1.035

    Notes:
    Intercept: p-value computed from statistic.
    X1: p-value computed from statistic.
    X2: p-value computed from statistic. 

``` r
# 5. Unstandardized coefficients and p-values given; infer SEs and test statistics
backcalc_multreg(
  intercept = 2.4, intercept_se = 0.35,
  b = c(0.4, 0.7),
  p = c(0.04, 0.01),
  n = 85
)
```


              Estimate    SE     t     df     p    LL    UL
    Intercept    2.400 0.350 6.857 84.000 0.000 1.704 3.096
    X1           0.400 0.192 2.086 84.000 0.040 0.019 0.781
    X2           0.700 0.266 2.636 84.000 0.010 0.172 1.228

    Notes:
    Intercept: p-value computed from statistic.
    X1: Statistic approximated from p-value and estimate. SE approximated from estimate and reconstructed statistic.
    X2: Statistic approximated from p-value and estimate. SE approximated from estimate and reconstructed statistic. 

### Standardized Coefficients Cases

``` r
# 6. Standardized betas with SEs and sample size
backcalc_multreg(
  intercept = 3.2, intercept_se = 0.5,
  std_beta = c(0.3, 0.5), se_std = c(0.1, 0.12),
  n = 100
)
```


              Estimate    SE     t     df     p    LL    UL
    Intercept    3.200 0.500 6.400 99.000 0.000 2.208 4.192
    X1           0.300 0.100 3.000 99.000 0.003 0.102 0.498
    X2           0.500 0.120 4.167 99.000 0.000 0.262 0.738

    Notes:
    Intercept: p-value computed from statistic.
    X1: p-value computed from statistic.
    X2: p-value computed from statistic. 

``` r
# 7. Standardized betas with SEs and degrees of freedom
backcalc_multreg(
  intercept = 2.5, intercept_se = 0.3,
  std_beta = c(0.25, 0.55, 0.1),
  se_std = c(0.07, 0.1, 0.05),
  df = 90
)
```


              Estimate    SE     t     df     p    LL    UL
    Intercept    2.500 0.300 8.333 90.000 0.000 1.904 3.096
    X1           0.250 0.070 3.571 90.000 0.001 0.111 0.389
    X2           0.550 0.100 5.500 90.000 0.000 0.351 0.749
    X3           0.100 0.050 2.000 90.000 0.049 0.001 0.199

    Notes:
    Intercept: p-value computed from statistic.
    X1: p-value computed from statistic.
    X2: p-value computed from statistic.
    X3: p-value computed from statistic. 

``` r
# 8. Unstandardized coefficients with SEs and standard deviations; infer standardized betas
backcalc_multreg(
  intercept = 2.0, intercept_se = 0.6,
  b = c(1.1, 0.5), se = c(0.3, 0.2),
  sd_x = c(2.5, 3), sd_y = 5,
  n = 120
)
```


              Estimate    SE     t      df     p    LL    UL
    Intercept    2.000 0.600 3.333 119.000 0.001 0.812 3.188
    X1           0.550 0.150 3.667 119.000 0.000 0.253 0.847
    X2           0.300 0.120 2.500 119.000 0.014 0.062 0.538

    Notes:
    Intercept: p-value computed from statistic.
    X1: Standardized beta approximated from unstandardized beta and SDs. SE of standardized beta approximated from unstandardized SE and SDs. p-value computed from statistic.
    X2: Standardized beta approximated from unstandardized beta and SDs. SE of standardized beta approximated from unstandardized SE and SDs. p-value computed from statistic. 

``` r
# 9. Standardized betas with missing SEs; use unstandardized input for approximation
backcalc_multreg(
  intercept = 3.0, intercept_se = 0.45,
  std_beta = c(0.3, NA, 0.5),
  se_std = c(0.1, NA, 0.12),
  b = c(NA, 0.4, NA),
  se = c(NA, 0.15, NA),
  sd_x = c(2, 3, 2.5),
  sd_y = 5,
  n = 130
)
```


              Estimate    SE     t      df     p    LL    UL
    Intercept    3.000 0.450 6.667 129.000 0.000 2.110 3.890
    X1           0.300 0.100 3.000 129.000 0.003 0.102 0.498
    X2           0.240 0.090 2.667 129.000 0.009 0.062 0.418
    X3           0.500 0.120 4.167 129.000 0.000 0.263 0.737

    Notes:
    Intercept: p-value computed from statistic.
    X1: p-value computed from statistic.
    X2: Standardized beta approximated from unstandardized beta and SDs. SE of standardized beta approximated from unstandardized SE and SDs. p-value computed from statistic.
    X3: p-value computed from statistic. 

``` r
# 10. Intercept with confidence intervals for predictors and sample size; infer SEs
backcalc_multreg(
  intercept = 4.5, intercept_se = 0.4,
  ci = list(c(0.2, 0.8), c(0.1, 0.9)),
  n = 80
)
```


              Estimate    SE      t     df     p    LL    UL
    Intercept    4.500 0.400 11.250 79.000 0.000 3.704 5.296
    X1           0.500 0.151  3.317 79.000 0.001 0.200 0.800
    X2           0.500 0.201  2.488 79.000 0.015 0.100 0.900

    Notes:
    Intercept: p-value computed from statistic.
    X1: Estimate approximated as midpoint of CI. SE approximated from CI width. p-value computed from statistic.
    X2: Estimate approximated as midpoint of CI. SE approximated from CI width. p-value computed from statistic. 

### Insufficient Information Cases

``` r
# 11. Predictors with no estimates and no confidence intervals
backcalc_multreg(
  intercept = 2.0, intercept_se = 0.4,
  se = c(0.1, 0.15),
  n = 100
)
```


    Insufficient Input for X1:
    Cannot estimate coefficient or SE.

``` r
# 12. Standardized betas provided, but SEs missing with no sample size or df
backcalc_multreg(
  intercept = 1.8, intercept_se = 0.25,
  std_beta = c(0.2, 0.5, 0.3)
)
```


    Insufficient Input for X1:
    Cannot estimate coefficient or SE.

``` r
# 13. Intercept with unstandardized predictor, missing SE for second predictor
backcalc_multreg(
  intercept = 1.5, intercept_se = 0.25,
  b = c(0.5, NA), se = c(0.1, NA), p = c(NA, 0.05),
  n = 150
)
```


    Insufficient Input for X2:
    Cannot estimate coefficient or SE.

``` r
# 14. Intercept with predictor estimates only, missing SE information
backcalc_multreg(
  intercept = 2.0, intercept_se = 0.4,
  b = c(0.5, 0.8)
)
```


    Insufficient Input for X1:
    Cannot estimate coefficient or SE.

``` r
# 15. Intercept with predictor p-values only, no estimates or df provided
backcalc_multreg(
  intercept = 1.5, intercept_se = 0.3,
  p = c(0.05, 0.01)
)
```


    Insufficient Input for X1:
    Cannot estimate coefficient or SE.
