# [`backcalc`](https://github.com/cwendorf/backcalc/)

## Coefficient Examples

- [Unstandardized Beta Cases](#unstandardized-beta-cases)
- [Standardized Beta and Conversion
  Cases](#standardized-beta-and-conversion-cases)
- [Insufficient Information Cases](#insufficient-information-cases)

------------------------------------------------------------------------

Each section of examples below progresses from more complete input to
less complete input, moving from miminal required inference to maximal
inference on the part of the function. Similarly, the structure
highlights the flexibility of the function across diverse study designs
and input constraints.

### Unstandardized Beta Cases

``` r
# 1. Basic: coefficient and SE provided, assume z-test
backcalc_coeffs(b = 0.5, se = 0.1)
```


            Estimate    SE     z df     p    LL    UL
    Outcome    0.500 0.100 5.000 NA 0.000 0.304 0.696

    Note(s):
    p-value computed from statistic.

``` r
# 2. Coefficient and p-value given, with df for t-test; infer SE and t
backcalc_coeffs(b = 1.2, p = 0.03, df = 28)
```


            Estimate    SE     t     df     p    LL    UL
    Outcome    1.200 0.525 2.286 28.000 0.030 0.125 2.275

    Note(s):
    Sample size inferred from df (n = df + 1).
    Statistic approximated from p-value and estimate.
    SE approximated from estimate and reconstructed statistic.

``` r
# 3. Estimate and confidence interval given with df; infer SE and t-statistic
backcalc_coeffs(b = 0.8, ci = c(0.2, 1.4), df = 45)
```


            Estimate    SE     t     df     p    LL    UL
    Outcome    0.800 0.298 2.685 45.000 0.010 0.200 1.400

    Note(s):
    Sample size inferred from df (n = df + 1).
    SE approximated from CI width.
    p-value computed from statistic.

``` r
# 4. Estimate, SE, and sample size given; infer df and CI
backcalc_coeffs(b = -0.7, se = 0.2, n = 50)
```


            Estimate    SE      t     df     p     LL     UL
    Outcome   -0.700 0.200 -3.500 49.000 0.001 -1.102 -0.298

    Note(s):
    df inferred from sample size (df = n - 1).
    p-value computed from statistic.

``` r
# 5. Estimate and test statistic provided directly with df; infer SE and p
backcalc_coeffs(b = 0.9, statistic = 2.3, df = 30)
```


            Estimate    SE     t     df     p    LL    UL
    Outcome    0.900 0.391 2.300 30.000 0.029 0.101 1.699

    Note(s):
    Sample size inferred from df (n = df + 1).
    SE approximated from estimate and provided statistic.
    p-value computed from statistic.

### Standardized Beta and Conversion Cases

``` r
# 6. Standardized beta and SE_std given; z-test assumed
backcalc_coeffs(std_beta = 0.25, se_std = 0.04)
```


            Estimate    SE     z df     p    LL    UL
    Outcome    0.250 0.040 6.250 NA 0.000 0.172 0.328

    Note(s):
    p-value computed from statistic.

``` r
# 7. Unstandardized beta and SDs given; infer standardized beta and SE_std
backcalc_coeffs(b = 1.1, se = 0.3, sd_x = 2.5, sd_y = 5)
```


            Estimate    SE     z df     p    LL    UL
    Outcome    0.550 0.150 3.667 NA 0.000 0.256 0.844

    Note(s):
    Standardized beta approximated from unstandardized beta and standard deviations.
    SE of standardized beta approximated from unstandardized SE and SDs.
    p-value computed from statistic.

``` r
# 8. Standardized beta, p-value, and df given; infer SE_std and t-statistic
backcalc_coeffs(std_beta = 0.3, p = 0.02, df = 25)
```


            Estimate    SE     t     df     p    LL    UL
    Outcome    0.300 0.121 2.485 25.000 0.020 0.051 0.549

    Note(s):
    Sample size inferred from df (n = df + 1).
    Statistic approximated from p-value and estimate.
    SE approximated from estimate and reconstructed statistic.

``` r
# 9. Provide standardized beta, CI, and sample size; infer SE_std and p
backcalc_coeffs(std_beta = 0.4, ci = c(0.1, 0.7), n = 40)
```


            Estimate    SE     t     df     p    LL    UL
    Outcome    0.400 0.148 2.697 39.000 0.010 0.100 0.700

    Note(s):
    df inferred from sample size (df = n - 1).
    SE approximated from CI width.
    p-value computed from statistic.

``` r
# 10. Standardized beta and statistic given with df; infer SE_std and p
backcalc_coeffs(std_beta = 0.35, statistic = 2.1, df = 29)
```


            Estimate    SE     t     df     p    LL    UL
    Outcome    0.350 0.167 2.100 29.000 0.045 0.009 0.691

    Note(s):
    Sample size inferred from df (n = df + 1).
    SE approximated from estimate and provided statistic.
    p-value computed from statistic.

### Insufficient Information Cases

``` r
# 11. Only p-value given, no estimate or SE
backcalc_coeffs(p = 0.05)
```


    Insufficient Input:
    Cannot estimate coefficient or SE.

``` r
# 12. Only SE given, no estimate
backcalc_coeffs(se = 0.15)
```


    Insufficient Input:
    Cannot estimate coefficient or SE.

``` r
# 13. Only confidence interval lower bound (invalid length)
backcalc_coeffs(ci = 0.3)
```


    Insufficient Input:
    CI must be a numeric vector of length 2.
    Cannot estimate coefficient or SE.

``` r
# 14. Only standardized SE given, no standardized beta
backcalc_coeffs(se_std = 0.05)
```


    Insufficient Input:
    Cannot estimate coefficient or SE.

``` r
# 15. Only sample size given, no estimate, SE, or p
backcalc_coeffs(n = 100)
```


    Insufficient Input:
    Cannot estimate coefficient or SE.
