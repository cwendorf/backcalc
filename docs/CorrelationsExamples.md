# [`backcalc`](https://github.com/cwendorf/backcalc/)

## Correlations Examples

Each section of examples below progresses from more complete input to
less complete input, moving from miminal required inference to maximal
inference on the part of the function. Similarly, the structure
highlights the flexibility of the function across diverse study designs
and input constraints.

- [One Sample Cases](#one-sample-cases)
- [Two Sample Cases](#two-sample-cases)
- [Insufficient Information Cases](#insufficient-information-cases)

------------------------------------------------------------------------

### One Sample Cases

``` r
# 1. All info given: r, se, n (df inferred)
backcalc_corrs(r = 0.45, se = 0.1, n = 25)
```


            Estimate    SE     t     df     p    LL    UL
    Outcome    0.450 0.100 4.847 23.000 0.000 0.271 0.599

    Note(s):
    df approximated as n - 2.

``` r
# 2. r + p-value + df (infer se, CI)
backcalc_corrs(r = 0.52, p = 0.02, df = 18)
```


            Estimate    SE     t     df     p    LL    UL
    Outcome    0.520 0.226 2.552 18.000 0.020 0.102 0.782

    Note(s):
    SE approximated from p-value and df.

``` r
# 3. r + confidence interval + df (infer se, p)
backcalc_corrs(r = 0.35, ci = c(0.10, 0.55), df = 20)
```


            Estimate    SE     t     df     p    LL    UL
    Outcome    0.350 0.124 2.943 20.000 0.008 0.106 0.554

    Note(s):
    SE approximated from CI in Fisher z scale.

``` r
# 4. r + test statistic + df (infer se, p)
backcalc_corrs(r = 0.38, statistic = 2.2, df = 28)
```


            Estimate    SE     t     df     p    LL    UL
    Outcome    0.380 0.182 2.200 28.000 0.036 0.028 0.648

    Note(s):
    SE approximated from test statistic and df.

``` r
# 5. Only r + n (infer se, df, p, CI) with 90% confidence level
backcalc_corrs(r = 0.42, n = 40, conf.level = 0.90)
```


            Estimate    SE     t     df     p    LL    UL
    Outcome    0.420 0.164 2.723 38.000 0.010 0.169 0.620

    Note(s):
    df approximated as n - 2.
    SE approximated using 1 / sqrt(n - 3).

### Two Sample Cases

``` r
# 6. Two correlations + equal n (infer difference, se, df, p, CI)
backcalc_corrs(r = c(0.60, 0.40), n = c(30, 30))
```


            Estimate    SE     t     df     p     LL    UL
    Outcome    0.263 0.272 0.990 27.000 0.331 -0.281 0.679

    Note(s):
    df approximated as min(n) - 3 for two-sample case.
    SE derived from Fisher z difference formula.

``` r
# 7. Two correlations + unequal n (infer difference, se, df, p, CI)
backcalc_corrs(r = c(0.70, 0.50), n = c(40, 25))
```


            Estimate    SE     t     df     p     LL    UL
    Outcome    0.308 0.269 1.181 22.000 0.250 -0.236 0.705

    Note(s):
    df approximated as min(n) - 3 for two-sample case.
    SE derived from Fisher z difference formula.

``` r
# 8. Difference in correlation + p-value + df (infer se, statistic, CI)
backcalc_corrs(r = 0.18, p = 0.03, df = 45)
```


            Estimate    SE     t     df     p    LL    UL
    Outcome    0.180 0.081 2.241 45.000 0.030 0.018 0.332

    Note(s):
    SE approximated from p-value and df.

``` r
# 9. Two correlations + se + equal n (infer df, p, statistic)
backcalc_corrs(r = c(0.52, 0.34), se = 0.12, n = c(35, 35))
```


            Estimate    SE     t     df     p     LL    UL
    Outcome    0.219 0.120 1.852 32.000 0.073 -0.022 0.436

    Note(s):
    df approximated as min(n) - 3 for two-sample case.

``` r
# 10. Two correlations + CI + unequal n (infer se, df, p, statistic)
backcalc_corrs(r = c(0.65, 0.48), ci = c(0.50, 0.70), n = c(50, 40))
```


            Estimate    SE     t     df     p     LL    UL
    Outcome    0.247 0.220 1.148 37.000 0.258 -0.191 0.603

    Note(s):
    df approximated as min(n) - 3 for two-sample case.
    SE derived from Fisher z difference formula.

### Insufficient Information Cases

``` r
# 11. Missing correlation coefficient r
backcalc_corrs(se = 0.1, n = 20)
```


    Insufficient Input:
    Correlation coefficient (r) must be provided.

``` r
# 12. Invalid correlation coefficient (>1)
backcalc_corrs(r = 1.2, n = 20)
```


    Insufficient Input:
    Correlation coefficients must be between -1 and 1.

``` r
# 13. Two correlations but missing sample sizes
backcalc_corrs(r = c(0.3, 0.5))
```


    Insufficient Input:
    Two correlations provided but sample sizes (n) missing or incomplete.

``` r
# 14. Only r provided, no se, n, p, df, ci or statistic
backcalc_corrs(r = 0.4)
```


    Insufficient Input:
    Provide se, n, ci, or p with df, or statistic with df to infer missing statistics.

``` r
# 15. Invalid confidence interval length (not length 2)
backcalc_corrs(r = 0.3, ci = c(0.1, 0.2, 0.3), n = 20)
```


    Insufficient Input:
    CI must be a numeric vector of length 2 with values between -1 and 1.
