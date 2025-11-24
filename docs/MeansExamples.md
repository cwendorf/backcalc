# [`backcalc`](https://github.com/cwendorf/backcalc/)

## Means Examples

- [One Sample Cases](#one-sample-cases)
- [Two Sample Cases](#two-sample-cases)
- [Insufficient Information Cases](#insufficient-information-cases)

------------------------------------------------------------------------

Each section of examples below progresses from more complete input to
less complete input, moving from miminal required inference to maximal
inference on the part of the function. Similarly, the structure
highlights the flexibility of the function across diverse study designs
and input constraints.

### One Sample Cases

``` r
# 1. Direct estimate with SE and sample size (minimal inference, t-distribution used)
backcalc_means(m = 25.4, se = 2.1, n = 30)
```


            Estimate    SE      t     df     p     LL     UL
    Outcome   25.400 2.100 12.095 29.000 0.000 21.105 29.695

    Note(s):
    Degrees of freedom approximated as n - 1.

``` r
# 2. Estimate with SD and sample size (SE is inferred, t-distribution used)
backcalc_means(m = 25.4, sd = 10, n = 30)
```


            Estimate    SE      t     df     p     LL     UL
    Outcome   25.400 1.826 13.912 29.000 0.000 21.666 29.134

    Note(s):
    SE approximated from sd and n.
    Degrees of freedom approximated as n - 1.

``` r
# 3. Estimate with confidence interval and sample size (SE is inferred from CI)
backcalc_means(m = 30, ci = c(25, 35), n = 25)
```


            Estimate    SE      t     df     p     LL     UL
    Outcome   30.000 2.423 12.383 24.000 0.000 25.000 35.000

    Note(s):
    Degrees of freedom approximated as n - 1.
    SE approximated from CI width.

``` r
# 4. Estimate with p-value and degrees of freedom (SE and test statistic inferred)
backcalc_means(m = 2.5, p = 0.03, df = 29)
```


            Estimate    SE     t     df     p    LL    UL
    Outcome    2.500 1.095 2.282 29.000 0.030 0.260 4.740

    Note(s):
    Test statistic and SE approximated from p-value and estimate.

``` r
# 5. Estimate with no SE, but p-value and sample size given (df is inferred, t-statistic and SE inferred)
backcalc_means(m = 2.1, p = 0.05, n = 16)
```


            Estimate    SE     t     df     p    LL    UL
    Outcome    2.100 0.985 2.131 15.000 0.050 0.000 4.200

    Note(s):
    Degrees of freedom approximated as n - 1.
    Test statistic and SE approximated from p-value and estimate.

### Two Sample Cases

``` r
# 6. Means, SDs, and ns provided (calculate difference, SE, df)
backcalc_means(m = c(15, 12), sd = c(4, 5), n = c(40, 35))
```


            Estimate    SE     t     df     p    LL    UL
    Outcome    3.000 1.056 2.842 65.000 0.006 0.892 5.108

    Note(s):
    Welch-Satterthwaite approximation used for df.

``` r
# 7. Difference of means and SE provided
backcalc_means(m = c(15, 12), se = 1.5, n = c(40, 35))
```


            Estimate    SE     t     df     p    LL    UL
    Outcome    3.000 1.500 2.000 73.000 0.049 0.011 5.989

    Note(s):
    Degrees of freedom approximated as n1 + n2 - 2.

``` r
# 8. Means and p-value + df provided (infer SE and statistic)
backcalc_means(m = c(10, 7), p = 0.04, df = 50)
```


            Estimate    SE     t     df     p    LL    UL
    Outcome    3.000 1.423 2.109 50.000 0.040 0.142 5.858

    Note(s):
    Test statistic and SE approximated from p-value and estimate.

``` r
# 9. Means and confidence interval provided (infer SE, df)
backcalc_means(m = c(100, 90), ci = c(2, 18), n = c(50, 45))
```


            Estimate    SE     t     df     p    LL     UL
    Outcome   10.000 4.029 2.482 93.000 0.015 2.000 18.000

    Note(s):
    Degrees of freedom approximated as n1 + n2 - 2.
    SE approximated from CI width.

``` r
# 10. Means and SDs provided, but only n for one group (more complex inference)
backcalc_means(m = c(8, 5), sd = c(3, 4), n = 20)
```


            Estimate    SE     t     df     p    LL    UL
    Outcome    3.000 1.118 2.683 35.000 0.011 0.731 5.269

    Note(s):
    Assumed equal sample sizes for both groups.
    Welch-Satterthwaite approximation used for df.

### Insufficient Information Cases

``` r
# 11. No mean, SE, p, or CI provided
backcalc_means(n = 15)
```


    Insufficient Input:
    Provide estimate and at least one of SE, p-value, CI, or test statistic.

``` r
# 12. Mean provided with SD but no n or SE
backcalc_means(m = 7, sd = 2)
```


    Insufficient Input:
    Cannot compute SE from SD without sample size (n).
    Provide estimate and at least one of SE, p-value, CI, or test statistic.

``` r
# 13. Only p-value and df provided (no estimate or SE)
backcalc_means(p = 0.05, df = 20)
```


    Insufficient Input:
    Provide estimate and at least one of SE, p-value, CI, or test statistic.

``` r
# 14. Mean and df provided, but no SE, p, or CI
backcalc_means(m = 5.6, df = 10)
```


    Insufficient Input:
    Provide estimate and at least one of SE, p-value, CI, or test statistic.

``` r
# 15. Only statistic and df provided (no estimate or SE)
backcalc_means(statistic = 2.5, df = 18)
```


    Insufficient Input:
    Provide estimate and at least one of SE, p-value, CI, or test statistic.
