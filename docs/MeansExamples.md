## Means Examples

- [One Sample Cases](#one-sample-cases)
- [Paired and Two Sample Cases](#paired-and-two-sample-cases)
- [Cases That Do Not Work](#cases-that-do-not-work)

------------------------------------------------------------------------

### One Sample Cases

``` r
# 1. Confidence interval only (m & SE inferred)
backcalc_means(ci = c(2.1, 4.9), digits = 2)
```

    Note(s):
    SE approximated from CI width.

    Estimate       SE        z       df        p       LL       UL 
        3.50     0.71     4.90       NA     0.00     2.10     4.90 

``` r
# 2. m + SE (z assumed)
backcalc_means(m = 5.2, se = 1.1, digits = 3)
```

    Estimate       SE        z       df        p       LL       UL 
       5.200    1.100    4.727       NA    0.000    3.044    7.356 

``` r
# 3. m + SE + df (standard t-test)
backcalc_means(m = 3.5, se = 0.8, df = 28, digits = 3)
```

    Estimate       SE        t       df        p       LL       UL 
       3.500    0.800    4.375   28.000    0.000    1.861    5.139 

``` r
# 4. m + p-value + df (SE inferred from p)
backcalc_means(m = 2.8, p = 0.023, df = 35, digits = 4)
```

    Note(s):
    Test statistic and SE approximated from p-value and estimate.

    Estimate       SE        t       df        p       LL       UL 
      2.8000   1.1775   2.3780  35.0000   0.0230   0.4096   5.1904 

``` r
# 5. CI + df (m and SE inferred)
backcalc_means(ci = c(4.5, 7.3), df = 15, digits = 3)
```

    Note(s):
    SE approximated from CI width.

    Estimate       SE        t       df        p       LL       UL 
       5.900    0.657    8.983   15.000    0.000    4.500    7.300 

### Paired and Two Sample Cases

``` r
# 6. Paired sample with m, se, and n only (df inferred)
backcalc_means(m = 1.5, se = 0.5, n = 15, paired = TRUE, digits = 3)
```

    Note(s):
    Degrees of freedom approximated as n - 1 for paired design.

    Estimate       SE        t       df        p       LL       UL 
       1.500    0.500    3.000   14.000    0.010    0.428    2.572 

``` r
# 7. Paired sample with m, p, and n (df inferred, one-sided test)
backcalc_means(m = 1.2, p = 0.03, n = 12, paired = TRUE, one_sided = TRUE, digits = 3)
```

    Note(s):
    Degrees of freedom approximated as n - 1 for paired design.
    Test statistic and SE approximated from p-value and estimate.

    Estimate       SE        t       df    p-one       LL       UL 
       1.200    0.572    2.096   11.000    0.030    0.172    2.228 

``` r
# 8. m + two-group SDs + equal ns (classic t test)
backcalc_means(m = 1.2, sd = c(10, 11), n = c(30, 30), digits = 3)
```

    Note(s):
    Welch-Satterthwaite approximation used for df.

    Estimate       SE        t       df        p       LL       UL 
       1.200    2.714    0.442   57.000    0.660   -4.234    6.634 

``` r
# 9. m + two-group SDs + unequal ns (Welch correction auto)
backcalc_means(m = 2.4, sd = c(5, 6), n = c(30, 40), digits = 2)
```

    Note(s):
    Welch-Satterthwaite approximation used for df.

    Estimate       SE        t       df        p       LL       UL 
        2.40     1.32     1.82    67.00     0.07    -0.23     5.03 

``` r
# 10. m + two-group SDs + df + unequal ns (redundant info, Welch correction)
backcalc_means(m = 4.1, sd = c(5.5, 6.0), n = c(20, 25), df = 43, digits = 4)
```

    Estimate       SE        t       df        p       LL       UL 
      4.1000   1.7183   2.3861  43.0000   0.0215   0.6348   7.5652 

### Cases That Do Not Work

``` r
# 11. Only p-value (insufficient to infer m or SE)
backcalc_means(p = 0.05)
```

    Insufficient input: Provide estimate and at least one of SE, p-value, or CI. 

``` r
# 12. m + n only (no SD or SE provided)
backcalc_means(m = 1.7, n = 25)
```

    Insufficient input: Provide estimate and at least one of SE, p-value, or CI. 

``` r
# 13. m + SD only (sample size missing)
backcalc_means(m = 2.1, sd = 4.5)
```

    Insufficient input: Provide estimate and at least one of SE, p-value, or CI. 

``` r
# 14. Two-group SDs + two sample sizes (no m provided)
backcalc_means(sd = c(7, 8), n = c(25, 30))
```

    Insufficient input: Provide estimate and at least one of SE, p-value, or CI. 
