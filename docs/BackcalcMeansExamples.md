backcalc Means Examples
================

## backcalc Means Examples

- [One Sample Cases](#one-sample-cases)
- [Paired and Two Sample Cases](#paired-and-two-sample-cases)

------------------------------------------------------------------------

### One Sample Cases

``` r
# 1. Confidence interval only (m & SE inferred)
backcalc_means(ci = c(2.1, 4.9), sig_digits = 2)
```

        m    se    df ci_ll ci_ul     z     p 
     3.50  0.71    NA  2.10  4.90  4.90  0.00 

``` r
# 2. m + SE (z assumed)
backcalc_means(m = 5.2, se = 1.1, sig_digits = 3)
```

        m    se    df ci_ll ci_ul     z     p 
    5.200 1.100    NA 3.044 7.356 4.727 0.000 

``` r
# 3. m + SE + df (standard t-test)
backcalc_means(m = 3.5, se = 0.8, df = 28, sig_digits = 3)
```

         m     se     df  ci_ll  ci_ul      t      p 
     3.500  0.800 28.000  1.861  5.139  4.375  0.000 

``` r
# 4. m + p-value + df (SE inferred from p)
backcalc_means(m = 2.8, p = 0.023, df = 35, sig_digits = 4)
```

          m      se      df   ci_ll   ci_ul       t       p 
     2.8000  1.1775 35.0000  0.4096  5.1904  2.3780  0.0230 

``` r
# 5. CI + df (m and SE inferred)
backcalc_means(ci = c(4.5, 7.3), df = 15, sig_digits = 3)
```

         m     se     df  ci_ll  ci_ul      t      p 
     5.900  0.657 15.000  4.500  7.300  8.983  0.000 

### Paired and Two Sample Cases

``` r
# 6. Paired sample with m, se, and n only (df inferred)
backcalc_means(m = 1.5, se = 0.5, n = 15, paired = TRUE, sig_digits = 3)
```

         m     se     df  ci_ll  ci_ul      t      p 
     1.500  0.500 14.000  0.428  2.572  3.000  0.010 

``` r
# 7. Paired sample with m, p, and n (df inferred, one-sided test)
backcalc_means(m = 1.2, p = 0.03, n = 12, paired = TRUE, one_sided = TRUE, sig_digits = 3)
```

         m     se     df  ci_ll  ci_ul      t  p-one 
     1.200  0.572 11.000  0.172  2.228  2.096  0.030 

``` r
# 8. m + two-group SDs + equal ns (classic t test)
backcalc_means(m = 1.2, sd = c(10, 11), n = c(30, 30), sig_digits = 3)
```

         m     se     df  ci_ll  ci_ul      t      p 
     1.200  2.714 57.000 -4.234  6.634  0.442  0.660 

``` r
# 9. m + two-group SDs + unequal ns (Welch correction auto)
backcalc_means(m = 2.4, sd = c(5, 6), n = c(30, 40), sig_digits = 2)
```

        m    se    df ci_ll ci_ul     t     p 
     2.40  1.32 67.00 -0.23  5.03  1.82  0.07 

``` r
# 10. m + two-group SDs + df + unequal ns (redundant info, Welch correction)
backcalc_means(m = 4.1, sd = c(5.5, 6.0), n = c(20, 25), df = 43, sig_digits = 4)
```

          m      se      df   ci_ll   ci_ul       t       p 
     4.1000  1.7183 43.0000  0.6348  7.5652  2.3861  0.0215 
