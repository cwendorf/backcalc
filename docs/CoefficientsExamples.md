## Coefficient Examples

- [Unstandardized Beta Cases](#unstandardized-beta-cases)
- [Standardized Beta and Conversion
  Cases](#standardized-beta-and-conversion-cases)
- [Cases That Do Not Work](#cases-that-do-not-work)

------------------------------------------------------------------------

### Unstandardized Beta Cases

``` r
# 1. Basic z-test with coefficient and SE; default 95% CI and 3 digits
backcalc_coeffs(b = 0.5, se = 0.1)
```

    Estimate       SE        z       df        p       LL       UL 
       0.500    0.100    5.000       NA    0.000    0.304    0.696 

``` r
# 2. Large sample z-test with coefficient and SE; high precision output
backcalc_coeffs(b = 0.15, se = 0.02, digits = 5)
```

    Estimate       SE        z       df        p       LL       UL 
      0.1500   0.0200   7.5000       NA   0.0000   0.1108   0.1892 

``` r
# 3. T-test from coefficient and p-value with df; default 95% CI
backcalc_coeffs(b = 1.2, p = 0.03, df = 28)
```

    Note(s):
    Test statistic approximated from p-value and estimate.
    SE approximated from estimate and reconstructed statistic.

    Estimate       SE        t       df        p       LL       UL 
       1.200    0.525    2.286   28.000    0.030    0.125    2.275 

``` r
# 4. Estimate and confidence interval given; infer SE and t-statistic with df
backcalc_coeffs(b = 0.8, ci = c(0.2, 1.4), df = 45, digits = 4)
```

    Note(s):
    SE approximated from CI width.

    Estimate       SE        t       df        p       LL       UL 
      0.8000   0.2979   2.6855  45.0000   0.0101   0.2000   1.4000 

``` r
# 5. Use all inputs: b, se, SDs, p-value, df, one-sided test, and custom digits
backcalc_coeffs(b = 1.1, se = 0.3, sd_x = 2.5, sd_y = 5, p = 0.04, df = 30, one_sided = TRUE, digits = 4)
```

    Note(s):
    Standardized beta approximated from unstandardized beta and standard deviations.
    SE of standardized beta approximated from unstandardized SE and SDs.

    Estimate       SE        t       df    p_one       LL       UL 
      0.5500   0.1500   3.6667  30.0000   0.0400   0.2954   0.8046 

``` r
# 6. One-sided t-test with coefficient, p-value, and df; 90% CI specified
backcalc_coeffs(b = -0.7, p = 0.01, df = 20, one_sided = TRUE, conf.level = 0.90)
```

    Note(s):
    Test statistic approximated from p-value and estimate.
    SE approximated from estimate and reconstructed statistic.

    Estimate       SE        t       df    p_one       LL       UL 
      -0.700    0.277   -2.528   20.000    0.010   -1.067   -0.333 

``` r
# 7. Minimal input: coefficient and p-value with df; change CI level and rounding
backcalc_coeffs(b = 1.5, p = 0.02, df = 25, conf.level = 0.90, digits = 4)
```

    Note(s):
    Test statistic approximated from p-value and estimate.
    SE approximated from estimate and reconstructed statistic.

    Estimate       SE        t       df        p       LL       UL 
      1.5000   0.6036   2.4851  25.0000   0.0200   0.4690   2.5310 

### Standardized Beta and Conversion Cases

``` r
# 8. Standardized beta and SE only; uses z-test by default
backcalc_coeffs(std_beta = 0.25, se_std = 0.04, conf.level = 0.99)
```

    Estimate       SE        z       df        p       LL       UL 
       0.250    0.040    6.250       NA    0.000    0.147    0.353 

``` r
# 9. Convert unstandardized b and SE to standardized beta using SDs; default CI and digits
backcalc_coeffs(b = 2.0, se = 0.5, sd_x = 3, sd_y = 6)
```

    Note(s):
    Standardized beta approximated from unstandardized beta and standard deviations.
    SE of standardized beta approximated from unstandardized SE and SDs.

    Estimate       SE        z       df        p       LL       UL 
        1.00     0.25     4.00       NA     0.00     0.51     1.49 

``` r
# 10. Standardized beta with confidence interval only; infer SE and stats, use 99% CI
backcalc_coeffs(std_beta = 0.4, ci = c(0.1, 0.7), conf.level = 0.99)
```

    Note(s):
    SE approximated from CI width.

    Estimate       SE        z       df        p       LL       UL 
       0.400    0.116    3.434       NA    0.001    0.100    0.700 

### Cases That Do Not Work

``` r
# 11. No coefficient or standardized beta provided
backcalc_coeffs(se = 0.1)
```

    Insufficient information: cannot estimate coefficient or SE, even approximately. 

``` r
# 12. Only p-value provided, no coefficient or SE
backcalc_coeffs(p = 0.05)
```

    Insufficient information: cannot estimate coefficient or SE, even approximately. 

``` r
# 13. Only df and se provided — missing estimate (b or std_beta), no CI or p-value
backcalc_coeffs(se = 0.1, df = 30)
```

    Insufficient information: cannot estimate coefficient or SE, even approximately. 

``` r
# 14. Standardized beta provided without SE or SDs to infer SE
backcalc_coeffs(std_beta = 0.4)
```

    Insufficient information: cannot estimate coefficient or SE, even approximately. 
