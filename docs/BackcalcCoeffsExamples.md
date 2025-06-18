backcalc Coeffs Examples
================

## backcalc Coeffs Examples

- [Unstandardized Beta Cases](#unstandardized-beta-cases)
- [Standardized Beta and Conversion Cases](#standardized-beta-and-conversion-cases)

------------------------------------------------------------------------

### Unstandardized Beta Cases

``` r
# 1. Basic z-test with coefficient and SE; default 95% CI and 3 digits
backcalc_coeffs(b = 0.5, se = 0.1)
```

    coeff    se    df ci_ll ci_ul     z     p 
    0.500 0.100    NA 0.304 0.696 5.000 0.000 

``` r
# 2. Large sample z-test with coefficient and SE; high precision output
backcalc_coeffs(b = 0.15, se = 0.02, sig_digits = 5)
```

     coeff     se     df  ci_ll  ci_ul      z      p 
    0.1500 0.0200     NA 0.1108 0.1892 7.5000 0.0000 

``` r
# 3. T-test from coefficient and p-value with df; default 95% CI
backcalc_coeffs(b = 1.2, p = 0.03, df = 28)
```

     coeff     se     df  ci_ll  ci_ul      t      p 
     1.200  0.525 28.000  0.125  2.275  2.286  0.030 

``` r
# 4. Estimate and confidence interval given; infer SE and t-statistic with df
backcalc_coeffs(b = 0.8, ci = c(0.2, 1.4), df = 45, sig_digits = 4)
```

      coeff      se      df   ci_ll   ci_ul       t       p 
     0.8000  0.2979 45.0000  0.2000  1.4000  2.6855  0.0101 

``` r
# 5. Use all inputs: b, se, SDs, p-value, df, one-sided test, and custom digits
backcalc_coeffs(b = 1.1, se = 0.3, sd_x = 2.5, sd_y = 5, p = 0.04, df = 30, one_sided = TRUE, sig_digits = 4)
```

      coeff      se      df   ci_ll   ci_ul       t   p_one 
     0.5500  0.1500 30.0000  0.2954  0.8046  3.6667  0.0400 

``` r
# 6. One-sided t-test with coefficient, p-value, and df; 90% CI specified
backcalc_coeffs(b = -0.7, p = 0.01, df = 20, one_sided = TRUE, conf.level = 0.90)
```

     coeff     se     df  ci_ll  ci_ul      t  p_one 
    -0.700  0.277 20.000 -1.067 -0.333 -2.528  0.010 

``` r
# 7. Minimal input: coefficient and p-value with df; change CI level and rounding
backcalc_coeffs(b = 1.5, p = 0.02, df = 25, conf.level = 0.90, sig_digits = 4)
```

      coeff      se      df   ci_ll   ci_ul       t       p 
     1.5000  0.6036 25.0000  0.4690  2.5310  2.4851  0.0200 

### Standardized Beta and Conversion Cases

``` r
# 8. Standardized beta and SE only; uses z-test by default
backcalc_coeffs(std_beta = 0.25, se_std = 0.04, conf.level = 0.99)
```

    coeff    se    df ci_ll ci_ul     z     p 
    0.250 0.040    NA 0.147 0.353 6.250 0.000 

``` r
# 9. Convert unstandardized b and SE to standardized beta using SDs; default CI and digits
backcalc_coeffs(b = 2.0, se = 0.5, sd_x = 3, sd_y = 6)
```

    coeff    se    df ci_ll ci_ul     z     p 
     1.00  0.25    NA  0.51  1.49  4.00  0.00 

``` r
# 10. Standardized beta with confidence interval only; infer SE and stats, use 99% CI
backcalc_coeffs(std_beta = 0.4, ci = c(0.1, 0.7), conf.level = 0.99)
```

    coeff    se    df ci_ll ci_ul     z     p 
    0.400 0.116    NA 0.100 0.700 3.434 0.001 
