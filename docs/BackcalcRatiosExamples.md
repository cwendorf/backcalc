backcalc Ratios Examples
================

## backcalc Ratios Examples

- [One Sample Cases](#one-sample-cases)
- [Two Sample Cases](#two-sample-cases)
- [Cases That Do Not Work](#cases-that-do-not-work)

------------------------------------------------------------------------

### One Sample Cases

``` r
# 1. Ratio + SE only (z-test)
backcalc_ratios(ratio = 1.5, se = 0.2, sig_digits = 3)
```

    ratio    se    df ci_ll ci_ul     z     p 
    1.500 0.200    NA 1.014 2.220 2.027 0.043 

``` r
# 2. Ratio + CI only (z-test)
backcalc_ratios(ratio = 2.0, ci = c(1.4, 2.9), sig_digits = 3)
```

    Note(s):
    SE approximated from CI using log scale and critical value.

    ratio    se    df ci_ll ci_ul     z     p 
    2.000 0.186    NA 1.390 2.878 3.731 0.000 

``` r
# 3. Ratio + CI + n (df inferred from n)
backcalc_ratios(ratio = 1.8, ci = c(1.3, 2.4), n = 25, sig_digits = 3)
```

    Note(s):
    df approximated as n - 1.
    SE approximated from CI using log scale and critical value.

     ratio     se     df  ci_ll  ci_ul      t      p 
     1.800  0.149 24.000  1.325  2.446  3.957  0.001 

``` r
# 4. Ratio + confidence interval + df
backcalc_ratios(ratio = 1.7, ci = c(1.1, 2.5), df = 18, sig_digits = 3)
```

    Note(s):
    SE approximated from CI using log scale and critical value.

     ratio     se     df  ci_ll  ci_ul      t      p 
     1.700  0.195 18.000  1.128  2.563  2.716  0.014 

``` r
# 5. Ratio + SE + df
backcalc_ratios(ratio = 1.4, se = 0.12, df = 20, sig_digits = 3)
```

     ratio     se     df  ci_ll  ci_ul      t      p 
     1.400  0.120 20.000  1.090  1.798  2.804  0.011 

### Two Sample Cases

``` r
# 6. Two-sample equal df
backcalc_ratios(ratio = c(2.2, 1.5), se = c(0.15, 0.10), df = 20, sig_digits = 3)
```

    Note(s):
    Estimate calculated as log ratio difference between two ratios.
    SE combined using sqrt(se1^2 + se2^2).

     ratio     se     df  ci_ll  ci_ul      t      p 
     1.467  0.180 20.000  1.007  2.136  2.124  0.046 

``` r
# 7. Two-sample unequal df (Welch correction)
backcalc_ratios(ratio = c(2.5, 1.7), se = c(0.18, 0.12), df = c(18, 22), sig_digits = 3)
```

    Note(s):
    Estimate calculated as log ratio difference between two ratios.
    SE combined using sqrt(se1^2 + se2^2).
    df approximated using Welch–Satterthwaite formula.

     ratio     se     df  ci_ll  ci_ul      t      p 
     1.471  0.216 18.000  0.933  2.317  1.783  0.092 

``` r
# 8. Ratio difference + SE + df provided
backcalc_ratios(ratio = 0.25, se = 0.09, df = 40, sig_digits = 3)
```

      ratio      se      df   ci_ll   ci_ul       t       p 
      0.250   0.090  40.000   0.208   0.300 -15.403   0.000 

``` r
# 9. Ratio difference + confidence interval + df
backcalc_ratios(ratio = 0.35, ci = c(0.10, 0.60), df = 38, sig_digits = 3)
```

    Note(s):
    SE approximated from CI using log scale and critical value.

     ratio     se     df  ci_ll  ci_ul      t      p 
     0.350  0.443 38.000  0.143  0.857 -2.372  0.023 

``` r
# 10. Two-sample with different ratios and SEs, equal df
backcalc_ratios(ratio = c(1.8, 1.2), se = c(0.14, 0.09), df = 24, sig_digits = 3)
```

    Note(s):
    Estimate calculated as log ratio difference between two ratios.
    SE combined using sqrt(se1^2 + se2^2).

     ratio     se     df  ci_ll  ci_ul      t      p 
     1.500  0.166 24.000  1.064  2.115  2.436  0.023 

### Cases That Do Not Work

``` r
# 11. Missing ratio entirely
backcalc_ratios(se = 0.2)
```

    Insufficient information: a ratio (or two ratios) must be provided.

``` r
# 12. Ratio provided but no SE or CI to infer variability
backcalc_ratios(ratio = 1.5)
```

    Insufficient information: SE or CI must be provided.

``` r
# 13. Two ratios provided but no SE or CI for inference
backcalc_ratios(ratio = c(2.0, 1.5))
```

    Insufficient information: SE or CI must be provided.

``` r
# 14. Ratio + p-value provided, but no SE, CI, or df to infer SE or statistic
backcalc_ratios(ratio = 1.8, p = 0.05)
```

    Insufficient information: SE or CI must be provided.
