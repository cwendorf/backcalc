backcalc Corrs Examples
================

## backcalc Corrs Examples

- [One Sample Correlation Cases](#one-sample-correlation-cases)
- [Two Sample Correlation Cases](#two-sample-correlation-cases)
- [Cases That Do Not Work](#cases-that-do-not-work)

------------------------------------------------------------------------

### One Sample Correlation Cases

``` r
# 1. r + SE + n (df inferred)
backcalc_corrs(r = 0.45, se = 0.1, n = 25, sig_digits = 3)
```

    Note(s):
     df approximated as n - 2. 

         r     se     df  ci_ll  ci_ul      t      p 
     0.450  0.100 23.000  0.271  0.599  4.847  0.000 

``` r
# 2. r + p-value + df
backcalc_corrs(r = 0.52, p = 0.02, df = 18, sig_digits = 3)
```

    Note(s):
     SE approximated from p-value and df. 

         r     se     df  ci_ll  ci_ul      t      p 
     0.520  0.226 18.000  0.102  0.782  2.552  0.020 

``` r
# 3. r + confidence interval + df
backcalc_corrs(r = 0.35, ci = c(0.10, 0.55), df = 20, sig_digits = 3)
```

    Note(s):
     SE approximated from CI in Fisher z scale. 

         r     se     df  ci_ll  ci_ul      t      p 
     0.350  0.124 20.000  0.106  0.554  2.943  0.008 

``` r
# 4. r + SE only (z-test)
backcalc_corrs(r = 0.3, se = 0.08, sig_digits = 3)
```

        r    se    df ci_ll ci_ul     z     p 
    0.300 0.080    NA 0.152 0.435 3.869 0.000 

``` r
# 5. r + p-value + n (df inferred)
backcalc_corrs(r = 0.42, p = 0.04, n = 22, sig_digits = 3)
```

    Note(s):
     SE approximated using 1 / sqrt(n - 3).
    df approximated as n - 2. 

         r     se     df  ci_ll  ci_ul      t      p 
     0.420  0.229 20.000 -0.031  0.729  1.951  0.040 

### Two Sample Correlation Cases

``` r
# 6. Two rs, equal n
backcalc_corrs(r = c(0.60, 0.40), n = c(30, 30), sig_digits = 3)
```

    Note(s):
     Estimate calculated as Fisher z-difference of two correlations.
    SE derived from z difference formula. df conservatively approximated as min(n) - 3. 

         r     se     df  ci_ll  ci_ul      t      p 
     0.263  0.272 27.000 -0.281  0.679  0.990  0.331 

``` r
# 7. Two rs, unequal n
backcalc_corrs(r = c(0.70, 0.50), n = c(40, 25), sig_digits = 3)
```

    Note(s):
     Estimate calculated as Fisher z-difference of two correlations.
    SE derived from z difference formula. df conservatively approximated as min(n) - 3. 

         r     se     df  ci_ll  ci_ul      t      p 
     0.308  0.269 22.000 -0.236  0.705  1.181  0.250 

``` r
# 8. r difference + SE + df
backcalc_corrs(r = 0.15, se = 0.07, df = 50, sig_digits = 3)
```

         r     se     df  ci_ll  ci_ul      t      p 
     0.150  0.070 50.000  0.011  0.284  2.159  0.036 

``` r
# 9. r difference + p-value + df
backcalc_corrs(r = 0.18, p = 0.03, df = 45, sig_digits = 3)
```

    Note(s):
     SE approximated from p-value and df. 

         r     se     df  ci_ll  ci_ul      t      p 
     0.180  0.081 45.000  0.018  0.332  2.241  0.030 

``` r
# 10. r difference + CI + df
backcalc_corrs(r = 0.20, ci = c(0.05, 0.35), df = 40, sig_digits = 3)
```

    Note(s):
     SE approximated from CI in Fisher z scale. 

         r     se     df  ci_ll  ci_ul      t      p 
     0.200  0.078 40.000  0.045  0.346  2.598  0.013 

### Cases That Do Not Work

``` r
# 11. No correlation coefficient (r) provided
backcalc_corrs(se = 0.1, n = 30)
```

    Insufficient information: Correlation coefficient (r) must be provided.

``` r
# 12. Correlation coefficient outside valid range (-1 to 1)
backcalc_corrs(r = 1.2, n = 30)
```

    Invalid input: Correlation coefficients must be between -1 and 1.

``` r
# 13. Two correlations provided but sample sizes missing or incomplete
backcalc_corrs(r = c(0.5, 0.3))
```

    Insufficient information: Two correlations provided but sample sizes (n) missing or incomplete.

``` r
# 14. One correlation but no SE, no sample size, and no CI provided
backcalc_corrs(r = 0.4)
```

    Insufficient information: Provide se, n, ci, or p with df to infer missing statistics.
