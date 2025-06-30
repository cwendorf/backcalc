## Correlations Examples

- [One Sample Cases](#one-sample-cases)
- [Two Sample Cases](#two-sample-cases)
- [Cases That Do Not Work](#cases-that-do-not-work)

------------------------------------------------------------------------

### One Sample Cases

``` r
# 1. r + SE + n (df inferred)
backcalc_corrs(r = 0.45, se = 0.1, n = 25, digits = 3)
```

    Note(s):
    df approximated as n - 2.

    Estimate       SE        t       df        p       LL       UL 
       0.450    0.100    4.847   23.000    0.000    0.271    0.599 

``` r
# 2. r + p-value + df
backcalc_corrs(r = 0.52, p = 0.02, df = 18, digits = 3)
```

    Note(s):
    SE approximated from p-value and df.

    Estimate       SE        t       df        p       LL       UL 
       0.520    0.226    2.552   18.000    0.020    0.102    0.782 

``` r
# 3. r + confidence interval + df
backcalc_corrs(r = 0.35, ci = c(0.10, 0.55), df = 20, digits = 3)
```

    Note(s):
    SE approximated from CI in Fisher z scale.

    Estimate       SE        t       df        p       LL       UL 
       0.350    0.124    2.943   20.000    0.008    0.106    0.554 

``` r
# 4. r + SE only (z-test)
backcalc_corrs(r = 0.3, se = 0.08, digits = 3)
```

    Estimate       SE        z       df        p       LL       UL 
       0.300    0.080    3.869       NA    0.000    0.152    0.435 

``` r
# 5. r + p-value + n (df inferred)
backcalc_corrs(r = 0.42, p = 0.04, n = 22, digits = 3)
```

    Note(s):
    SE approximated using 1 / sqrt(n - 3).
    df approximated as n - 2.

    Estimate       SE        t       df        p       LL       UL 
       0.420    0.229    1.951   20.000    0.040   -0.031    0.729 

### Two Sample Cases

``` r
# 6. Two rs, equal n
backcalc_corrs(r = c(0.60, 0.40), n = c(30, 30), digits = 3)
```

    Note(s):
    Estimate calculated as Fisher z-difference of two correlations.
    SE derived from z difference formula. df conservatively approximated as min(n) - 3.

    Estimate       SE        t       df        p       LL       UL 
       0.263    0.272    0.990   27.000    0.331   -0.281    0.679 

``` r
# 7. Two rs, unequal n
backcalc_corrs(r = c(0.70, 0.50), n = c(40, 25), digits = 3)
```

    Note(s):
    Estimate calculated as Fisher z-difference of two correlations.
    SE derived from z difference formula. df conservatively approximated as min(n) - 3.

    Estimate       SE        t       df        p       LL       UL 
       0.308    0.269    1.181   22.000    0.250   -0.236    0.705 

``` r
# 8. r difference + SE + df
backcalc_corrs(r = 0.15, se = 0.07, df = 50, digits = 3)
```

    Estimate       SE        t       df        p       LL       UL 
       0.150    0.070    2.159   50.000    0.036    0.011    0.284 

``` r
# 9. r difference + p-value + df
backcalc_corrs(r = 0.18, p = 0.03, df = 45, digits = 3)
```

    Note(s):
    SE approximated from p-value and df.

    Estimate       SE        t       df        p       LL       UL 
       0.180    0.081    2.241   45.000    0.030    0.018    0.332 

``` r
# 10. r difference + CI + df
backcalc_corrs(r = 0.20, ci = c(0.05, 0.35), df = 40, digits = 3)
```

    Note(s):
    SE approximated from CI in Fisher z scale.

    Estimate       SE        t       df        p       LL       UL 
       0.200    0.078    2.598   40.000    0.013    0.045    0.346 

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
