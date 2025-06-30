## Ratios Examples

- [One Sample Cases](#one-sample-cases)
- [Two Sample Cases](#two-sample-cases)
- [Cases That Do Not Work](#cases-that-do-not-work)

------------------------------------------------------------------------

### One Sample Cases

``` r
# 1. Ratio + SE only (z-test)
backcalc_ratios(ratio = 1.5, se = 0.2, digits = 3)
```

    Estimate       SE        z       df        p       LL       UL 
       1.500    0.200    2.027       NA    0.043    1.014    2.220 

``` r
# 2. Ratio + CI only (z-test)
backcalc_ratios(ratio = 2.0, ci = c(1.4, 2.9), digits = 3)
```

    Note(s):
    SE approximated from CI using log scale and critical value.

    Estimate       SE        z       df        p       LL       UL 
       2.000    0.186    3.731       NA    0.000    1.390    2.878 

``` r
# 3. Ratio + CI + n (df inferred from n)
backcalc_ratios(ratio = 1.8, ci = c(1.3, 2.4), n = 25, digits = 3)
```

    Note(s):
    df approximated as n - 1.
    SE approximated from CI using log scale and critical value.

    Estimate       SE        t       df        p       LL       UL 
       1.800    0.149    3.957   24.000    0.001    1.325    2.446 

``` r
# 4. Ratio + confidence interval + df
backcalc_ratios(ratio = 1.7, ci = c(1.1, 2.5), df = 18, digits = 3)
```

    Note(s):
    SE approximated from CI using log scale and critical value.

    Estimate       SE        t       df        p       LL       UL 
       1.700    0.195    2.716   18.000    0.014    1.128    2.563 

``` r
# 5. Ratio + SE + df
backcalc_ratios(ratio = 1.4, se = 0.12, df = 20, digits = 3)
```

    Estimate       SE        t       df        p       LL       UL 
       1.400    0.120    2.804   20.000    0.011    1.090    1.798 

### Two Sample Cases

``` r
# 6. Two-sample equal df
backcalc_ratios(ratio = c(2.2, 1.5), se = c(0.15, 0.10), df = 20, digits = 3)
```

    Note(s):
    Estimate calculated as log ratio difference between two ratios.
    SE combined using sqrt(se1^2 + se2^2).

    Estimate       SE        t       df        p       LL       UL 
       1.467    0.180    2.124   20.000    0.046    1.007    2.136 

``` r
# 7. Two-sample unequal df (Welch correction)
backcalc_ratios(ratio = c(2.5, 1.7), se = c(0.18, 0.12), df = c(18, 22), digits = 3)
```

    Note(s):
    Estimate calculated as log ratio difference between two ratios.
    SE combined using sqrt(se1^2 + se2^2).
    df approximated using Welch–Satterthwaite formula.

    Estimate       SE        t       df        p       LL       UL 
       1.471    0.216    1.783   18.000    0.092    0.933    2.317 

``` r
# 8. Ratio difference + SE + df provided
backcalc_ratios(ratio = 0.25, se = 0.09, df = 40, digits = 3)
```

    Estimate       SE        t       df        p       LL       UL 
       0.250    0.090  -15.403   40.000    0.000    0.208    0.300 

``` r
# 9. Ratio difference + confidence interval + df
backcalc_ratios(ratio = 0.35, ci = c(0.10, 0.60), df = 38, digits = 3)
```

    Note(s):
    SE approximated from CI using log scale and critical value.

    Estimate       SE        t       df        p       LL       UL 
       0.350    0.443   -2.372   38.000    0.023    0.143    0.857 

``` r
# 10. Two-sample with different ratios and SEs, equal df
backcalc_ratios(ratio = c(1.8, 1.2), se = c(0.14, 0.09), df = 24, digits = 3)
```

    Note(s):
    Estimate calculated as log ratio difference between two ratios.
    SE combined using sqrt(se1^2 + se2^2).

    Estimate       SE        t       df        p       LL       UL 
       1.500    0.166    2.436   24.000    0.023    1.064    2.115 

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
