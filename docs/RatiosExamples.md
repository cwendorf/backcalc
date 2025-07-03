## Ratios Examples

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
# 1. Ratio and SE provided (no inference needed)
backcalc_ratios(ratio = 2.5, se = 0.2)
```

    Estimate       SE        z       df        p       LL       UL 
       2.500    0.200    4.581       NA    0.000    1.689    3.700 

``` r
# 2. Ratio and confidence interval provided (infer SE from CI)
backcalc_ratios(ratio = 1.8, ci = c(1.3, 2.5))
```

    Note(s):
    SE approximated from CI using log scale and critical value.

    Estimate       SE        z       df        p       LL       UL 
       1.800    0.167    3.523       NA    0.000    1.298    2.496 

``` r
# 3. Ratio and test statistic provided (infer SE from statistic)
backcalc_ratios(ratio = 3.0, statistic = 4.0)
```

    Note(s):
    SE approximated from statistic and estimate.

    Estimate       SE        z       df        p       LL       UL 
       3.000    0.275    4.000       NA    0.000    1.751    5.139 

``` r
# 4. Ratio, test statistic, and df provided (infer SE and p)
backcalc_ratios(ratio = 2.2, statistic = 2.5, df = 29)
```

    Note(s):
    SE approximated from statistic and estimate.

    Estimate       SE        t       df        p       LL       UL 
       2.200    0.315    2.500   29.000    0.018    1.154    4.193 

``` r
# 5. Ratio, test statistic, and sample size (infer SE, df, p, and CI)
backcalc_ratios(ratio = 1.5, statistic = 2.1, n = 40)
```

    Note(s):
    df approximated as n - 1.
    SE approximated from statistic and estimate.

    Estimate       SE        t       df        p       LL       UL 
       1.500    0.193    2.100   39.000    0.042    1.015    2.217 

### Two Sample Cases

``` r
# 6. Two ratios and SEs provided (no inference needed)
backcalc_ratios(ratio = c(2.5, 1.8), se = c(0.2, 0.1))
```

    Note(s):
    Estimate calculated as log ratio difference between two ratios.
    SE combined using sqrt(se1^2 + se2^2).

    Estimate       SE        z       df        p       LL       UL 
       1.389    0.224    1.469       NA    0.142    0.896    2.153 

``` r
# 7. Two ratios, SEs, and dfs provided (Welch-Satterthwaite df calculation)
backcalc_ratios(ratio = c(3.2, 1.9), se = c(0.25, 0.15), df = c(25, 30))
```

    Note(s):
    Estimate calculated as log ratio difference between two ratios.
    SE combined using sqrt(se1^2 + se2^2).
    df vector provided but SE of difference only; df not adjusted.

    Estimate       SE        t       df        p       LL       UL 
       1.684    0.292    1.788   28.000    0.085    0.926    3.062 

``` r
# 8. Two ratios, SE for first group, and CI for the difference (infer second SE and test stats)
backcalc_ratios(ratio = c(2.0, 1.5), se = 0.12, ci = c(1.2, 3.0))
```

    Note(s):
    Estimate calculated as log ratio difference between two ratios.

    Estimate       SE        z       df        p       LL       UL 
       1.333    0.120    2.397       NA    0.017    1.054    1.687 

``` r
# 9. Two ratios, SE for first group, test statistic and df provided (infer missing SE and p)
backcalc_ratios(ratio = c(2.0, 1.5), se = 0.15, statistic = 3.0, df = 40)
```

    Note(s):
    Estimate calculated as log ratio difference between two ratios.

    Estimate       SE        t       df        p       LL       UL 
       1.333    0.150    3.000   40.000    0.062    0.985    1.806 

``` r
# 10. Two ratios and CI provided (infer SE and all inferential stats)
backcalc_ratios(ratio = c(2.1, 1.7), ci = c(1.05, 3.1))
```

    Note(s):
    Estimate calculated as log ratio difference between two ratios.
    SE approximated from CI using log scale and critical value.

    Estimate       SE        z       df        p       LL       UL 
       1.235    0.276    0.765       NA    0.444    0.719    2.123 

### Insufficient Information Cases

``` r
# 11. No ratio provided (immediate insufficient info)
backcalc_ratios()
```

    Insufficient information: a ratio (or two ratios) must be provided.

``` r
# 12. Ratio provided but no SE, statistic, or CI (insufficient SE)
backcalc_ratios(ratio = 2.3)
```

    Insufficient information: SE or CI must be provided or inferable.

``` r
# 13. Two-sample ratio vector with SE missing (insufficient info, triggers message)
backcalc_ratios(ratio = c(2.5, 1.9))
```

    Insufficient information: SE or CI must be provided or inferable.

``` r
# 14. Ratio and p-value provided but no SE, statistic, or CI (insufficient SE)
backcalc_ratios(ratio = 1.8, p = 0.05)
```

    Insufficient information: SE or CI must be provided or inferable.

``` r
# 15. Ratio with zero or negative value (invalid for log transform, insufficient info)
backcalc_ratios(ratio = c(-1.5, 2.0))
```

    Ratios must be positive numeric values without NA.
