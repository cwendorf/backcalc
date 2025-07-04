## Medians Examples

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
# 1.  Median, IQR, and sample size
backcalc_medians(m = 50, iqr = 20, n = 30)
```

    Note(s):
    SE approximated from IQR and sample size using normal approximation.

    Estimate       SE        z       df        p       LL       UL 
      50.000    2.707   18.472       NA    0.000   44.695   55.305 

``` r
# 2.  Median, MAD, and sample size
backcalc_medians(m = 48, mad = 12, n = 25)
```

    Note(s):
    SE approximated from MAD and sample size using normal approximation.

    Estimate       SE        z       df        p       LL       UL 
      48.000    3.558   13.490       NA    0.000   41.026   54.974 

``` r
# 3.  Median, IQR, paired design
backcalc_medians(m = 42, iqr = 15, n = 20, paired = TRUE)
```

    Note(s):
    SE approximated from IQR and sample size using normal approximation.

    Estimate       SE        z       df        p       LL       UL 
      42.000    2.486   16.892       NA    0.000   37.127   46.873 

``` r
# 4.  Median, MAD, one-sided test
backcalc_medians(m = 55, mad = 10, n = 40, one_sided = TRUE)
```

    Note(s):
    SE approximated from MAD and sample size using normal approximation.

    Estimate       SE        z       df    p-one       LL       UL 
      55.000    2.344   23.463       NA    0.000   51.144   58.856 

``` r
# 5. median + MAD + n, no paired, no one-sided
backcalc_medians(m = 55, range = 30, n = 20)
```

    Note(s):
    SE approximated from range and sample size using rough normal approximation.

    Estimate       SE        z       df        p       LL       UL 
      55.000    1.677   32.796       NA    0.000   51.713   58.287 

### Two Sample Cases

``` r
# 6.  Group medians, IQR, and sample size
backcalc_medians(m = c(60, 50), iqr = 20, n = 30)
```

    Note(s):
    SE approximated from IQR and sample size using normal approximation.

    Estimate       SE        z       df        p       LL       UL 
      10.000    2.707    3.694       NA    0.000    4.695   15.305 

``` r
# 7.  Group medians, MAD, and sample size
backcalc_medians(m = c(75, 68), mad = 9, n = 40)
```

    Note(s):
    SE approximated from MAD and sample size using normal approximation.

    Estimate       SE        z       df        p       LL       UL 
       7.000    2.110    3.318       NA    0.001    2.865   11.135 

``` r
# 8.  Paired medians, IQR, and n
backcalc_medians(m = c(45, 40), iqr = 10, n = 20, paired = TRUE)
```

    Note(s):
    SE approximated from IQR and sample size using normal approximation.

    Estimate       SE        z       df        p       LL       UL 
       5.000    1.658    3.016       NA    0.003    1.751    8.249 

``` r
# 9.  Group medians, IQR, n, one-sided
backcalc_medians(m = c(82, 78), iqr = 12, n = 35, one_sided = TRUE)
```

    Note(s):
    SE approximated from IQR and sample size using normal approximation.

    Estimate       SE        z       df    p-one       LL       UL 
       4.000    1.504    2.660       NA    0.004    1.527    6.473 

``` r
# 10. two medians, two MADs, two sample sizes (no IQR used here)
backcalc_medians(m = c(92, 85), mad = c(7, 10), n = c(25, 28))
```

    Note(s):
    SE approximated from MAD and sample size using normal approximation.

    Estimate       SE        z       df        p       LL       UL 
       7.000    3.487    2.008       NA    0.045    0.166   13.834 

### Insufficient Information Cases

``` r
# 11. Only IQR provided
backcalc_medians(iqr = 10)
```

    Insufficient information: Provide estimate and at least one of SE, p-value, CI, or test statistic.

``` r
# 12. Only MAD provided
backcalc_medians(mad = 5)
```

    Insufficient information: Provide estimate and at least one of SE, p-value, CI, or test statistic.

``` r
# 13. Only sample size provided
backcalc_medians(n = 30)
```

    Insufficient information: Provide estimate and at least one of SE, p-value, CI, or test statistic.

``` r
# 14. Median provided, no n or spread
backcalc_medians(m = 50)
```

    Insufficient information: Provide estimate and at least one of SE, p-value, CI, or test statistic.

``` r
# 15. Medians and sample size, but no dispersion
backcalc_medians(m = c(52, 49), n = 30)
```

    Insufficient information: Provide estimate and at least one of SE, p-value, CI, or test statistic.
