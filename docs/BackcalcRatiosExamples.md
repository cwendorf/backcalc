## backcalc Ratios Examples

- [One Sample Ratio Cases](#one-sample-ratio-cases)
- [Two Sample Ratio Cases](#two-sample-ratio-cases)

------------------------------------------------------------------------

### One Sample Ratio Cases

``` r
# 1. Ratio + SE only (z-test)
backcalc_ratios(ratio = 1.5, se = 0.2, sig_digits = 3)
```

    ratio    se    df ci_ll ci_ul     z     p 
    1.500 0.200    NA 1.014 2.220 2.027 0.043 

``` r
# 2. Ratio + confidence interval + df
backcalc_ratios(ratio = 1.7, ci = c(1.1, 2.5), df = 18, sig_digits = 3)
```

     ratio     se     df  ci_ll  ci_ul      t      p 
     1.700  0.195 18.000  1.128  2.563  2.716  0.014 

``` r
# 3. Ratio + SE + df
backcalc_ratios(ratio = 1.4, se = 0.12, df = 20, sig_digits = 3)
```

     ratio     se     df  ci_ll  ci_ul      t      p 
     1.400  0.120 20.000  1.090  1.798  2.804  0.011 

### Two Sample Ratio Cases

``` r
# 4. Two-sample equal df
backcalc_ratios(ratio = c(2.2, 1.5), se = c(0.15, 0.10), df = 20, sig_digits = 3)
```

     ratio     se     df  ci_ll  ci_ul      t      p 
     0.682  0.180 20.000  0.468  0.993 -2.124  0.046 

``` r
# 5. Two-sample unequal df (Welch correction)
backcalc_ratios(ratio = c(2.5, 1.7), se = c(0.18, 0.12), df = c(18, 22), sig_digits = 3)
```

     ratio     se     df  ci_ll  ci_ul      t      p 
     0.680  0.216     NA     NA     NA -1.783     NA 

``` r
# 6. Ratio difference + SE + df provided
backcalc_ratios(ratio = 0.25, se = 0.09, df = 40, sig_digits = 3)
```

      ratio      se      df   ci_ll   ci_ul       t       p 
      0.250   0.090  40.000   0.208   0.300 -15.403   0.000 

``` r
# 7. Ratio difference + confidence interval + df
backcalc_ratios(ratio = 0.35, ci = c(0.10, 0.60), df = 38, sig_digits = 3)
```

     ratio     se     df  ci_ll  ci_ul      t      p 
     0.350  0.443 38.000  0.143  0.857 -2.372  0.023 
