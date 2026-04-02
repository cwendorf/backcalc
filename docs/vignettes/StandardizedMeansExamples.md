# [`backcalc`](https://github.com/cwendorf/backcalc/)

## Standardized Mean Differences Examples

Each section of examples below progresses from more complete input to
less complete input, moving from miminal required inference to maximal
inference on the part of the function. Similarly, the structure
highlights the flexibility of the function across diverse study designs
and input constraints.

- [One Sample Cases](#one-sample-cases)
- [Two Sample Cases](#two-sample-cases)
- [Different Effect Size Types](#different-effect-size-types)
- [Backcalculation from Test Statistics](#backcalculation-from-test-statistics)
- [Paired Designs](#paired-designs)
- [Insufficient Information Cases](#insufficient-information-cases)

------------------------------------------------------------------------

### One Sample Cases

``` r
# 1. Cohen's d from mean difference, SD, and sample size (minimal inference)
backcalc_standardized_means(m = 2.5, sd = 4, n = 25)
```


    Cohen's d  Estimate  SE     df     LL     UL
               0.625  0.132  24.000 0.360  0.890

    Note(s):
    Effect size calculated from mean difference and SD.
    Degrees of freedom approximated as n - 1.

``` r
# 2. Cohen's d with larger sample (more precision)
backcalc_standardized_means(m = 1.5, sd = 3, n = 100)
```


    Cohen's d  Estimate  SE     df     LL     UL
               0.500  0.071  99.000 0.360  0.640

    Note(s):
    Effect size calculated from mean difference and SD.
    Degrees of freedom approximated as n - 1.

------------------------------------------------------------------------

### Two Sample Cases

``` r
# 3. Cohen's d from two group means, SDs, and sample sizes (equal variances assumed)
backcalc_standardized_means(m = c(15, 12), sd = c(4, 5), n = c(40, 35))
```


    Cohen's d  Estimate  SE     df     LL     UL
               0.636  0.145  73.000 0.351  0.921

    Note(s):
    Effect size calculated from means and pooled SD.

``` r
# 4. Cohen's d for unequal group sizes
backcalc_standardized_means(m = c(20, 18), sd = c(3, 3), n = c(50, 20))
```


    Cohen's d  Estimate  SE     df     LL     UL
               0.667  0.184  68.000 0.304  1.030

    Note(s):
    Effect size calculated from means and pooled SD.

------------------------------------------------------------------------

### Different Effect Size Types

``` r
# 5. Hedges' g (bias-corrected version of Cohen's d, more appropriate for small samples)
backcalc_standardized_means(m = c(15, 12), sd = c(4, 5), n = c(40, 35), type = "g")
```


    Hedges' g  Estimate  SE     df     LL     UL
               0.628  0.143  73.000 0.345  0.911

    Note(s):
    Effect size calculated from means and pooled SD.
    Hedges' g bias-correction applied.

``` r
# 6. Hedges' g with small sample (bias-correction is more pronounced)
backcalc_standardized_means(m = c(10, 8), sd = c(2, 2), n = c(10, 10), type = "g")
```


    Hedges' g  Estimate  SE     df     LL     UL
               0.916  0.352  18.000 0.203  1.629

    Note(s):
    Effect size calculated from means and pooled SD.
    Hedges' g bias-correction applied.

``` r
# 7. Glass's delta (using reference/control group SD only)
backcalc_standardized_means(m = c(15, 12), sd = c(4, 5), n = c(40, 35), type = "delta", control_sd = 2)
```


    Glass's Δ  Estimate  SE     df     LL     UL
               0.600  0.147  73.000 0.309  0.891

    Note(s):
    Glass's delta calculated using group 2 SD.

------------------------------------------------------------------------

### Backcalculation from Test Statistics

``` r
# 8. Estimate effect size from t-statistic and sample size
backcalc_standardized_means(statistic = 2.5, n = 50)
```


    Cohen's d  Estimate  SE     df     LL     UL
               0.354  0.112  49.000 0.130  0.578

    Note(s):
    Effect size estimated from t-statistic.
    Degrees of freedom approximated as n - 1.

``` r
# 9. Two-sample effect size from t-statistic
backcalc_standardized_means(statistic = 3.0, n = c(40, 40))
```


    Cohen's d  Estimate  SE     df     LL     UL
               0.671  0.162  78.000 0.350  0.992

    Note(s):
    Effect size estimated from t-statistic.

------------------------------------------------------------------------

### Paired Designs

``` r
# 10. Paired samples Cohen's d
backcalc_standardized_means(m = 1.2, sd = 2.5, n = 30, paired = TRUE)
```


    Cohen's d  Estimate  SE     df     LL     UL
               0.480  0.114  29.000 0.248  0.712

    Note(s):
    Effect size calculated from mean difference and SD.
    Degrees of freedom approximated as n - 1 for paired design.

------------------------------------------------------------------------

### Insufficient Information Cases

``` r
# 11. Insufficient input (only effect size, no SE or CI information)
backcalc_standardized_means(d = 0.5)
```


    Insufficient Input:
    Provide either: (1) effect size d, or (2) means/mean-diff with SD, or (3) statistic with sample size.
