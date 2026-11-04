# [`backcalc`](https://github.com/cwendorf/backcalc/)

## Standardized Mean Differences Examples

Each section of examples below progresses from more complete input to
less complete input, moving from minimal required inference to maximal
inference on the part of the function. Similarly, the structure
highlights the flexibility of the function across diverse study designs
and input constraints.

- [One Sample Cases](#one-sample-cases)
- [Two and Paired Sample Cases](#two-and-paired-sample-cases)
- [Insufficient Information Cases](#insufficient-information-cases)

------------------------------------------------------------------------

### One Sample Cases

#### 1. Cohen's d from mean difference, SD, and sample size (minimal inference)

``` r
backcalc_standard(m = 2.5, sd = 4, n = 25)
```


              Estimate    SE     t     df     p    LL    UL
    Cohen's d    0.625 0.219 2.849 24.000 0.009 0.172 1.078

    Notes:
    Effect size calculated from mean difference and SD. 

#### 2. Hedges' g from one-sample input (bias-corrected d)

``` r
backcalc_standard(m = 1.5, sd = 3, n = 30, type = "g")
```


              Estimate    SE     t     df     p    LL    UL
    Hedges' g    0.487 0.193 2.517 29.000 0.018 0.091 0.883

    Notes:
    Effect size calculated from mean difference and SD.
    Hedges' g bias-correction applied. 

#### 3. One-sample effect size from t-statistic and sample size

``` r
backcalc_standard(statistic = 2.5, n = 50)
```


              Estimate    SE     t     df     p    LL    UL
    Cohen's d    0.354 0.146 2.424 49.000 0.019 0.060 0.647

    Notes:
    Effect size estimated from t-statistic. 

#### 4. One-sample estimate from CI midpoint plus sample size

``` r
backcalc_standard(ci_d = c(0.2, 0.8), n = 40)
```


              Estimate    SE     t     df     p    LL    UL
    Cohen's d    0.500 0.168 2.977 39.000 0.005 0.160 0.840

    Notes:
    Effect size estimated from CI midpoint. 

#### 5. Directly supplied d with sample size to obtain SE and CI

``` r
backcalc_standard(d = 0.45, n = 60)
```


              Estimate    SE     t     df     p    LL    UL
    Cohen's d    0.450 0.136 3.319 59.000 0.002 0.179 0.721

### Two and Paired Sample Cases

#### 6. Two-sample Cohen's d from means, SDs, and sample sizes

``` r
backcalc_standard(m = c(15, 12), sd = c(4, 5), n = c(40, 35))
```


              Estimate    SE     t     df     p    LL    UL
    Cohen's d    0.668 0.238 2.806 73.000 0.006 0.193 1.142

    Notes:
    Effect size calculated from means and pooled SD. 

#### 7. Two-sample Hedges' g with unequal group sizes

``` r
backcalc_standard(m = c(20, 18), sd = c(3, 3), n = c(50, 20), type = "g")
```


              Estimate    SE     t     df     p    LL    UL
    Hedges' g    0.659 0.271 2.437 68.000 0.017 0.119 1.199

    Notes:
    Effect size calculated from means and pooled SD.
    Hedges' g bias-correction applied. 

#### 8. Two-sample Glass's delta using group 2 as control SD

``` r
backcalc_standard(m = c(15, 12), sd = c(4, 5), n = c(40, 35), type = "delta", control_sd = 2)
```


              Estimate    SE     t     df     p    LL    UL
    Glass's Δ    0.600 0.237 2.535 73.000 0.013 0.128 1.072

    Notes:
    Effect size calculated from means and pooled SD.
    Glass's delta calculated using group 2 SD. 

#### 9. Two-sample effect size from t-statistic with SDs and group sample sizes

``` r
backcalc_standard(statistic = 3.0, sd = c(4, 5), n = c(40, 40))
```


              Estimate    SE     t     df     p    LL    UL
    Cohen's d    0.474 0.227 2.091 78.000 0.040 0.023 0.926

    Notes:
    Effect size estimated from t-statistic. 

#### 10. Paired-sample Cohen's d from mean difference, SD, and n

``` r
backcalc_standard(m = 1.2, sd = 2.5, n = 30, paired = TRUE)
```


              Estimate    SE     t     df     p    LL    UL
    Cohen's d    0.480 0.193 2.485 29.000 0.019 0.085 0.875

    Notes:
    Degrees of freedom approximated as n - 1 for paired design.
    Effect size calculated from mean difference and SD. 

### Insufficient Information Cases

#### 11. Sample size only, no effect size, mean difference, or statistic

``` r
backcalc_standard(n = 40)
```


    Insufficient Input:
    Provide either: (1) effect size d, or (2) means/mean-diff with SD, or (3) statistic with sample size.

#### 12. SD and sample size provided, but no mean difference or effect size

``` r
backcalc_standard(sd = 3, n = 35)
```


    Insufficient Input:
    Provide either: (1) effect size d, or (2) means/mean-diff with SD, or (3) statistic with sample size.

#### 13. Test statistic provided without sample size

``` r
backcalc_standard(statistic = 2.1)
```


    Insufficient Input:
    Provide either: (1) effect size d, or (2) means/mean-diff with SD, or (3) statistic with sample size.

#### 14. P-value and sample size provided, but no effect size or statistic

``` r
backcalc_standard(p = 0.03, n = 45)
```


    Insufficient Input:
    Provide either: (1) effect size d, or (2) means/mean-diff with SD, or (3) statistic with sample size.

#### 15. Mean difference only, missing SD and effect size

``` r
backcalc_standard(m = 1.8)
```


    Insufficient Input:
    Provide either: (1) effect size d, or (2) means/mean-diff with SD, or (3) statistic with sample size.
