# [`backcalc`](https://github.com/cwendorf/backcalc/)

## Proportions Examples

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
# 1. Least inference: Provide proportion and sample size only
backcalc_props(prop = 0.4, n = 100)
```


            Estimate    SE     z df     p    LL    UL
    Outcome    0.400 0.049 8.165 NA 0.000 0.304 0.496

    Note(s):
    P-value computed from estimated test statistic.

``` r
# 2. Provide count and sample size, infer proportion and SE
backcalc_props(x = 45, n = 120)
```


            Estimate    SE     z df     p    LL    UL
    Outcome    0.375 0.044 8.485 NA 0.000 0.288 0.462

    Note(s):
    P-value computed from estimated test statistic.

``` r
# 3. Provide proportion, sample size, and test statistic (z)
backcalc_props(prop = 0.5, n = 150, statistic = 2.1)
```


            Estimate    SE     z df     p    LL    UL
    Outcome    0.500 0.041 2.100 NA 0.036 0.420 0.580

    Note(s):
    P-value computed from estimated test statistic.

``` r
# 4. Provide proportion, sample size, and p-value, infer SE and statistic
backcalc_props(prop = 0.35, n = 80, p = 0.04)
```


            Estimate    SE     z df     p    LL    UL
    Outcome    0.350 0.053 6.563 NA 0.040 0.245 0.455

``` r
# 5. Provide proportion, t-statistic, and degrees of freedom, infer SE and p
backcalc_props(prop = 0.6, statistic = 2.2, df = 19)
```


            Estimate    SE     z df     p    LL    UL
    Outcome    0.600 0.045 2.200 NA 0.028 0.511 0.689

    Note(s):
    Sample size inferred from statistic assuming null proportion = 0.5.
    P-value computed from estimated test statistic.

### Two Sample Cases

``` r
# 6. Least inference: Provide two proportions and sample sizes only
backcalc_props(prop = c(0.55, 0.4), n = c(150, 130))
```


            Estimate    SE statistic df     p    LL    UL
    Outcome    0.150 0.059     2.537 NA 0.011 0.034 0.266

    Note(s):
    P-value computed from estimated test statistic.

``` r
# 7. Provide two proportions, sample sizes, and test statistic (z)
backcalc_props(prop = c(0.6, 0.5), n = c(120, 110), statistic = 2.2)
```


            Estimate    SE statistic df     p     LL    UL
    Outcome    0.100 0.065     2.200 NA 0.028 -0.028 0.228

    Note(s):
    P-value computed from estimated test statistic.

``` r
# 8. Provide two proportions, sample sizes, and p-value, infer SE and statistic
backcalc_props(prop = c(0.3, 0.35), n = c(100, 110), p = 0.05)
```


            Estimate    SE statistic df     p     LL    UL
    Outcome   -0.050 0.065    -0.774 NA 0.050 -0.177 0.077

``` r
# 9. Provide counts and sample sizes for two groups, infer difference and SE
backcalc_props(x = c(40, 30), n = c(100, 90))
```


            Estimate    SE statistic df     p     LL    UL
    Outcome    0.067 0.070     0.955 NA 0.339 -0.070 0.203

    Note(s):
    P-value computed from estimated test statistic.

``` r
# 10. Provide two proportions, p-value, and degrees of freedom (t-test)
backcalc_props(prop = c(0.45, 0.5), p = 0.04, df = 50)
```


            Estimate    SE statistic     df     p     LL     UL
    Outcome   -0.050 0.024    -2.109 50.000 0.040 -0.098 -0.002

    Note(s):
    Standard error and confidence interval estimated from test statistic and p-value.

### Insufficient Information Cases

``` r
# 11. No prop, no counts, no SE — cannot infer
backcalc_props()
```


    Insufficient Input:
    Provide 'prop' or ('x' and 'n').

``` r
# 12. Provide statistic and sample size only, no prop or se — infer prop and SE
backcalc_props(statistic = 2.1, n = 50)
```


    Insufficient Input:
    Provide 'prop' or ('x' and 'n').

``` r
# 13. Provide n only, no prop, counts, or se
backcalc_props(n = 100)
```


    Insufficient Input:
    Provide 'prop' or ('x' and 'n').

``` r
# 14. Provide counts but missing sample size
backcalc_props(x = 30)
```


    Insufficient Input:
    Provide 'prop' or ('x' and 'n').

``` r
# 15. Provide CI but no counts or proportion — cannot compute estimate or SE
backcalc_props(ci = c(0.2, 0.8))
```


    Insufficient Input:
    Provide 'prop' or ('x' and 'n').
