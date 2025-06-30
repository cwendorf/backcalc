## Proportions Examples

- [One Sample Cases](#one-sample-cases)
- [Two Sample Cases](#two-sample-cases)
- [Cases That Do Not Work](#cases-that-do-not-work)

------------------------------------------------------------------------

### One Sample Cases

``` r
# 1. Basic single-sample (Wald)
backcalc_props(prop = 0.45, n = 100)
```

    Note(s):
    SE estimated from prop and n using Wald approximation.

    Estimate       SE        z        p       LL       UL 
       0.450    0.201    2.239    0.025    0.514    0.699 

``` r
# 2. Exact interval with counts, more precision
backcalc_props(x = 45, n = 100, interval_type = "exact", digits = 4)
```

    Note(s):
    Estimate computed as x/n.
    SE estimated from prop and n using Wald approximation.

    Estimate       SE        z        p       LL       UL 
      0.4500   0.2010   2.2387   0.0252   0.5140   0.6993 

``` r
# 3. Exact interval, one-sided test
backcalc_props(x = 30, n = 80, interval_type = "exact", one_sided = TRUE)
```

    Note(s):
    Estimate computed as x/n.
    SE estimated from prop and n using Wald approximation.

    Estimate       SE        z    p_one       LL       UL 
       0.375    0.231    1.624    0.052    0.520    0.662 

``` r
# 4. Given confidence interval (Wald)
backcalc_props(ci = c(0.30, 0.50), prop = 0.40, n = 80)
```

    Note(s):
    SE estimated from prop and n using Wald approximation.

    Estimate       SE        z        p       LL       UL 
       0.400    0.228    1.753    0.080    0.488    0.700 

``` r
# 5. SE and p-value provided (Wald)
backcalc_props(prop = 0.52, se = 0.05, p = 0.03, n = 90)
```

    Estimate       SE        z        p       LL       UL 
       0.520    0.050   10.400    0.030    0.604    0.650 

``` r
# 6. One-sided, exact interval, custom rounding
backcalc_props(x = 72, n = 120, one_sided = TRUE, interval_type = "exact", digits = 4)
```

    Note(s):
    Estimate computed as x/n.
    SE estimated from prop and n using Wald approximation.

    Estimate       SE        z    p_one       LL       UL 
      0.6000   0.1863   3.2199   0.0006   0.5893   0.6982 

``` r
# 7. Exact interval with explicit x
backcalc_props(x = 20, n = 50, interval_type = "exact", digits = 3)
```

    Note(s):
    Estimate computed as x/n.
    SE estimated from prop and n using Wald approximation.

    Estimate       SE        z        p       LL       UL 
       0.400    0.289    1.386    0.166    0.459    0.724 

### Two Sample Cases

``` r
# 8. Two-sample proportions, Wald intervals
backcalc_props(prop = c(0.55, 0.40), n = c(150, 130))
```

    Note(s):
    Two-sample SE calculated using Wald formula for difference in proportions.

    Estimate       SE        z        p       LL       UL 
       0.150    0.059    2.537    0.011    0.034    0.266 

``` r
# 9. Two-sample proportions, with p-value and rounding
backcalc_props(prop = c(0.25, 0.35), n = c(100, 110), p = 0.04, digits = 4)
```

    Note(s):
    Two-sample SE calculated using Wald formula for difference in proportions.

    Estimate       SE        z        p       LL       UL 
     -0.1000   0.0628  -1.5925   0.0400  -0.2231   0.0231 

``` r
# 10. Two-sample proportions with estimates only
backcalc_props(prop = c(0.65, 0.58), n = c(140, 135))
```

    Note(s):
    Two-sample SE calculated using Wald formula for difference in proportions.

    Estimate       SE        z        p       LL       UL 
       0.070    0.059    1.195    0.232   -0.045    0.185 

### Cases That Do Not Work

``` r
# 11. No inputs at all - nothing to compute
backcalc_props()
```

    Provide 'prop' or both 'x' and 'n'. 

``` r
# 12. Single proportion but missing sample size 'n' (needed for SE estimation)
backcalc_props(prop = 0.5)
```

    Insufficient information: provide 'se', 'n', 'ci', or 'p' with 'estimate'. 

``` r
# 13. Two proportions provided but sample sizes missing (needed for two-sample calculation)
backcalc_props(prop = c(0.3, 0.4))
```

    Insufficient information: provide 'se', 'n', 'ci', or 'p' with 'estimate'. 

``` r
# 14. Exact interval requested without providing counts 'x' and sample size 'n'
backcalc_props(prop = 0.6, interval_type = "exact")
```

    Insufficient information: provide 'se', 'n', 'ci', or 'p' with 'estimate'. 
