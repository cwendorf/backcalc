## backcalc Props Examples

- [One Sample Proportion Cases](#one-sample-proportion-cases)
- [Two Sample Proportion Cases](#two-sample-proportion-cases)

------------------------------------------------------------------------

### One Sample Proportion Cases

``` r
# 1. Basic single-sample (Wald)
backcalc_props(prop = 0.45, n = 100)
```

       logit_p se_logit_p proportion      ci_ll      ci_ul          z          p 
         0.450      0.201      0.611      0.514      0.699      2.239      0.025 

``` r
# 2. Exact interval with counts, more precision
backcalc_props(x = 45, n = 100, interval_type = "exact", sig_digits = 4)
```

       logit_p se_logit_p proportion      ci_ll      ci_ul          z          p 
        0.4500     0.2010     0.6106     0.3503     0.5527     2.2387     0.0252 

``` r
# 3. Exact interval, one-sided test
backcalc_props(x = 30, n = 80, interval_type = "exact", one_sided = TRUE)
```

       logit_p se_logit_p proportion      ci_ll      ci_ul          z      p_one 
         0.375      0.231      0.593      0.284      0.473      1.624      0.052 

``` r
# 4. Given confidence interval (Wald)
backcalc_props(ci = c(0.30, 0.50), n = 80)
```

       logit_p se_logit_p proportion      ci_ll      ci_ul          z          p 
        -0.424      0.216      0.396      0.300      0.500     -1.960      0.050 

``` r
# 5. SE and p-value provided (Wald)
backcalc_props(prop = 0.52, se = 0.05, p = 0.03, n = 90)
```

       logit_p se_logit_p proportion      ci_ll      ci_ul          z          p 
         0.520      0.050      0.627      0.604      0.650     10.400      0.030 

``` r
# 8. One-sided, exact interval, custom rounding
backcalc_props(x = 72, n = 120, one_sided = TRUE, interval_type = "exact", sig_digits = 4)
```

       logit_p se_logit_p proportion      ci_ll      ci_ul          z      p_one 
        0.6000     0.1863     0.6457     0.5210     0.6752     3.2199     0.0006 

``` r
# 10. Exact interval with explicit x
backcalc_props(x = 20, n = 50, interval_type = "exact", sig_digits = 3)
```

       logit_p se_logit_p proportion      ci_ll      ci_ul          z          p 
         0.400      0.289      0.599      0.264      0.548      1.386      0.166 

### Two Sample Proportion Cases

``` r
# 6. Two-sample proportions, Wald intervals
backcalc_props(prop = c(0.55, 0.40), n = c(150, 130))
```

       log_odds_ratio se_log_odds_ratio        odds_ratio             ci_ll 
                0.606             0.243             1.833             1.139 
                ci_ul                 z                 p 
                2.951             2.496             0.013 

``` r
# 7. Two-sample proportions, with p-value and rounding
backcalc_props(prop = c(0.25, 0.35), n = c(100, 110), p = 0.04, sig_digits = 4)
```

       log_odds_ratio se_log_odds_ratio        odds_ratio             ci_ll 
              -0.4796            0.3054            0.6190            0.3402 
                ci_ul                 z                 p 
               1.1265           -1.5701            0.0400 

``` r
# 9. Two-sample proportions with estimates only
backcalc_props(prop = c(0.65, 0.58), n = c(140, 135))
```

       log_odds_ratio se_log_odds_ratio        odds_ratio             ci_ll 
                0.296             0.249             1.345             0.826 
                ci_ul                 z                 p 
                2.189             1.192             0.233 
