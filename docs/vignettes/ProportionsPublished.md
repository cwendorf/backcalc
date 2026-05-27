# [`backcalc`](https://github.com/cwendorf/backcalc/)

## Proportions Published Cases

The sections below move from one published study to another, showing how
the package can be used to interpret research findings across diverse
study designs and input constraints.

- [One Sample Cases](#one-sample-cases)
- [Two Sample Cases](#two-sample-cases)

------------------------------------------------------------------------

### One Sample Cases

#### Milgram (1963) — Baseline Obedience Rate

``` r
backcalc_props(x = 26, n = 40)
```

    ## 
    ##         Estimate    SE     z df     p    LL    UL
    ## Outcome    0.650 0.075 8.619 NA 0.000 0.502 0.798
    ## 
    ## Notes:
    ## P-value computed from estimated test statistic.

Interpretation: The observed proportion is high, and its reconstructed
interval remains well above trivial levels, supporting Milgram’s
substantive conclusion that many participants complied under authority
pressure.

#### Darley & Batson (1973) — Helping In The Seminary Study

``` r
backcalc_props(x = 10, n = 40)
```

    ## 
    ##         Estimate    SE     z df     p    LL    UL
    ## Outcome    0.250 0.068 3.651 NA 0.000 0.116 0.384
    ## 
    ## Notes:
    ## P-value computed from estimated test statistic.

Interpretation: The helping proportion is low, and the uncertainty band
stays far from ceiling behavior, supporting the situationist
interpretation that context can suppress prosocial action.

### Two Sample Cases

#### Milgram (1963) — Baseline Vs. Touch-Proximity Conditions

``` r
backcalc_props(x = c(26, 12), n = c(40, 40))
```

    ## 
    ##         Estimate    SE statistic df     p    LL    UL
    ## Outcome    0.350 0.105     3.347 NA 0.001 0.145 0.555
    ## 
    ## Notes:
    ## P-value computed from estimated test statistic.

Interpretation: The two-group difference in proportions is large and
directionally consistent, supporting the claim that situational
manipulations of social distance can materially reduce obedience.

#### Latane & Darley (1968) — Seizure Study, Alone Vs. Group Condition

``` r
backcalc_props(x = c(11, 4), n = c(13, 13))
```

    ## 
    ##         Estimate    SE statistic df     p    LL    UL
    ## Outcome    0.538 0.162     3.314 NA 0.001 0.220 0.857
    ## 
    ## Notes:
    ## P-value computed from estimated test statistic.

Interpretation: The substantial proportion gap indicates a strong
directional effect, substantiating the bystander-effect claim that
shared responsibility diffuses intervention.
