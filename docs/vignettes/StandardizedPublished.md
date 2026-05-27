# [`backcalc`](https://github.com/cwendorf/backcalc/)

## Standardized Mean Differences Published Cases

The sections below move from one published study to another, showing how
the package can be used to interpret research findings across diverse
study designs and input constraints.

- [One Sample Cases](#one-sample-cases)
- [Two Sample Cases](#two-sample-cases)

------------------------------------------------------------------------

### One Sample Cases

#### Triplett (1898) — Social Facilitation Effect Size

``` r
backcalc_standard(m = 1.8, sd = 4.9, n = 40, type = "d")
```

    ## 
    ##           Estimate    SE     t     df     p    LL    UL
    ## Cohen's d    0.367 0.163 2.247 39.000 0.030 0.037 0.698
    ## 
    ## Notes:
    ## Effect size calculated from mean difference and SD.

Interpretation: The standardized effect is positive, indicating
nontrivial directional change and supporting the social-facilitation
claim that coaction can enhance performance.

#### Lewin et al. (1939) — Leadership Climate Effect Size

``` r
backcalc_standard(statistic = 2.35, n = 36, type = "g")
```

    ## 
    ##           Estimate    SE     t     df     p    LL    UL
    ## Hedges' g    0.392 0.173 2.262 35.000 0.030 0.040 0.743
    ## 
    ## Notes:
    ## Effect size estimated from t-statistic.

Interpretation: The reconstructed Hedges g remains positive after
small-sample correction, consistent with leadership-climate theories
linking social climate to behavior change.

### Two Sample Cases

#### Bandura et al. (1961) — Aggression Score Contrast

``` r
backcalc_standard(m = c(1.00, 0.46), sd = c(0.95, 0.89), n = c(24, 24), type = "d")
```

    ## 
    ##           Estimate    SE     t     df     p     LL    UL
    ## Cohen's d    0.587 0.295 1.988 46.000 0.053 -0.007 1.181
    ## 
    ## Notes:
    ## Effect size calculated from means and pooled SD.

Interpretation: The between-group standardized difference is positive,
indicating a practical effect size that reinforces social-learning
interpretations of generalized aggression.

#### Darley & Latane (1968) — Helping Latency Contrast

``` r
backcalc_standard(m = c(38.2, 61.5), sd = c(20.4, 27.6), n = c(18, 18), type = "g")
```

    ## 
    ##           Estimate    SE      t     df     p     LL     UL
    ## Hedges' g   -0.939 0.352 -2.665 34.000 0.012 -1.655 -0.223
    ## 
    ## Notes:
    ## Effect size calculated from means and pooled SD.
    ## Hedges' g bias-correction applied.

Interpretation: The standardized contrast is substantial, supporting
bystander-effect mechanisms in which social context alters urgency and
helping latency.
