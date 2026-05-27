# [`backcalc`](https://github.com/cwendorf/backcalc/)

## Means Published Cases

The sections below move from one published study to another, showing how
the package can be used to interpret research findings across diverse
study designs and input constraints.

- [One Sample Cases](#one-sample-cases)
- [Paired Sample Cases](#paired-sample-cases)
- [Two Sample Cases](#two-sample-cases)

------------------------------------------------------------------------

### One Sample Cases

#### Loftus & Palmer (1974) — “Smashed” Condition

``` r
backcalc_means(m = 40.8, se = 1.19, df = 44)
```

    ## 
    ##         Estimate    SE      t     df     p     LL     UL
    ## Outcome   40.800 1.190 34.286 44.000 0.000 38.402 43.198

Interpretation: Because the estimated mean is clearly positive with
moderate uncertainty, the result supports the classic claim that wording
can shift eyewitness judgments, implying reconstructive memory
processes.

#### Dutton & Aron (1974) — High Bridge Condition

``` r
backcalc_means(m = 2.99, se = 0.268, df = 19)
```

    ## 
    ##         Estimate    SE      t     df     p    LL    UL
    ## Outcome    2.990 0.268 11.157 19.000 0.000 2.429 3.551

Interpretation: The positive estimate and relatively small uncertainty
indicate a stable directional effect, consistent with misattribution of
arousal from context to attraction.

### Paired Sample Cases

#### Milgram (1963) — Shock Differences

``` r
backcalc_means(m = 0.15, se = 0.05, df = 39, paired = TRUE)
```

    ## 
    ##         Estimate    SE     t     df     p    LL    UL
    ## Outcome    0.150 0.050 3.000 39.000 0.005 0.049 0.251

Interpretation: The paired estimate is positive and precise relative to
its size, which supports a reliable within-subject shift consistent with
classic obedience effects under authority pressure.

#### Haney et al. (1973) — Aggression Scores

``` r
backcalc_means(m = 2.0, se = 0.306, df = 23, paired = TRUE)
```

    ## 
    ##         Estimate    SE     t     df     p    LL    UL
    ## Outcome    2.000 0.306 6.536 23.000 0.000 1.367 2.633

Interpretation: The positive paired effect with moderate uncertainty
indicates a meaningful condition-related increase, aligning with the
claim that institutional roles can rapidly shape aggressive behavior.

### Two Sample Cases

#### Bandura et al. (1961) — Bobo Doll Study

``` r
backcalc_means(m = 1.0, se = 0.217, df = 45)
```

    ## 
    ##         Estimate    SE     t     df     p    LL    UL
    ## Outcome    1.000 0.217 4.608 45.000 0.000 0.563 1.437

Interpretation: The between-group estimate is positive with acceptable
precision, supporting the substantive social-learning interpretation
that modeled aggression elevates children’s aggressive responding.

#### Asch (1951) — Conformity Rates

``` r
backcalc_means(m = 0.32, se = 0.0259, df = 58)
```

    ## 
    ##         Estimate    SE      t     df     p    LL    UL
    ## Outcome    0.320 0.026 12.355 58.000 0.000 0.268 0.372

Interpretation: The positive estimate paired with very small uncertainty
implies a robust group difference, reinforcing the conformity conclusion
that majority pressure shifts public judgments.
