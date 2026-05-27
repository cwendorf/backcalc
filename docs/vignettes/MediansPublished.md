# [`backcalc`](https://github.com/cwendorf/backcalc/)

## Medians Published Cases

The sections below move from one published study to another, showing how
the package can be used to interpret research findings across diverse
study designs and input constraints.

- [One Sample Cases](#one-sample-cases)
- [Paired Sample Cases](#paired-sample-cases)
- [Two Sample Cases](#two-sample-cases)

------------------------------------------------------------------------

### One Sample Cases

#### Ebbinghaus (1885) — Retention Interval Comparison

``` r
backcalc_medians(mdn = 36, iqr = 18, n = 28)
```

    ## 
    ##         Estimate    SE      z df     p     LL     UL
    ## Outcome   36.000 2.522 14.276 NA 0.000 31.058 40.942
    ## 
    ## Notes:
    ## SE approximated from IQR and sample size using normal approximation.

Interpretation: The positive central estimate with nontrivial dispersion
indicates a stable retention level but broad spread, supporting the
substantive conclusion that memory changes substantially over delay.

#### Rosenthal & Jacobson (1968) — Gain Score Median

``` r
backcalc_medians(mdn = 5.0, mad = 2.7, n = 24)
```

    ## 
    ##         Estimate    SE     z df     p    LL    UL
    ## Outcome    5.000 0.817 6.119 NA 0.000 3.399 6.601
    ## 
    ## Notes:
    ## SE approximated from MAD and sample size using normal approximation.

Interpretation: The positive median gain and moderate uncertainty
support a directional improvement pattern consistent with
expectancy-based classroom effects.

### Paired Sample Cases

#### Stroop (1935) — Interference Condition Difference Scores

``` r
backcalc_medians(mdn = 7.0, iqr = 9.5, n = 32, paired = TRUE)
```

    ## 
    ##         Estimate    SE     z df     p    LL    UL
    ## Outcome    7.000 1.245 5.623 NA 0.000 4.560 9.440
    ## 
    ## Notes:
    ## SE approximated from IQR and sample size using normal approximation.

Interpretation: The paired median shift with appreciable spread
indicates a consistent directional interference effect, matching the
Stroop claim that automatic reading competes with controlled color
naming.

#### Tversky & Kahneman (1974) — Judgment Shift Scores

``` r
backcalc_medians(mdn = 4.0, range = 20, n = 30, paired = TRUE)
```

    ## 
    ##         Estimate    SE     z df     p    LL    UL
    ## Outcome    4.000 0.913 4.382 NA 0.000 2.211 5.789
    ## 
    ## Notes:
    ## SE approximated from range and sample size using rough normal approximation.

Interpretation: The positive paired shift alongside broad uncertainty
still supports the directional claim predicted by
anchoring-and-adjustment accounts.

### Two Sample Cases

#### Festinger & Carlsmith (1959) — Dissonance Condition Ratings

``` r
backcalc_medians(mdn = c(7.0, 5.5), iqr = c(2.4, 2.8), n = c(20, 20))
```

    ## 
    ##         Estimate    SE     z df     p    LL    UL
    ## Outcome    1.500 0.611 2.454 NA 0.014 0.302 2.698
    ## 
    ## Notes:
    ## SE approximated from IQR and sample size using normal approximation.

Interpretation: The median contrast with moderate dispersion in both
groups supports a directional between-condition difference aligned with
cognitive dissonance interpretations.

#### Mischel et al. (1972) — Delay-Choice Outcomes

``` r
backcalc_medians(mdn = c(15, 9), mad = c(4, 5), n = c(26, 24))
```

    ## 
    ##         Estimate    SE     z df     p    LL    UL
    ## Outcome    6.000 1.908 3.144 NA 0.002 2.259 9.741
    ## 
    ## Notes:
    ## SE approximated from MAD and sample size using normal approximation.

Interpretation: The sizable median gap, despite group variability,
indicates a meaningful condition difference consistent with
delay-of-gratification accounts of self-control.
