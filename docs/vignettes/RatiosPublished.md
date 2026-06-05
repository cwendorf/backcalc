# [`backcalc`](https://github.com/cwendorf/backcalc/)

## Ratios Published Cases

The sections below move from one published study to another, showing how
the package can be used to interpret research findings across diverse
study designs and input constraints.

- [One Sample Cases](#one-sample-cases)
- [Two Sample Cases](#two-sample-cases)

------------------------------------------------------------------------

### One Sample Cases

#### Weber (1834) — Just-Noticeable Difference Ratio Summaries

``` r
backcalc_ratios(ratio = 1.32, se = 0.09, df = 29)
```
    
            Estimate    SE     t     df     p    LL    UL
    Outcome    1.320 0.090 3.085 29.000 0.004 1.098 1.587

Interpretation: The ratio estimate exceeds one and is reasonably
precise, which supports the Weberian claim that sensitivity tracks
proportional, not absolute, change.

#### Fechner (1860) — Psychophysical Ratio Estimate

``` r
backcalc_ratios(ratio = 1.18, ci = c(1.05, 1.33), n = 41)
```
    
            Estimate    SE     t     df     p    LL    UL
    Outcome    1.180 0.058 2.830 40.000 0.007 1.048 1.328
    
    Notes:
    df approximated as n - 1.
    SE approximated from CI using log scale and critical value.

Interpretation: The ratio remains above one with an interval that also
stays above one, supporting Fechner-style scaling in which perceived
intensity grows systematically with stimulus magnitude.

### Two Sample Cases

#### Stevens (1957) — Magnitude Estimation By Task

``` r
backcalc_ratios(ratio = c(1.48, 1.21), se = c(0.11, 0.10), df = c(22, 24))
```
    
            Estimate    SE     t     df     p    LL    UL
    Outcome    1.223 0.149 1.355 23.000 0.189 0.899 1.664
    
    Notes:
    Estimate calculated as log ratio difference between two ratios.
    SE combined using sqrt(se1^2 + se2^2).
    df vector provided but SE of difference only; df not adjusted.

Interpretation: The first context shows a larger ratio, and combined
uncertainty information supports a reliable directional contrast,
consistent with context-dependent judgment scaling.

#### Shepard (1967) — Generalization Ratio By Context

``` r
backcalc_ratios(ratio = c(1.61, 1.27), ci = c(1.08, 1.79), df = 30)
```
    
            Estimate    SE     t     df     p    LL    UL
    Outcome    1.268 0.124 1.918 30.000 0.065 0.985 1.632
    
    Notes:
    Estimate calculated as log ratio difference between two ratios.
    SE approximated from CI using log scale and critical value.

Interpretation: The comparison of ratios, together with interval-based
uncertainty, supports a positive contextual shift consistent with
generalization accounts of transfer strength.
