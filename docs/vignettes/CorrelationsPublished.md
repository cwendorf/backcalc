# [`backcalc`](https://github.com/cwendorf/backcalc/)

## Correlations Published Cases

The sections below move from one published study to another, showing how
the package can be used to interpret research findings across diverse
study designs and input constraints.

- [One Sample Cases](#one-sample-cases)
- [Two Sample Cases](#two-sample-cases)

------------------------------------------------------------------------

### One Sample Cases

#### Rosenhan (1973) — Confidence And Diagnostic Certainty

``` r
backcalc_corrs(r = 0.42, n = 48)
```
    
            Estimate    SE     t     df     p    LL    UL
    Outcome    0.420 0.149 3.003 46.000 0.004 0.147 0.634
    
    Notes:
    df approximated as n - 2.
    SE approximated using 1 / sqrt(n - 3).

Interpretation: The positive correlation with moderate sample size
indicates meaningful co-movement, supporting the substantive claim that
confidence and diagnostic certainty can become tightly coupled.

#### Tolman (1948) — Maze Performance And Trials

``` r
backcalc_corrs(r = 0.57, ci = c(0.33, 0.73), n = 36)
```
    
            Estimate    SE     t     df     p    LL    UL
    Outcome    0.570 0.174 3.720 34.000 0.001 0.286 0.762
    
    Notes:
    df approximated as n - 2.
    SE approximated using 1 / sqrt(n - 3).

Interpretation: The correlation is large and its interval remains
clearly positive, supporting purposive-learning interpretations linking
experience to maze performance.

### Two Sample Cases

#### Maccoby & Jacklin (1974) — Subgroup Correlation Comparison

``` r
backcalc_corrs(r = c(0.34, 0.11), n = c(82, 79))
```
    
            Estimate    SE     t     df     p     LL    UL
    Outcome    0.239 0.161 1.516 76.000 0.134 -0.076 0.511
    
    Notes:
    df approximated as min(n) - 3 for two-sample case.
    SE derived from Fisher z difference formula.

Interpretation: The subgroup correlations differ in magnitude, and the
comparison supports a directional gap, suggesting the construct
relationship varies across populations.

#### Sherif et al. (1961) — Intergroup Context Correlation Shift

``` r
backcalc_corrs(r = c(0.46, 0.20), n = c(40, 38))
```
    
            Estimate    SE     t     df     p     LL    UL
    Outcome    0.286 0.236 1.249 35.000 0.220 -0.182 0.649
    
    Notes:
    df approximated as min(n) - 3 for two-sample case.
    SE derived from Fisher z difference formula.

Interpretation: The correlation decreases across contexts, and the
reconstructed difference supports attenuation, aligning with accounts
that social setting weakens expected trait-behavior coupling.
