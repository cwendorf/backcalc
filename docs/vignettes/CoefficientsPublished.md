# [`backcalc`](https://github.com/cwendorf/backcalc/)

## Coefficients Published Cases

The sections below move from one published study to another, showing how
the package can be used to interpret research findings across diverse
study designs and input constraints.

- [Single Coefficient Cases](#single-coefficient-cases)
- [Standardized Coefficient Cases](#standardized-coefficient-cases)

------------------------------------------------------------------------

### Single Coefficient Cases

#### Thorndike (1904) — Practice To Performance Slope

``` r
backcalc_coeffs(b = 0.42, se = 0.11, df = 58)
```
    
            Estimate    SE     t     df     p    LL    UL
    Outcome    0.420 0.110 3.818 58.000 0.000 0.200 0.640
    
    Notes:
    Sample size inferred from df (n = df + 1).
    p-value computed from statistic.

Interpretation: The slope is positive with moderate precision,
supporting the substantive practice principle that repeated exposure
improves performance.

#### Yerkes & Dodson (1908) — Arousal-Performance Slope

``` r
backcalc_coeffs(b = -0.31, p = 0.02, df = 44)
```
    
            Estimate    SE      t     df     p     LL     UL
    Outcome   -0.310 0.128 -2.414 44.000 0.020 -0.569 -0.051
    
    Notes:
    Sample size inferred from df (n = df + 1).
    Statistic approximated from p-value and estimate.
    SE approximated from estimate and reconstructed statistic.

Interpretation: The coefficient is negative, and the reconstructed
evidence supports a non-null slope, supporting the Yerkes-Dodson claim
that high arousal can impair performance.

### Standardized Coefficient Cases

#### Cronbach (1951) — Reliability Predictor Weight

``` r
backcalc_coeffs(std_beta = 0.38, se_std = 0.14, df = 62)
```
    
            Estimate    SE     t     df     p    LL    UL
    Outcome    0.380 0.140 2.714 62.000 0.009 0.100 0.660
    
    Notes:
    Sample size inferred from df (n = df + 1).
    p-value computed from statistic.

Interpretation: The standardized coefficient is positive and moderate,
indicating a meaningful predictor contribution and reinforcing the
practical importance of reliability-related predictors.

#### Meehl (1954) — Criterion Validity Predictor Weight

``` r
backcalc_coeffs(b = 1.7, se = 0.55, sd_x = 4.1, sd_y = 9.6, n = 52)
```
    
            Estimate    SE     t     df     p    LL    UL
    Outcome    0.726 0.235 3.091 51.000 0.003 0.254 1.198
    
    Notes:
    df inferred from sample size (df = n - 1).
    Standardized beta approximated from unstandardized beta and standard deviations.
    SE of standardized beta approximated from unstandardized SE and SDs.
    p-value computed from statistic.

Interpretation: The raw effect is positive with moderate uncertainty,
and the standardized conversion preserves direction, supporting
criterion-validity interpretations.
