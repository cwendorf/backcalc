# [`backcalc`](https://github.com/cwendorf/backcalc/)

## Regression Published Cases

The sections below move from one published study to another, showing how
the package can be used to interpret research findings across diverse
input constraints for multiple regression coefficients.

- [Multiple Regression Cases](#multiple-regression-cases)

------------------------------------------------------------------------

### Multiple Regression Cases

#### Coleman et al. (1966) — School Outcomes Model

``` r
backcalc_multreg(
  intercept = 2.1,
  intercept_se = 0.6,
  b = c(0.45, -0.28, 0.33),
  se = c(0.12, 0.10, 0.11),
  n = 120
)
```

    ## 
    ##           Estimate    SE      t      df     p     LL     UL
    ## Intercept    2.100 0.600  3.500 119.000 0.001  0.912  3.288
    ## X1           0.450 0.120  3.750 119.000 0.000  0.212  0.688
    ## X2          -0.280 0.100 -2.800 119.000 0.006 -0.478 -0.082
    ## X3           0.330 0.110  3.000 119.000 0.003  0.112  0.548
    ## 
    ## Notes:
    ## Intercept: p-value computed from statistic.
    ## X1: p-value computed from statistic.
    ## X2: p-value computed from statistic.
    ## X3: p-value computed from statistic.

Interpretation: The model includes both positive and negative
coefficients with moderate uncertainty, supporting a multicausal
substantive account where predictors influence outcomes in different
directions.

#### Jencks et al. (1972) — Educational Attainment Model

``` r
backcalc_multreg(
  b = c(0.52, 0.19, -0.14, 0.27),
  p = c(0.001, 0.04, 0.09, 0.02),
  n = 150
)
```

    ## 
    ##    Estimate    SE      t      df     p     LL    UL
    ## X1    0.520 0.155  3.357 149.000 0.001  0.214 0.826
    ## X2    0.190 0.092  2.072 149.000 0.040  0.009 0.371
    ## X3   -0.140 0.082 -1.706 149.000 0.090 -0.302 0.022
    ## X4    0.270 0.115  2.352 149.000 0.020  0.043 0.497
    ## 
    ## Notes:
    ## X1: Statistic approximated from p-value and estimate. SE approximated from estimate and reconstructed statistic.
    ## X2: Statistic approximated from p-value and estimate. SE approximated from estimate and reconstructed statistic.
    ## X3: Statistic approximated from p-value and estimate. SE approximated from estimate and reconstructed statistic.
    ## X4: Statistic approximated from p-value and estimate. SE approximated from estimate and reconstructed statistic.

Interpretation: The evidence pattern indicates several predictors with
clearer support and one weaker effect, supporting the claim that
attainment is jointly determined but unequally weighted across
predictors.
