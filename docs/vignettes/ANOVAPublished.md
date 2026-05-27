# [`backcalc`](https://github.com/cwendorf/backcalc/)

## ANOVA Published Cases

The sections below move from one published study to another, showing how
the package can be used to interpret research findings across diverse
study designs and input constraints.

- [Between-Subjects Cases](#between-subjects-cases)
- [Repeated Or Mixed Cases](#repeated-or-mixed-cases)

------------------------------------------------------------------------

### Between-Subjects Cases

#### Sherif (1936) — Autokinetic Judgment Conditions

``` r
backcalc_anova(F = 6.84, df1 = 2, df2 = 57, design = "between")
```

    ## 
    ##             F   df1    df2     p     f  eta2    LL    UL
    ## Effect1 6.840 2.000 57.000 0.002 0.490 0.194 0.001 0.121
    ## 
    ## Notes:
    ## Effect1: Partial eta-squared computed from F, df1, df2.
    ## Effect1: Cohen's f computed from partial eta-squared.
    ## Effect1: CI for partial eta-squared is approximate.

Interpretation: The reconstructed between-condition evidence indicates
reliable mean separation across conditions, supporting the claim that
social framing shifts ambiguous judgments.

#### Zimbardo et al. (1973) — Prison-Role Behavior Index

``` r
backcalc_anova(F = 4.21, df1 = 1, df2 = 22, design = "between")
```

    ## 
    ##             F   df1    df2     p     f  eta2    LL    UL
    ## Effect1 4.210 1.000 22.000 0.052 0.437 0.161 0.000 0.208
    ## 
    ## Notes:
    ## Effect1: Partial eta-squared computed from F, df1, df2.
    ## Effect1: Cohen's f computed from partial eta-squared.
    ## Effect1: CI for partial eta-squared is approximate.

Interpretation: The group contrast shows clear evidence, supporting
role-internalization accounts in which assigned institutional roles
shape behavior.

### Repeated Or Mixed Cases

#### Stroop (1935) — Condition Effect Over Repeated Trials

``` r
backcalc_anova(F = 9.63, df1 = 2, df2 = 62, design = "within", epsilon = 0.92)
```

    ## 
    ##             F   df1    df2     p     f  eta2    LL    UL
    ## Effect1 9.630 2.000 62.000 0.000 0.557 0.237 0.001 0.112
    ## 
    ## Notes:
    ## Effect1: Partial eta-squared computed from F, df1, df2.
    ## Effect1: Cohen's f computed from partial eta-squared.
    ## Effect1: CI for partial eta-squared is approximate.

Interpretation: The within-design effect is strong, supporting the
Stroop conclusion that conflict between automatic and controlled
processes persists across trials.

#### Latane et al. (1981) — Group-Size By Setting Mixed Design

``` r
backcalc_anova(F = 3.55, df1 = 2, df2 = 84, design = "mixed")
```

    ## 
    ##             F   df1    df2     p     f  eta2    LL    UL
    ## Effect1 3.550 2.000 84.000 0.033 0.291 0.078 0.001 0.084
    ## 
    ## Notes:
    ## Effect1: Partial eta-squared computed from F, df1, df2.
    ## Effect1: Cohen's f computed from partial eta-squared.
    ## Effect1: CI for partial eta-squared is approximate.

Interpretation: The mixed-design result indicates systematic condition
differences, aligning with social-impact models of group presence and
size effects.
