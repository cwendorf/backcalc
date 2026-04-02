# [`backcalc`](https://github.com/cwendorf/backcalc)

## Reference

This section is the reference for `backcalc`. Each page documents one exported function, including usage, arguments, and return values.

- [Backcalculate Methods](#backcalculate-methods)
- [Print Methods](#print-methods)

------------------------------------------------------------------------

### Backcalculate Methods

- [Backcalculate Missing Inferential Statistics for ANOVA](./backcalc_anova.md): Handles between-subjects, within-subjects, factorial, and mixed designs, with optional sphericity corrections. Works with multiple effects at once.
- [Backcalculate Missing Inferential Statistics for Regression Coefficients](./backcalc_coeffs.md): This function reconstructs inferential statistics for a regression coefficient. It allows for partial input of summary or inferential statistics and infers missing values such as standard errors, test statistics, confidence intervals, p-values, and degrees of freedom. It supports both unstandardized and standardized coefficients.
- [Backcalculate Missing Inferential Statistics for Correlations](./backcalc_corrs.md): This function reconstructs missing inferential statistics for correlation coefficients using Fisher's z-transformation. It supports both one-sample and two-sample correlation comparisons and allows flexible combinations of inputs to infer standard errors, test statistics, p-values, confidence intervals, and degrees of freedom.
- [Backcalculate Missing Inferential Statistics for Means](./backcalc_means.md): This function reconstructs inferential statistics related to means from partial information. It can estimate standard errors, confidence intervals, p-values, test statistics (t or z), degrees of freedom, and point estimates when some components are missing, supporting one-sample, paired, and two-sample cases.
- [Backcalculate Missing Inferential Statistics for Medians](./backcalc_medians.md): This function estimates standard errors, test statistics, p-values, confidence intervals, and degrees of freedom for median-based statistics using summary-level data. It supports robust measures of spread such as interquartile range (IQR), median absolute deviation (MAD), and range, applying normal approximation for inference without relying on parametric assumptions. When parametric inputs (e.g., standard deviations) are provided, t-distribution based inference is used.
- [Backcalculate Missing Inferential Statistics for Proportions](./backcalc_props.md): This function reconstructs inferential statistics related to proportions from partial information. It can estimate standard errors, confidence intervals, p-values, test statistics (t or z), degrees of freedom, and point estimates when some components are missing, supporting one-sample, paired, and two-sample cases.
- [Backcalculate Inferential Statistics for Ratio Measures](./backcalc_ratios.md): This function reconstructs inferential statistics (e.g., SE, test statistic, p-value, confidence interval) for ratio-type measures (such as odds ratios or risk ratios) using the log transformation internally. The function allows flexible input and determines the appropriate test type (z or t) based on the presence of degrees of freedom.
- [Backcalculate Standardized Mean Differences (Effect Sizes)](./backcalc_standardized_means.md): This function reconstructs standardized mean differences and related effect size statistics from various combinations of inputs. It supports Cohen's d, Hedges' g, and Glass's delta, along with confidence intervals and test statistics for one-sample, paired, and two-sample designs.

### Print Methods

- [Knit print method for backcalc objects](./knit_print.backcalc.md): Knit print method for backcalc objects Ensures Notes and Approximations print in Rmd
- [Custom Print Method for backcalc Objects](./print.backcalc.md): This function provides a tailored print method for objects of class backcalc. It optionally displays additional attributes such as notes and approximation messages alongside the main data output.

