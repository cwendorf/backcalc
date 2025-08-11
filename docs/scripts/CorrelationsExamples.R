# backcalc
## Correlations Examples

source("http://raw.githubusercontent.com/cwendorf/backcalc/main/source-backcalc.R")

### One Sample Cases

# 1. All info given: r, se, n (df inferred)
backcalc_corrs(r = 0.45, se = 0.1, n = 25)

# 2. r + p-value + df (infer se, CI)
backcalc_corrs(r = 0.52, p = 0.02, df = 18)

# 3. r + confidence interval + df (infer se, p)
backcalc_corrs(r = 0.35, ci = c(0.10, 0.55), df = 20)

# 4. r + test statistic + df (infer se, p)
backcalc_corrs(r = 0.38, statistic = 2.2, df = 28)

# 5. Only r + n (infer se, df, p, CI) with 90% confidence level
backcalc_corrs(r = 0.42, n = 40, conf.level = 0.90)

### Two Sample Cases

# 6. Two correlations + equal n (infer difference, se, df, p, CI)
backcalc_corrs(r = c(0.60, 0.40), n = c(30, 30))

# 7. Two correlations + unequal n (infer difference, se, df, p, CI)
backcalc_corrs(r = c(0.70, 0.50), n = c(40, 25))

# 8. Difference in correlation + p-value + df (infer se, statistic, CI)
backcalc_corrs(r = 0.18, p = 0.03, df = 45)

# 9. Two correlations + se + equal n (infer df, p, statistic)
backcalc_corrs(r = c(0.52, 0.34), se = 0.12, n = c(35, 35))

# 10. Two correlations + CI + unequal n (infer se, df, p, statistic)
backcalc_corrs(r = c(0.65, 0.48), ci = c(0.50, 0.70), n = c(50, 40))

### Insufficient Information Cases

# 11. Missing correlation coefficient r
backcalc_corrs(se = 0.1, n = 20)

# 12. Invalid correlation coefficient (>1)
backcalc_corrs(r = 1.2, n = 20)

# 13. Two correlations but missing sample sizes
backcalc_corrs(r = c(0.3, 0.5))

# 14. Only r provided, no se, n, p, df, ci or statistic
backcalc_corrs(r = 0.4)

# 15. Invalid confidence interval length (not length 2)
backcalc_corrs(r = 0.3, ci = c(0.1, 0.2, 0.3), n = 20)
