# backcalc
## Ratios Examples

source("http://raw.githubusercontent.com/cwendorf/backcalc/main/source-backcalc.R")

### One Sample Cases

# 1. Ratio and SE provided (no inference needed)
backcalc_ratios(ratio = 2.5, se = 0.2)

# 2. Ratio and confidence interval provided (infer SE from CI)
backcalc_ratios(ratio = 1.8, ci = c(1.3, 2.5))

# 3. Ratio and test statistic provided (infer SE from statistic)
backcalc_ratios(ratio = 3.0, statistic = 4.0)

# 4. Ratio, test statistic, and df provided (infer SE and p)
backcalc_ratios(ratio = 2.2, statistic = 2.5, df = 29)

# 5. Ratio, test statistic, and sample size (infer SE, df, p, and CI)
backcalc_ratios(ratio = 1.5, statistic = 2.1, n = 40)

### Two Sample Cases

# 6. Two ratios and SEs provided (no inference needed)
backcalc_ratios(ratio = c(2.5, 1.8), se = c(0.2, 0.1))

# 7. Two ratios, SEs, and dfs provided (Welch-Satterthwaite df calculation)
backcalc_ratios(ratio = c(3.2, 1.9), se = c(0.25, 0.15), df = c(25, 30))

# 8. Two ratios, SE for first group, and CI for the difference (infer second SE and test stats)
backcalc_ratios(ratio = c(2.0, 1.5), se = 0.12, ci = c(1.2, 3.0))

# 9. Two ratios, SE for first group, test statistic and df provided (infer missing SE and p)
backcalc_ratios(ratio = c(2.0, 1.5), se = 0.15, statistic = 3.0, df = 40)

# 10. Two ratios and CI provided (infer SE and all inferential stats)
backcalc_ratios(ratio = c(2.1, 1.7), ci = c(1.05, 3.1))

### Insufficient Information Cases

# 11. No ratio provided (immediate insufficient info)
backcalc_ratios()

# 12. Ratio provided but no SE, statistic, or CI (insufficient SE)
backcalc_ratios(ratio = 2.3)

# 13. Two-sample ratio vector with SE missing (insufficient info, triggers message)
backcalc_ratios(ratio = c(2.5, 1.9))

# 14. Ratio and p-value provided but no SE, statistic, or CI (insufficient SE)
backcalc_ratios(ratio = 1.8, p = 0.05)

# 15. Ratio with zero or negative value (invalid for log transform, insufficient info)
backcalc_ratios(ratio = c(-1.5, 2.0))
