# backcalc 
## Medians Examples

source("http://raw.githubusercontent.com/cwendorf/backcalc/main/source-backcalc.R")

### One Sample Cases

# 1. Direct median with SE and sample size (assumes t-distribution)
backcalc_medians(m = 25.4, se = 2.1, n = 30)

# 2. Median with IQR and sample size (SE approximated from IQR, assumes normality)
backcalc_medians(m = 25.4, sd = 10, n = 30)

# 3. Median with confidence interval and sample size (SE inferred from CI)
backcalc_medians(m = 30, ci = c(25, 35), n = 25)

# 4. Median with p-value and degrees of freedom (statistic and SE inferred)
backcalc_medians(m = 2.5, p = 0.03, df = 29)

# 5. Median with p-value and sample size (df inferred, SE and statistic approximated)
backcalc_medians(m = 2.1, p = 0.05, n = 16)

### Two Sample Cases

# 6. Two group medians, IQRs, and ns provided (difference computed, SE and df inferred)
backcalc_medians(m = c(15, 12), sd = c(4, 5), n = c(40, 35))

# 7. Median difference and SE provided (CI and p-value inferred)
backcalc_medians(m = c(15, 12), se = 1.5, n = c(40, 35))

# 8. Medians and p-value + df provided (infer SE and statistic)
backcalc_medians(m = c(10, 7), p = 0.04, df = 50)

# 9. Medians and confidence interval provided (infer SE)
backcalc_medians(m = c(100, 90), ci = c(2, 18), n = c(50, 45))

# 10. Medians and IQRs provided, but only n for one group (partial inference possible)
backcalc_medians(m = c(8, 5), sd = c(3, 4), n = 20)

### Insufficient Information Cases

# 11. No median, SE, p, or CI provided (should warn)
backcalc_medians(n = 15)

# 12. Median and IQR provided, but no n (cannot infer SE)
backcalc_medians(m = 7, sd = 2)

# 13. Only p-value and df provided (no median or SE; insufficient)
backcalc_medians(p = 0.05, df = 20)

# 14. Median and df provided, but no SE, p, or CI
backcalc_medians(m = 5.6, df = 10)

# 15. Only test statistic and df provided (no median or SE; insufficient)
backcalc_medians(statistic = 2.5, df = 18)
