# backcalc 
## Means Examples

source("http://raw.githubusercontent.com/cwendorf/backcalc/main/source-backcalc.R")

### One Sample Cases

# 1. Direct estimate with SE and sample size (minimal inference, t-distribution used)
backcalc_means(m = 25.4, se = 2.1, n = 30)

# 2. Estimate with SD and sample size (SE is inferred, t-distribution used)
backcalc_means(m = 25.4, sd = 10, n = 30)

# 3. Estimate with confidence interval and sample size (SE is inferred from CI)
backcalc_means(m = 30, ci = c(25, 35), n = 25)

# 4. Estimate with p-value and degrees of freedom (SE and test statistic inferred)
backcalc_means(m = 2.5, p = 0.03, df = 29)

# 5. Estimate with no SE, but p-value and sample size given (df is inferred, t-statistic and SE inferred)
backcalc_means(m = 2.1, p = 0.05, n = 16)

### Two Sample Cases

# 6. Means, SDs, and ns provided (calculate difference, SE, df)
backcalc_means(m = c(15, 12), sd = c(4, 5), n = c(40, 35))

# 7. Difference of means and SE provided
backcalc_means(m = c(15, 12), se = 1.5, n = c(40, 35))

# 8. Means and p-value + df provided (infer SE and statistic)
backcalc_means(m = c(10, 7), p = 0.04, df = 50)

# 9. Means and confidence interval provided (infer SE, df)
backcalc_means(m = c(100, 90), ci = c(2, 18), n = c(50, 45))

# 10. Means and SDs provided, but only n for one group (more complex inference)
backcalc_means(m = c(8, 5), sd = c(3, 4), n = 20)

### Insufficient Information Cases

# 11. No mean, SE, p, or CI provided
backcalc_means(n = 15)

# 12. Mean provided with SD but no n or SE
backcalc_means(m = 7, sd = 2)

# 13. Only p-value and df provided (no estimate or SE)
backcalc_means(p = 0.05, df = 20)

# 14. Mean and df provided, but no SE, p, or CI
backcalc_means(m = 5.6, df = 10)

# 15. Only statistic and df provided (no estimate or SE)
backcalc_means(statistic = 2.5, df = 18)
