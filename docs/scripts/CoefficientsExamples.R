# backcalc 
## Coefficients Examples

source("http://raw.githubusercontent.com/cwendorf/backcalc/main/source-backcalc.R")

### Unstandardized Beta Cases

# 1. Basic: coefficient and SE provided, assume z-test
backcalc_coeffs(b = 0.5, se = 0.1)

# 2. Coefficient and p-value given, with df for t-test; infer SE and t
backcalc_coeffs(b = 1.2, p = 0.03, df = 28)

# 3. Estimate and confidence interval given with df; infer SE and t-statistic
backcalc_coeffs(b = 0.8, ci = c(0.2, 1.4), df = 45)

# 4. Estimate, SE, and sample size given; infer df and CI
backcalc_coeffs(b = -0.7, se = 0.2, n = 50)

# 5. Estimate and test statistic provided directly with df; infer SE and p
backcalc_coeffs(b = 0.9, statistic = 2.3, df = 30)

### Standardized Beta and Conversion Cases

# 6. Standardized beta and SE_std given; z-test assumed
backcalc_coeffs(std_beta = 0.25, se_std = 0.04)

# 7. Unstandardized beta and SDs given; infer standardized beta and SE_std
backcalc_coeffs(b = 1.1, se = 0.3, sd_x = 2.5, sd_y = 5)

# 8. Standardized beta, p-value, and df given; infer SE_std and t-statistic
backcalc_coeffs(std_beta = 0.3, p = 0.02, df = 25)

# 9. Provide standardized beta, CI, and sample size; infer SE_std and p
backcalc_coeffs(std_beta = 0.4, ci = c(0.1, 0.7), n = 40)

# 10. Standardized beta and statistic given with df; infer SE_std and p
backcalc_coeffs(std_beta = 0.35, statistic = 2.1, df = 29)

### Insufficient Information Cases

# 11. Only p-value given, no estimate or SE
backcalc_coeffs(p = 0.05)

# 12. Only SE given, no estimate
backcalc_coeffs(se = 0.15)

# 13. Only confidence interval lower bound (invalid length)
backcalc_coeffs(ci = 0.3)

# 14. Only standardized SE given, no standardized beta
backcalc_coeffs(se_std = 0.05)

# 15. Only sample size given, no estimate, SE, or p
backcalc_coeffs(n = 100)
