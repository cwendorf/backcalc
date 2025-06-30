# backcalc 
## Ratios Examples

source("http://raw.githubusercontent.com/cwendorf/backcalc/main/source-backcalc.R")

### One Sample Cases

# 1. Ratio + SE only (z-test)
backcalc_ratios(ratio = 1.5, se = 0.2, digits = 3)

# 2. Ratio + CI only (z-test)
backcalc_ratios(ratio = 2.0, ci = c(1.4, 2.9), digits = 3)

# 3. Ratio + CI + n (df inferred from n)
backcalc_ratios(ratio = 1.8, ci = c(1.3, 2.4), n = 25, digits = 3)

# 4. Ratio + confidence interval + df
backcalc_ratios(ratio = 1.7, ci = c(1.1, 2.5), df = 18, digits = 3)

# 5. Ratio + SE + df
backcalc_ratios(ratio = 1.4, se = 0.12, df = 20, digits = 3)

### Two Sample Cases

# 6. Two-sample equal df
backcalc_ratios(ratio = c(2.2, 1.5), se = c(0.15, 0.10), df = 20, digits = 3)

# 7. Two-sample unequal df (Welch correction)
backcalc_ratios(ratio = c(2.5, 1.7), se = c(0.18, 0.12), df = c(18, 22), digits = 3)

# 8. Ratio difference + SE + df provided
backcalc_ratios(ratio = 0.25, se = 0.09, df = 40, digits = 3)

# 9. Ratio difference + confidence interval + df
backcalc_ratios(ratio = 0.35, ci = c(0.10, 0.60), df = 38, digits = 3)

# 10. Two-sample with different ratios and SEs, equal df
backcalc_ratios(ratio = c(1.8, 1.2), se = c(0.14, 0.09), df = 24, digits = 3)

### Cases That Do Not Work

# 11. Missing ratio entirely
backcalc_ratios(se = 0.2)

# 12. Ratio provided but no SE or CI to infer variability
backcalc_ratios(ratio = 1.5)

# 13. Two ratios provided but no SE or CI for inference
backcalc_ratios(ratio = c(2.0, 1.5))

# 14. Ratio + p-value provided, but no SE, CI, or df to infer SE or statistic
backcalc_ratios(ratio = 1.8, p = 0.05)
