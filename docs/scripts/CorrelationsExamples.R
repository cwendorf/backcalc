# backcalc 
## Correlations Examples

source("http://raw.githubusercontent.com/cwendorf/backcalc/main/source-backcalc.R")

### One Sample Cases

# 1. r + SE + n (df inferred)
backcalc_corrs(r = 0.45, se = 0.1, n = 25, sig_digits = 3)

# 2. r + p-value + df
backcalc_corrs(r = 0.52, p = 0.02, df = 18, sig_digits = 3)

# 3. r + confidence interval + df
backcalc_corrs(r = 0.35, ci = c(0.10, 0.55), df = 20, sig_digits = 3)

# 4. r + SE only (z-test)
backcalc_corrs(r = 0.3, se = 0.08, sig_digits = 3)

# 5. r + p-value + n (df inferred)
backcalc_corrs(r = 0.42, p = 0.04, n = 22, sig_digits = 3)

### Two Sample Cases

# 6. Two rs, equal n
backcalc_corrs(r = c(0.60, 0.40), n = c(30, 30), sig_digits = 3)

# 7. Two rs, unequal n
backcalc_corrs(r = c(0.70, 0.50), n = c(40, 25), sig_digits = 3)

# 8. r difference + SE + df
backcalc_corrs(r = 0.15, se = 0.07, df = 50, sig_digits = 3)

# 9. r difference + p-value + df
backcalc_corrs(r = 0.18, p = 0.03, df = 45, sig_digits = 3)

# 10. r difference + CI + df
backcalc_corrs(r = 0.20, ci = c(0.05, 0.35), df = 40, sig_digits = 3)

### Cases That Do Not Work

# 11. No correlation coefficient (r) provided
backcalc_corrs(se = 0.1, n = 30)

# 12. Correlation coefficient outside valid range (-1 to 1)
backcalc_corrs(r = 1.2, n = 30)

# 13. Two correlations provided but sample sizes missing or incomplete
backcalc_corrs(r = c(0.5, 0.3))

# 14. One correlation but no SE, no sample size, and no CI provided
backcalc_corrs(r = 0.4)
