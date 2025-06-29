# backcalc 
## Proportions Examples

source("http://raw.githubusercontent.com/cwendorf/backcalc/main/source-backcalc.R")

### One Sample Cases

# 1. Basic single-sample (Wald)
backcalc_props(prop = 0.45, n = 100)

# 2. Exact interval with counts, more precision
backcalc_props(x = 45, n = 100, interval_type = "exact", sig_digits = 4)

# 3. Exact interval, one-sided test
backcalc_props(x = 30, n = 80, interval_type = "exact", one_sided = TRUE)

# 4. Given confidence interval (Wald)
backcalc_props(ci = c(0.30, 0.50), n = 80)

# 5. SE and p-value provided (Wald)
backcalc_props(prop = 0.52, se = 0.05, p = 0.03, n = 90)

# 6. One-sided, exact interval, custom rounding
backcalc_props(x = 72, n = 120, one_sided = TRUE, interval_type = "exact", sig_digits = 4)

# 7. Exact interval with explicit x
backcalc_props(x = 20, n = 50, interval_type = "exact", sig_digits = 3)

### Two Sample Cases

# 8. Two-sample proportions, Wald intervals
backcalc_props(prop = c(0.55, 0.40), n = c(150, 130))

# 9. Two-sample proportions, with p-value and rounding
backcalc_props(prop = c(0.25, 0.35), n = c(100, 110), p = 0.04, sig_digits = 4)

# 10. Two-sample proportions with estimates only
backcalc_props(prop = c(0.65, 0.58), n = c(140, 135))

### Cases That Do Not Work

# 11. No inputs at all - nothing to compute
backcalc_props()

# 12. Single proportion but missing sample size 'n' (needed for SE estimation)
backcalc_props(prop = 0.5)

# 13. Two proportions provided but sample sizes missing (needed for two-sample calculation)
backcalc_props(prop = c(0.3, 0.4))

# 14. Exact interval requested without providing counts 'x' and sample size 'n'
backcalc_props(prop = 0.6, interval_type = "exact")
