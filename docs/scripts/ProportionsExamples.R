# backcalc 
## Proportions Examples

source("http://raw.githubusercontent.com/cwendorf/backcalc/main/source-backcalc.R")

### One Sample Cases

# 1. Least inference: Provide proportion and sample size only
backcalc_props(prop = 0.4, n = 100)

# 2. Provide count and sample size, infer proportion and SE
backcalc_props(x = 45, n = 120)

# 3. Provide proportion, sample size, and test statistic (z)
backcalc_props(prop = 0.5, n = 150, statistic = 2.1)

# 4. Provide proportion, sample size, and p-value, infer SE and statistic
backcalc_props(prop = 0.35, n = 80, p = 0.04)

# 5. Provide proportion, t-statistic, and degrees of freedom, infer SE and p
backcalc_props(prop = 0.6, statistic = 2.2, df = 19)

### Two Sample Cases

# 6. Least inference: Provide two proportions and sample sizes only
backcalc_props(prop = c(0.55, 0.4), n = c(150, 130))

# 7. Provide two proportions, sample sizes, and test statistic (z)
backcalc_props(prop = c(0.6, 0.5), n = c(120, 110), statistic = 2.2)

# 8. Provide two proportions, sample sizes, and p-value, infer SE and statistic
backcalc_props(prop = c(0.3, 0.35), n = c(100, 110), p = 0.05)

# 9. Provide counts and sample sizes for two groups, infer difference and SE
backcalc_props(x = c(40, 30), n = c(100, 90))

# 10. Provide two proportions, p-value, and degrees of freedom (t-test)
backcalc_props(prop = c(0.45, 0.5), p = 0.04, df = 50)

### Insufficient Information Cases

# 11. No prop, no counts, no SE — cannot infer
backcalc_props()

# 12. Provide statistic and sample size only, no prop or se — infer prop and SE
backcalc_props(statistic = 2.1, n = 50)

# 13. Provide n only, no prop, counts, or se
backcalc_props(n = 100)

# 14. Provide counts but missing sample size
backcalc_props(x = 30)

# 15. Provide CI but no counts or proportion — cannot compute estimate or SE
backcalc_props(ci = c(0.2, 0.8))
