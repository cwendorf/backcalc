# backcalc 
## Means Examples

source("http://raw.githubusercontent.com/cwendorf/backcalc/main/source-backcalc.R")

### One Sample Cases

# 1. Confidence interval only (m & SE inferred)
backcalc_means(ci = c(2.1, 4.9), digits = 2)

# 2. m + SE (z assumed)
backcalc_means(m = 5.2, se = 1.1, digits = 3)

# 3. m + SE + df (standard t-test)
backcalc_means(m = 3.5, se = 0.8, df = 28, digits = 3)

# 4. m + p-value + df (SE inferred from p)
backcalc_means(m = 2.8, p = 0.023, df = 35, digits = 4)

# 5. CI + df (m and SE inferred)
backcalc_means(ci = c(4.5, 7.3), df = 15, digits = 3)

### Paired and Two Sample Cases

# 6. Paired sample with m, se, and n only (df inferred)
backcalc_means(m = 1.5, se = 0.5, n = 15, paired = TRUE, digits = 3)

# 7. Paired sample with m, p, and n (df inferred, one-sided test)
backcalc_means(m = 1.2, p = 0.03, n = 12, paired = TRUE, one_sided = TRUE, digits = 3)

# 8. m + two-group SDs + equal ns (classic t test)
backcalc_means(m = 1.2, sd = c(10, 11), n = c(30, 30), digits = 3)

# 9. m + two-group SDs + unequal ns (Welch correction auto)
backcalc_means(m = 2.4, sd = c(5, 6), n = c(30, 40), digits = 2)

# 10. m + two-group SDs + df + unequal ns (redundant info, Welch correction)
backcalc_means(m = 4.1, sd = c(5.5, 6.0), n = c(20, 25), df = 43, digits = 4)

### Cases That Do Not Work

# 11. Only p-value (insufficient to infer m or SE)
backcalc_means(p = 0.05)

# 12. m + n only (no SD or SE provided)
backcalc_means(m = 1.7, n = 25)

# 13. m + SD only (sample size missing)
backcalc_means(m = 2.1, sd = 4.5)

# 14. Two-group SDs + two sample sizes (no m provided)
backcalc_means(sd = c(7, 8), n = c(25, 30))
