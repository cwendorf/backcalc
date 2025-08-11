# backcalc
## Medians Examples

source("http://raw.githubusercontent.com/cwendorf/backcalc/main/source-backcalc.R")

### One Sample Cases

# 1.  Median, IQR, and sample size
backcalc_medians(m = 50, iqr = 20, n = 30)

# 2.  Median, MAD, and sample size
backcalc_medians(m = 48, mad = 12, n = 25)

# 3.  Median, IQR, paired design
backcalc_medians(m = 42, iqr = 15, n = 20, paired = TRUE)

# 4.  Median, MAD, one-sided test
backcalc_medians(m = 55, mad = 10, n = 40, one_sided = TRUE)

# 5. median + MAD + n, no paired, no one-sided
backcalc_medians(m = 55, range = 30, n = 20)

### Two Sample Cases

# 6.  Group medians, IQR, and sample size
backcalc_medians(m = c(60, 50), iqr = 20, n = 30)

# 7.  Group medians, MAD, and sample size
backcalc_medians(m = c(75, 68), mad = 9, n = 40)

# 8.  Paired medians, IQR, and n
backcalc_medians(m = c(45, 40), iqr = 10, n = 20, paired = TRUE)

# 9.  Group medians, IQR, n, one-sided
backcalc_medians(m = c(82, 78), iqr = 12, n = 35, one_sided = TRUE)

# 10. two medians, two MADs, two sample sizes (no IQR used here)
backcalc_medians(m = c(92, 85), mad = c(7, 10), n = c(25, 28))

### Insufficient Information Cases

# 11. Only IQR provided
backcalc_medians(iqr = 10)

# 12. Only MAD provided
backcalc_medians(mad = 5)

# 13. Only sample size provided
backcalc_medians(n = 30)

# 14. Median provided, no n or spread
backcalc_medians(m = 50)

# 15. Medians and sample size, but no dispersion
backcalc_medians(m = c(52, 49), n = 30)
