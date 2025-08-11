# backcalc
## ANOVA Examples

### Full ANOVA Cases

# 1. One-way between-subjects ANOVA
backcalc_anova(
  F = c(5.2, NA),         # First effect has F, second only p
  df1 = c(2, NA),
  df2 = c(30, NA),
  p = c(NA, 0.01),
  design = "between",
  levels = list(c(3), c(4)),  # 3 levels, 4 levels
  n = 33
)

# 2. Repeated-measures ANOVA (single factor) with sphericity correction
backcalc_anova(
  F = 4.8,
  df1 = NA, df2 = NA,
  design = "within",
  levels = list(c(4)),
  subjects = 12,
  epsilon = 0.75
)

# 3. Factorial between-subjects ANOVA (2×3)
backcalc_anova(
  F = c(6.4, 3.2, 2.5),
  df1 = NA, df2 = NA,
  design = "between",
  levels = list(c(2, 3), c(2, 3), c(2, 3)),
  n = 36,
  effect = c("main", "main", "interaction")
)

# 4. Mixed design (Between × Within)
backcalc_anova(
  F = c(5.5, 4.2, 1.8),
  df1 = NA, df2 = NA,
  design = c("between", "within", "mixed"),
  levels = list(c(2), c(3), c(2, 3)),
  n = 20,               # per between group
  subjects = 10,        # per condition for within
  epsilon = c(1, 0.85, 0.85),
  effect = c("main", "main", "interaction")
)

### Minimal Input Cases

# 5. Only η²p and df given, reconstruct F and p
backcalc_anova(
  eta2 = 0.15,
  df1 = 2,
  df2 = 30
)

# 6. Only p and dfs given, reconstruct F and η²p
backcalc_anova(
  p = 0.012,
  df1 = 1,
  df2 = 28
)

# 7. Only F and dfs given, calculate p and η²p
backcalc_anova(
  F = 4.5,
  df1 = 1,
  df2 = 22
)
