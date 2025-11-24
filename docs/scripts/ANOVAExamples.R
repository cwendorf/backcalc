# backcalc
## ANOVA Examples

### Full ANOVA Cases

# 1. One-way between-subjects ANOVA (single factor)
backcalc_anova(
  F = 5.2,
  df1 = 2,
  df2 = 30,
  design = "between",
  levels = list(3),  # 3 levels for the single factor
  n = 33,
  labels = "Treatment"
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

# 3. Factorial between-subjects ANOVA (2×3): two mains + interaction
backcalc_anova(
  F = c(6.4, 3.2, 2.5),
  df1 = NA, df2 = NA,
  design = "between",
  levels = list(c(2, 3), c(2, 3), c(2, 3)),
  n = 36,
  effect = c("main", "main", "interaction"),
  labels = c("FactorA", "FactorB", "A×B")
)

# 4. Mixed design (Between × Within): between main, within main, interaction
backcalc_anova(
  F = c(5.5, 4.2, 1.8),
  df1 = NA, df2 = NA,
  design = c("between", "within", "mixed"),
  levels = list(c(2), c(3), c(2, 3)),
  n = 20,               # total sample size between groups
  subjects = 10,        # subjects per cell for within factor
  epsilon = c(1, 0.85, 0.85),
  effect = c("main", "main", "interaction"),
  labels = c("Group", "Time", "Group×Time")
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
