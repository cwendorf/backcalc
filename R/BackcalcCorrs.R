backcalc_corrs <- function(r = NULL, se = NULL, n = NULL, df = NULL,
                           p = NULL, ci = NULL, one_sided = FALSE, sig_digits = 3) {
  if (is.null(r)) stop("Correlation coefficient (r) must be provided.")
  if (any(abs(r) > 1)) stop("Correlation coefficients must be between -1 and 1.")

  estimate <- r

  # Handle two-sample case
  if (length(estimate) == 2) {
    if (is.null(n) || length(n) != 2) stop("For comparing two correlations, supply sample sizes for both groups.")
    z1 <- atanh(estimate[1])
    z2 <- atanh(estimate[2])
    estimate <- z1 - z2
    se <- sqrt(1 / (n[1] - 3) + 1 / (n[2] - 3))
    df <- min(n) - 3  # Conservative estimate
  } else {
    # One-sample case
    if (is.null(se)) {
      if (!is.null(n)) {
        se <- 1 / sqrt(n - 3)
        df <- if (is.null(df)) n - 2 else df
      } else if (!is.null(ci)) {
        if (length(ci) != 2) stop("Confidence interval must be a numeric vector of length 2.")
        z_ci <- atanh(ci)
        z_r <- atanh(estimate)
        crit <- if (is.null(df)) qnorm(1 - 0.05 / (if (one_sided) 1 else 2)) else qt(1 - 0.05 / (if (one_sided) 1 else 2), df)
        se <- (max(z_ci) - min(z_ci)) / (2 * crit)
      } else if (!is.null(p) && !is.null(df)) {
        # Infer se from p-value and df
        stat <- if (one_sided) {
          qt(1 - p, df)
        } else {
          qt(1 - p / 2, df)
        }
        estimate <- atanh(r)
        se <- estimate / stat
      } else {
        stop("Insufficient information: provide se, n, ci, or p with df.")
      }
    } else {
      if (is.null(df) && !is.null(n)) {
        df <- n - 2
      }
    }

    estimate <- atanh(r)  # Convert to Fisher z
  }

  # Compute test statistic
  stat <- estimate / se
  stat_type <- if (!is.null(df)) "t" else "z"

  # Compute p-value if missing
  if (is.null(p)) {
    if (stat_type == "t") {
      p <- if (one_sided) 1 - pt(stat, df) else 2 * (1 - pt(abs(stat), df))
    } else {
      p <- if (one_sided) 1 - pnorm(stat) else 2 * (1 - pnorm(abs(stat)))
    }
  }

  # Compute confidence interval in raw correlation scale
  crit_val <- if (!is.null(df)) qt(1 - 0.05 / (if (one_sided) 1 else 2), df) else qnorm(1 - 0.05 / (if (one_sided) 1 else 2))
  ci_z <- c(estimate - crit_val * se, estimate + crit_val * se)
  ci_r <- tanh(ci_z)

  # Convert estimate back to r scale
  final_r <- if (length(r) == 2) tanh(estimate) else r

  # Assemble results
  result <- c(
    r = round(final_r, sig_digits),
    se = round(se, sig_digits),
    df = if (!is.null(df)) round(df, 0) else NA,
    ci_ll = round(ci_r[1], sig_digits),
    ci_ul = round(ci_r[2], sig_digits),
    statistic = round(stat, sig_digits),
    p_value = round(p, sig_digits)
  )

  names(result)[which(names(result) == "statistic")] <- stat_type
  names(result)[which(names(result) == "p_value")] <- ifelse(one_sided, "p-one", "p")

  return(result)
}
