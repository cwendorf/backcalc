#' Backcalc Missing Inferential Statistics for Regression Coefficients
#'
#' \code{backcalc_coeffs()} reconstructs inferential statistics for a regression coefficient.
#' It supports unstandardized or standardized coefficients, inferring standard errors,
#' confidence intervals, test statistics, and p-values from partial or 
#' varied combinations of summary and inferential statistics.
#'
#' @param b Numeric. Unstandardized regression coefficient.
#' @param se Numeric. Standard error of the unstandardized coefficient.
#' @param std_beta Numeric. Standardized regression coefficient.
#' @param se_std Numeric. Standard error of the standardized coefficient.
#' @param sd_x, sd_y Numeric. Standard deviations of predictor and outcome (used to infer standardized beta).
#' @param df Numeric. Degrees of freedom for t-distribution. If omitted, assumes normal distribution (z-test).
#' @param p Numeric. p-value for the coefficient.
#' @param ci Numeric vector of length 2. Confidence interval for the coefficient.
#' @param one_sided Logical. Use one-sided test? Default is FALSE.
#' @param conf.level Numeric between 0 and 1. Confidence level for confidence intervals. Default is 0.95.
#' @param sig_digits Integer. Digits to round the output. Default is 3.
#'
#' @return Named numeric vector with:
#' \describe{
#'   \item{coeff}{Coefficient (standardized or unstandardized)}
#'   \item{se}{Standard error}
#'   \item{df}{Degrees of freedom, if provided; otherwise NA}
#'   \item{ci_ll, ci_ul}{Confidence interval lower and upper bounds, based on \code{conf.level}}
#'   \item{t / z}{Test statistic (t if \code{df} provided, otherwise z)}
#'   \item{p / p_one}{Two-sided or one-sided p-value (named \code{p_one} if \code{one_sided = TRUE})}
#' }
#'
#' @examples
#' # Basic z-test with coefficient and SE; default 95% CI and 3 digits
#' backcalc_coeffs(b = 0.5, se = 0.1)
#' # t-test from coefficient and p-value with df; default 95% CI
#' backcalc_coeffs(b = 1.2, p = 0.03, df = 28)
#' # Estimate and confidence interval given; infer SE and t-statistic with df and 4 digits rounding
#' backcalc_coeffs(b = 0.8, ci = c(0.2, 1.4), df = 45, sig_digits = 4)
#' # Standardized beta and SE only; uses z-test by default with 99% CI
#' backcalc_coeffs(std_beta = 0.25, se_std = 0.04, conf.level = 0.99)
#' # Use all inputs including one-sided test and custom rounding digits
#' backcalc_coeffs(b = 1.1, se = 0.3, sd_x = 2.5, sd_y = 5,
#'                      p = 0.04, df = 30, one_sided = TRUE, sig_digits = 4)
#' # One-sided t-test with coefficient, p-value, df, and 90% CI
#' backcalc_coeffs(b = -0.7, p = 0.01, df = 20, one_sided = TRUE, conf.level = 0.90)
#'
#' @export
backcalc_coeffs <- function(b = NULL, se = NULL,
                            std_beta = NULL, se_std = NULL,
                            sd_x = NULL, sd_y = NULL,
                            df = NULL,
                            p = NULL,
                            ci = NULL,
                            one_sided = FALSE, 
                            conf.level = 0.95,
                            sig_digits = 3) {
  messages <- character(0)
  approx_notes <- character(0)

  # Helper: critical value based on df and conf.level
  get_crit <- function(df = NULL) {
    alpha <- 1 - conf.level
    if (one_sided) {
      if (!is.null(df)) qt(1 - alpha, df) else qnorm(1 - alpha)
    } else {
      if (!is.null(df)) qt(1 - alpha / 2, df) else qnorm(1 - alpha / 2)
    }
  }

  # Choose estimate and SE
  estimate <- if (!is.null(std_beta)) std_beta else b
  se_val <- if (!is.null(std_beta)) se_std else se

  # Infer standardized beta
  if (is.null(std_beta) && !is.null(b) && !is.null(sd_x) && !is.null(sd_y)) {
    estimate <- b * (sd_x / sd_y)
    approx_notes <- c(approx_notes, "Standardized beta approximated from unstandardized beta and standard deviations.")
    if (!is.null(se)) {
      se_val <- se * (sd_x / sd_y)
      approx_notes <- c(approx_notes, "SE of standardized beta approximated from unstandardized SE and SDs.")
    }
  }

  # Infer from CI
  if (!is.null(ci)) {
    if (length(ci) != 2) {
      messages <- c(messages, "CI must be a numeric vector of length 2.")
    } else {
      if (is.null(estimate)) {
        estimate <- mean(ci)
        approx_notes <- c(approx_notes, "Estimate approximated as midpoint of CI.")
      }
      if (is.null(se_val)) {
        crit <- get_crit(df)
        se_val <- abs(ci[2] - ci[1]) / (2 * crit)
        approx_notes <- c(approx_notes, "SE approximated from CI width.")
      }
    }
  }

  # Determine statistic type
  stat_type <- if (!is.null(df)) "t" else "z"

  # Compute statistic
  stat <- if (!is.null(estimate) && !is.null(se_val)) {
    estimate / se_val
  } else if (!is.null(p) && !is.null(estimate)) {
    crit_val <- if (one_sided) {
      if (!is.null(df)) qt(1 - p, df) else qnorm(1 - p)
    } else {
      if (!is.null(df)) qt(1 - p / 2, df) else qnorm(1 - p / 2)
    }
    approx_notes <- c(approx_notes, "Test statistic approximated from p-value and estimate.")
    stat <- sign(estimate) * crit_val
    se_val <- estimate / stat
    approx_notes <- c(approx_notes, "SE approximated from estimate and reconstructed statistic.")
    stat
  } else {
    NA
  }

  # Compute p if missing
  if (is.null(p) && !is.na(stat)) {
    p <- if (stat_type == "t") {
      if (one_sided) 1 - pt(stat, df) else 2 * (1 - pt(abs(stat), df))
    } else {
      if (one_sided) 1 - pnorm(stat) else 2 * (1 - pnorm(abs(stat)))
    }
  }

  # Compute CI if possible
  if (!is.null(estimate) && !is.null(se_val)) {
    crit <- get_crit(df)
    ci_lower <- estimate - crit * se_val
    ci_upper <- estimate + crit * se_val
  } else {
    ci_lower <- NA
    ci_upper <- NA
  }

  # Assemble result
  result <- c(
    coeff = round(estimate, sig_digits),
    se = round(se_val, sig_digits),
    df = if (!is.null(df)) round(df, 0) else NA,
    ci_ll = round(ci_lower, sig_digits),
    ci_ul = round(ci_upper, sig_digits)
  )

  stat_name <- stat_type
  result <- c(result, setNames(round(stat, sig_digits), stat_name))

  p_name <- if (one_sided) "p_one" else "p"
  result <- c(result, setNames(round(p, sig_digits), p_name))

  # Show notes
  if (length(messages)) cat(paste(messages, collapse = "\n"), "\n")
  if (length(approx_notes)) cat("Note(s):\n", paste(approx_notes, collapse = "\n"), "\n")

  return(result)
}
