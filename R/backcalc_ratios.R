#' Backcalc Missing Inferential Statistics for Ratios
#'
#' \code{backcalc_ratios()} reconstructs inferential statistics for ratio measures
#' (e.g., odds ratios, risk ratios) using log-transformation internally. It supports
#' reconstructing standard errors, confidence intervals, p-values, and test statistics
#' from partial or varied combinations of summary and inferential statistics.
#'
#' @param ratio Numeric scalar (one-sample) or length-2 vector (two-sample comparison).
#'   If a vector of two values is given, inference is based on the difference in their log-transformed values.
#' @param se Numeric. Standard error on the log scale.
#' @param n Integer. Sample size (used to infer df if not provided).
#' @param df Numeric. Degrees of freedom. Can be a scalar or a vector of length 2 (Welch's approximation).
#' @param p Numeric. p-value.
#' @param ci Numeric vector of length 2. Confidence interval on ratio scale.
#' @param one_sided Logical. Whether the test is one-sided (default is FALSE).
#' @param digits Integer. Number of digits to round results to (default = 3).
#' @param conf.level Numeric. Confidence level for interval (default = 0.95).
#'
#' @return A named numeric vector with the following components:
#' \describe{
#'   \item{Estimate}{Exponentiated estimate (i.e., the ratio on original scale).}
#'   \item{SE}{Standard error of the log-transformed estimate.}
#'   \item{z or t}{Test statistic (either z or t, depending on whether \code{df} is provided).}
#'   \item{df}{Degrees of freedom used for test statistic (if applicable; otherwise \code{NA}).}
#'   \item{p or p-one}{Two-sided or one-sided p-value (depending on \code{one_sided}).}
#'   \item{LL}{Lower limit of the confidence interval on the ratio scale.}
#'   \item{UL}{Upper limit of the confidence interval on the ratio scale.}
#' }
#'
#' @examples
#' # One-sample odds ratio with SE
#' backcalc_ratios(ratio = 2.5, se = 0.2)
#'
#' # Two-sample ratio with SEs and Welch-Satterthwaite df
#' backcalc_ratios(ratio = c(2.5, 1.5), se = c(0.2, 0.15), df = c(50, 40))
#'
#' # Infer SE from CI
#' backcalc_ratios(ratio = 2.5, ci = c(1.8, 3.2))
#'
#' # Using one-sided p-value and 4-digit rounding
#' backcalc_ratios(ratio = 1.9, se = 0.3, one_sided = TRUE, digits = 4)
#'
#' @export
backcalc_ratios <- function(ratio = NULL, se = NULL, n = NULL, df = NULL,
                            p = NULL, ci = NULL, one_sided = FALSE,
                            digits = 3, conf.level = 0.95) {

  if (is.null(ratio)) {
    message("Insufficient information: a ratio (or two ratios) must be provided.")
    return(invisible(NULL))
  }

  approx_notes <- character(0)

  get_crit <- function(df = NULL) {
    alpha <- 1 - conf.level
    if (!is.null(df)) qt(1 - alpha / ifelse(one_sided, 1, 2), df)
    else qnorm(1 - alpha / ifelse(one_sided, 1, 2))
  }

  # Two-ratio case
  if (length(ratio) == 2) {
    estimate <- log(ratio[1]) - log(ratio[2])
    approx_notes <- c(approx_notes, "Estimate calculated as log ratio difference between two ratios.")

    # Combine SEs if both provided
    if (!is.null(se) && length(se) == 2) {
      se_combined <- sqrt(se[1]^2 + se[2]^2)
      se <- se_combined
      approx_notes <- c(approx_notes, "SE combined using sqrt(se1^2 + se2^2).")
    }

    # Welch–Satterthwaite approximation for df
    if (!is.null(df) && length(df) == 2 && !is.null(se) && length(se) == 1) {
      df1 <- df[1]; df2 <- df[2]
      se1 <- se[1]; se2 <- sqrt(se^2 - se1^2)  # Approximate second SE
      df <- (se1^2 + se2^2)^2 / ((se1^4 / df1) + (se2^4 / df2))
      approx_notes <- c(approx_notes, "df approximated using Welch–Satterthwaite formula.")
    }
  } else {
    estimate <- log(ratio)
  }

  # Infer df from n
  if (is.null(df) && !is.null(n)) {
    df <- n - 1
    approx_notes <- c(approx_notes, "df approximated as n - 1.")
  }

  # Infer SE from CI
  if (!is.null(ci) && length(ci) == 2 && is.null(se)) {
    if (all(ci > 0)) {
      crit <- get_crit(df)
      se <- abs(log(ci[2]) - log(ci[1])) / (2 * crit)
      approx_notes <- c(approx_notes, "SE approximated from CI using log scale and critical value.")
    } else {
      message("Confidence interval values must be positive for log transformation. Ignoring CI.")
    }
  }

  if (is.null(se)) {
    message("Insufficient information: SE or CI must be provided.")
    return(invisible(NULL))
  }

  # Statistic and p-value
  statistic_type <- if (!is.null(df)) "t" else "z"
  statistic <- estimate / se

  if (is.null(p)) {
    if (statistic_type == "t") {
      p <- if (one_sided) 1 - pt(statistic, df) else 2 * (1 - pt(abs(statistic), df))
    } else {
      p <- if (one_sided) 1 - pnorm(statistic) else 2 * (1 - pnorm(abs(statistic)))
    }
  }

  # CI on ratio scale
  crit <- get_crit(df)
  ci_lower <- exp(estimate - crit * se)
  ci_upper <- exp(estimate + crit * se)
  estimate_exp <- exp(estimate)

  # Result vector
  result <- c(
    Estimate = round(estimate_exp, digits),
    SE = round(se, digits),
    statistic = round(statistic, digits),
    df = if (!is.null(df)) round(df, 0) else NA,
    p_value = round(p, digits),
    LL = round(ci_lower, digits),
    UL = round(ci_upper, digits)
  )

  # Rename for sidedness and statistic type
  names(result)[names(result) == "statistic"] <- statistic_type
  names(result)[names(result) == "p_value"] <- if (one_sided) "p-one" else "p"

  # Show messages/notes
  if (length(approx_notes)) cat("Note(s):\n", paste(approx_notes, collapse = "\n"), "\n", sep = "")

  return(result)
}
