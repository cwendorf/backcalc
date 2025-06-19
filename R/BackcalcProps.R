#' Backcalc Missing Inferential Statistics for Proportions and Percentages
#'
#' \code{backcalc_correlations()} reconstructs inferential statistics for proportions and 
#' percentages using log-odds scales as appropriate. It supports reconstructing standard errors, 
#' Wald and exact confidence intervals, p-values, and test statistics partial or 
#' varied combinations of summary and inferential statistics.
#'
#' @param prop Numeric scalar or vector of length 2. Estimated proportion(s) (between 0 and 1,
#'   or as percentages 0-100). If two values are provided with corresponding sample sizes, the function
#'   calculates the log-odds difference.
#' @param se Numeric scalar or vector of length 1 or 2. Standard error(s) on the logit scale.
#' @param n Integer scalar or vector of length 2. Sample size(s) corresponding to estimate(s).
#' @param x Integer scalar or vector of length 1 or 2. Number of successes for exact interval calculation.
#' @param ci Numeric vector of length 2. Confidence interval bounds for the proportion (between 0 and 1).
#' @param p Numeric scalar. P-value for the test statistic.
#' @param one_sided Logical. If \code{TRUE}, calculate one-sided p-value and confidence interval.
#'   Default is \code{FALSE}.
#' @param sig_digits Integer. Number of significant digits to round output to. Default is 3.
#' @param interval_type Character string specifying type of confidence interval:
#'   \code{"wald"} (default) for Wald intervals on the logit scale,
#'   or \code{"exact"} for Clopper-Pearson exact intervals (single sample only).
#'
#' @return Named numeric vector containing reconstructed inferential statistics, including:
#' \itemize{
#'   \item \code{logit_p} or \code{log_odds_ratio}: estimate on the logit or log-odds scale
#'   \item \code{se_logit_p} or \code{se_log_odds_ratio}: standard error on logit or log-odds scale
#'   \item \code{proportion} or \code{odds_ratio}: estimate back-transformed to proportion or odds ratio
#'   \item \code{ci_ll}, \code{ci_ul}: confidence interval bounds (on proportion or odds ratio scale)
#'   \item \code{z}: test statistic value (z-score)
#'   \item \code{p} or \code{p_one}: two-sided or one-sided p-value
#' }
#'
#' @examples
#' # Single-sample proportion, basic (Wald interval, default rounding)
#' backcalc_props(prop = 0.45, n = 100)
#' # Single-sample with exact interval, counts provided, increased precision
#' backcalc_props(x = 45, n = 100, interval_type = "exact", sig_digits = 4)
#' # Single-sample with exact interval and one-sided test
#' backcalc_props(x = 30, n = 80, interval_type = "exact", one_sided = TRUE)
#' # Two-sample proportions with sample sizes, Wald intervals (exact intervals not supported here)
#' backcalc_props(prop = c(0.55, 0.40), n = c(150, 130))
#' # Two-sample proportions with provided p-value and adjusted rounding
#' backcalc_props(prop = c(0.25, 0.35), n = c(100, 110), p = 0.04, sig_digits = 4)
#' # One-sided test for a single proportion with custom rounding and exact interval
#' backcalc_props(x = 72, n = 120, one_sided = TRUE, interval_type = "exact", sig_digits = 4)
#'
#' @export
backcalc_props <- function(prop = NULL, se = NULL, n = NULL, x = NULL, ci = NULL,
                           p = NULL, one_sided = FALSE, sig_digits = 3,
                           interval_type = c("wald", "exact")) {
  interval_type <- match.arg(interval_type)
  messages <- character(0)
  approx_notes <- character(0)

  # Helper functions
  logit <- function(p) log(p / (1 - p))
  inv_logit <- function(l) exp(l) / (1 + exp(l))
  get_crit <- function() {
    alpha <- if (one_sided) 0.10 else 0.05
    qnorm(1 - alpha / ifelse(one_sided, 1, 2))
  }
  exact_ci <- function(x, n, conf.level = 0.95) {
    alpha <- 1 - conf.level
    lower <- if (x == 0) 0 else qbeta(alpha / 2, x, n - x + 1)
    upper <- if (x == n) 1 else qbeta(1 - alpha / 2, x + 1, n - x)
    c(lower, upper)
  }

  # Sanitize input
  estimate <- prop
  if (!is.null(estimate) && any(estimate > 1)) {
    estimate <- estimate / 100
    approx_notes <- c(approx_notes, "Input proportions >1 assumed to be percentages and converted to proportions.")
  }

  # Compute estimate from x/n
  if (is.null(estimate) && !is.null(x) && !is.null(n)) {
    estimate <- x / n
    approx_notes <- c(approx_notes, "Proportion estimated from x/n.")
  }

  # Detect two-sample case
  two_sample_case <- !is.null(estimate) && length(estimate) == 2 && !is.null(n) && length(n) == 2

  # SE estimation (single sample)
  if (!two_sample_case && is.null(se) && !is.null(estimate) && !is.null(n)) {
    se <- sqrt(1 / (n * estimate) + 1 / (n * (1 - estimate)))
    approx_notes <- c(approx_notes, "SE estimated from proportion and sample size using Wald approximation on logit scale.")
  }

  # Two-sample logit difference
  if (two_sample_case) {
    p1 <- estimate[1]
    p2 <- estimate[2]
    n1 <- n[1]
    n2 <- n[2]

    l1 <- logit(p1)
    l2 <- logit(p2)
    estimate <- l1 - l2
    se1 <- sqrt(1 / (n1 * p1) + 1 / (n1 * (1 - p1)))
    se2 <- sqrt(1 / (n2 * p2) + 1 / (n2 * (1 - p2)))
    se <- sqrt(se1^2 + se2^2)

    approx_notes <- c(approx_notes, "Two-sample log-odds ratio and SE computed from proportions and sample sizes.")
  }

  # Back-calculate from CI
  if (!is.null(ci)) {
    if (length(ci) == 2 && all(ci >= 0 & ci <= 1)) {
      ci_logit <- logit(ci)
      if (is.null(estimate)) {
        estimate <- mean(ci_logit)
        approx_notes <- c(approx_notes, "Estimate approximated as midpoint of logit-transformed CI.")
      }
      if (is.null(se)) {
        se <- abs(ci_logit[2] - ci_logit[1]) / (2 * get_crit())
        approx_notes <- c(approx_notes, "SE approximated from width of logit-transformed CI.")
      }
    } else {
      messages <- c(messages, "CI must be length 2 and within [0, 1]. Ignoring CI.")
    }
  }

  # Statistic from estimate & SE
  if (!is.null(estimate) && !is.null(se)) {
    statistic <- estimate / se
  } else if (!is.null(p) && !is.null(estimate)) {
    crit_val <- if (one_sided) qnorm(1 - p) else qnorm(1 - p / 2)
    statistic <- sign(estimate) * crit_val
    se <- estimate / statistic
    approx_notes <- c(approx_notes, "SE approximated from estimate and p-value.")
  } else {
    statistic <- NA
  }

  # Compute p-value if missing
  if (is.null(p) && !is.na(statistic)) {
    p <- if (one_sided) 1 - pnorm(statistic) else 2 * (1 - pnorm(abs(statistic)))
  }

  crit <- get_crit()

  # Compute CI
  if (interval_type == "wald" && !is.null(estimate) && !is.null(se)) {
    if (two_sample_case) {
      ci_lower <- exp(estimate - crit * se)
      ci_upper <- exp(estimate + crit * se)
      estimate_exp <- exp(estimate)
    } else {
      ci_lower <- inv_logit(estimate - crit * se)
      ci_upper <- inv_logit(estimate + crit * se)
      estimate_exp <- inv_logit(estimate)
    }
  } else if (interval_type == "exact" && !two_sample_case) {
    if (!is.null(x) && !is.null(n)) {
      bounds <- exact_ci(x, n, conf.level = ifelse(one_sided, 0.90, 0.95))
      ci_lower <- bounds[1]
      ci_upper <- bounds[2]
      estimate_exp <- estimate
      approx_notes <- c(approx_notes, "Exact CI calculated using Clopper-Pearson method.")
    } else {
      messages <- c(messages, "Exact interval requested but x or n missing. CI not calculated.")
      ci_lower <- ci_upper <- NA
      estimate_exp <- if (!is.null(estimate)) inv_logit(estimate) else NA
    }
  } else {
    ci_lower <- ci_upper <- NA
    estimate_exp <- if (!is.null(estimate)) inv_logit(estimate) else NA
  }

  # Compile result
  if (two_sample_case) {
    result <- c(
      log_odds_ratio = round(estimate, sig_digits),
      se_log_odds_ratio = round(se, sig_digits),
      odds_ratio = round(estimate_exp, sig_digits),
      ci_ll = round(ci_lower, sig_digits),
      ci_ul = round(ci_upper, sig_digits),
      z = round(statistic, sig_digits),
      p = round(p, sig_digits)
    )
  } else {
    result <- c(
      logit_p = round(estimate, sig_digits),
      se_logit_p = round(se, sig_digits),
      proportion = round(estimate_exp, sig_digits),
      ci_ll = round(ci_lower, sig_digits),
      ci_ul = round(ci_upper, sig_digits),
      z = round(statistic, sig_digits),
      p = round(p, sig_digits)
    )
  }

  # Rename p for sidedness
  names(result)[names(result) == "p"] <- if (one_sided) "p_one" else "p"

  # Print notes and messages
  if (length(messages)) cat(paste(messages, collapse = "\n"), "\n")
  if (length(approx_notes)) cat("Note(s):\n", paste(approx_notes, collapse = "\n"), "\n")

  return(result)
}
