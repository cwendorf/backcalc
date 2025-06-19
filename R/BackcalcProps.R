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
                           interval_type = c("wald", "exact"),
                           test_statistic = c("z", "t")) {
  interval_type <- match.arg(interval_type)
  test_statistic <- match.arg(test_statistic)
  messages <- character(0)
  approx_notes <- character(0)

  logit <- function(p) log(p / (1 - p))
  inv_logit <- function(l) exp(l) / (1 + exp(l))
  get_crit <- function() {
    alpha <- if (one_sided) 0.10 else 0.05
    if (test_statistic == "z") {
      qnorm(1 - alpha / ifelse(one_sided, 1, 2))
    } else {
      qt(1 - alpha / ifelse(one_sided, 1, 2), df = 1000)
    }
  }

  # Validate inputs
  if (is.null(prop) && (is.null(x) || is.null(n))) {
    messages <- c(messages, "Provide 'prop' or both 'x' and 'n'.")
    cat(paste(messages, collapse = "\n"), "\n")
    return(invisible(NULL))
  }

  # Convert percentages
  estimate <- prop
  if (!is.null(estimate) && any(estimate > 1)) {
    estimate <- estimate / 100
    approx_notes <- c(approx_notes, "Proportions >1 assumed percentages and converted.")
  }

  # Compute from x and n if needed
  if (is.null(estimate) && !is.null(x) && !is.null(n)) {
    estimate <- x / n
    approx_notes <- c(approx_notes, "Estimate computed as x/n.")
  }

  if (!is.null(estimate) && any(estimate < 0 | estimate > 1)) {
    messages <- c(messages, "Proportions must be between 0 and 1.")
    cat(paste(messages, collapse = "\n"), "\n")
    return(invisible(NULL))
  }

  # Determine one-sample vs two-sample
  is_two_sample <- length(estimate) == 2 && length(n) == 2
  crit <- get_crit()

  # Two-sample logic
  if (is_two_sample) {
    p1 <- estimate[1]
    p2 <- estimate[2]
    n1 <- n[1]
    n2 <- n[2]

    estimate_diff <- p1 - p2
    se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
    statistic <- estimate_diff / se
    ci_ll <- estimate_diff - crit * se
    ci_ul <- estimate_diff + crit * se
    approx_notes <- c(approx_notes, "Two-sample SE calculated using Wald formula for difference in proportions.")

    # Compute p if missing
    if (is.null(p)) {
      p <- if (one_sided) {
        if (test_statistic == "z") 1 - pnorm(statistic) else 1 - pt(statistic, df = 1000)
      } else {
        if (test_statistic == "z") 2 * (1 - pnorm(abs(statistic))) else 2 * (1 - pt(abs(statistic), df = 1000))
      }
    }

    out <- c(
      prop = round(estimate_diff, sig_digits),
      se = round(se, sig_digits),
      ci_ll = round(ci_ll, sig_digits),
      ci_ul = round(ci_ul, sig_digits),
      z = round(statistic, sig_digits),
      p = round(p, sig_digits)
    )
  }

  # One-sample logic
  else {
    if (is.null(se) && !is.null(estimate) && !is.null(n)) {
      if (any(estimate == 0) || any(estimate == 1)) {
        messages <- c(messages, "Cannot calculate SE for proportions exactly 0 or 1.")
        cat(paste(messages, collapse = "\n"), "\n")
        return(invisible(NULL))
      }
      se <- sqrt(1 / (n * estimate) + 1 / (n * (1 - estimate)))
      approx_notes <- c(approx_notes, "SE estimated from prop and n using Wald approximation.")
    }

    # Back-calc from CI if needed
    if (!is.null(ci) && length(ci) == 2 && all(ci >= 0 & ci <= 1)) {
      if (any(ci == 0) || any(ci == 1)) {
        messages <- c(messages, "CI bounds cannot be exactly 0 or 1 for logit transformation.")
        cat(paste(messages, collapse = "\n"), "\n")
        return(invisible(NULL))
      }
      ci_logit <- logit(ci)
      if (is.null(estimate)) {
        estimate <- mean(ci_logit)
        approx_notes <- c(approx_notes, "Estimate approximated as midpoint of logit CI.")
      }
      if (is.null(se)) {
        se <- abs(ci_logit[2] - ci_logit[1]) / (2 * crit)
        approx_notes <- c(approx_notes, "SE approximated from width of logit CI.")
      }
    }

    # Compute from estimate and p if needed
    if (is.null(se) && !is.null(p) && !is.null(estimate)) {
      crit_val <- if (one_sided) {
        if (test_statistic == "z") qnorm(1 - p) else qt(1 - p, df = 1000)
      } else {
        if (test_statistic == "z") qnorm(1 - p / 2) else qt(1 - p / 2, df = 1000)
      }
      statistic <- sign(estimate) * crit_val
      se <- estimate / statistic
      approx_notes <- c(approx_notes, "SE approximated from estimate and p-value.")
    }

    if (!is.null(estimate) && !is.null(se)) {
      statistic <- estimate / se
    } else {
      messages <- c(messages, "Insufficient information: provide 'se', 'n', 'ci', or 'p' with 'estimate'.")
      cat(paste(messages, collapse = "\n"), "\n")
      return(invisible(NULL))
    }

    # Compute p if missing
    if (is.null(p)) {
      p <- if (one_sided) {
        if (test_statistic == "z") 1 - pnorm(statistic) else 1 - pt(statistic, df = 1000)
      } else {
        if (test_statistic == "z") 2 * (1 - pnorm(abs(statistic))) else 2 * (1 - pt(abs(statistic), df = 1000))
      }
    }

    # CI on logit scale
    ci_ll <- inv_logit(estimate - crit * se)
    ci_ul <- inv_logit(estimate + crit * se)

    out <- c(
      prop = round(estimate, sig_digits),
      se = round(se, sig_digits),
      ci_ll = round(ci_ll, sig_digits),
      ci_ul = round(ci_ul, sig_digits),
      z = round(statistic, sig_digits),
      p = round(p, sig_digits)
    )
  }

  # Label output correctly
  stat_label <- test_statistic
  p_label <- if (one_sided) "p_one" else "p"
  names(out)[5:6] <- c(stat_label, p_label)

  # Show messages/notes
  if (length(messages)) cat(paste(messages, collapse = "\n"), "\n")
  if (length(approx_notes)) cat("Note(s):\n", paste(approx_notes, collapse = "\n"), "\n")

  return(out)
}
