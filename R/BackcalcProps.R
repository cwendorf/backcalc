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
  estimate <- prop
  interval_type <- match.arg(interval_type)
  
  # Helper: logit and inverse logit
  logit <- function(p) log(p / (1 - p))
  inv_logit <- function(l) exp(l) / (1 + exp(l))
  
  # Exact binomial CI (Clopper-Pearson)
  exact_ci <- function(x, n, conf.level = 0.95) {
    alpha <- 1 - conf.level
    lower <- if (x == 0) 0 else qbeta(alpha / 2, x, n - x + 1)
    upper <- if (x == n) 1 else qbeta(1 - alpha / 2, x + 1, n - x)
    c(lower, upper)
  }
  
  # Convert percentage to proportion if needed
  if (!is.null(estimate)) {
    if (any(estimate > 1)) estimate <- estimate / 100
  }
  
  # If x and n are given but estimate missing, compute estimate
  if (!is.null(x) && !is.null(n) && is.null(estimate)) {
    estimate <- x / n
  }
  
  # Check if two-sample (two estimates and two ns)
  two_sample_case <- !is.null(estimate) && length(estimate) == 2 && !is.null(n) && length(n) == 2
  
  # Function to get critical value for CI and tests
  get_crit <- function() {
    alpha <- 0.05
    if (one_sided) qnorm(1 - alpha) else qnorm(1 - alpha / 2)
  }
  
  # Infer SE for single sample if not provided (Wald approx on logit scale)
  if (!two_sample_case) {
    if (is.null(se)) {
      if (!is.null(estimate) && !is.null(n)) {
        se_logit <- sqrt(1 / (n * estimate) + 1 / (n * (1 - estimate)))
        se <- se_logit
      }
    }
  }
  
  # For two-sample, calculate difference of logits and SE of difference
  if (two_sample_case) {
    p1 <- estimate[1]
    p2 <- estimate[2]
    n1 <- n[1]
    n2 <- n[2]
    
    # Logits
    l1 <- logit(p1)
    l2 <- logit(p2)
    estimate_diff <- l1 - l2
    
    # SE of difference
    se1 <- sqrt(1 / (n1 * p1) + 1 / (n1 * (1 - p1)))
    se2 <- sqrt(1 / (n2 * p2) + 1 / (n2 * (1 - p2)))
    se_diff <- sqrt(se1^2 + se2^2)
    
    # Override estimate and se with difference scale values
    estimate <- estimate_diff
    se <- se_diff
  }
  
  # If CI is provided but estimate or SE missing, infer on logit scale
  if (!is.null(ci)) {
    if (length(ci) != 2) stop("ci must be length 2: c(lower, upper)")
    if (any(ci < 0 | ci > 1)) stop("CI bounds must be between 0 and 1")
    ci_logit <- logit(ci)
    if (is.null(estimate)) estimate <- mean(ci_logit)
    if (is.null(se)) {
      crit <- get_crit()
      se <- abs(ci_logit[2] - ci_logit[1]) / (2 * crit)
    }
  }
  
  # Calculate test statistic (z)
  if (!is.null(estimate) && !is.null(se)) {
    statistic <- estimate / se
  } else if (!is.null(p) && !is.null(estimate)) {
    crit_val <- if (one_sided) qnorm(1 - p) else qnorm(1 - p / 2)
    statistic <- sign(estimate) * crit_val
    se <- estimate / statistic
  } else {
    statistic <- NA
  }
  
  # Calculate p-value if missing
  if (is.null(p) && !is.na(statistic)) {
    if (one_sided) {
      p <- 1 - pnorm(statistic)
    } else {
      p <- 2 * (1 - pnorm(abs(statistic)))
    }
  }
  
  crit <- get_crit()
  
  # Calculate confidence interval
  if (interval_type == "wald") {
    # Wald CI on logit scale, then back-transform
    ci_lower <- inv_logit(estimate - crit * se)
    ci_upper <- inv_logit(estimate + crit * se)
  } else if (interval_type == "exact") {
    # Exact Clopper-Pearson for single proportion only
    if (two_sample_case) {
      stop("Exact intervals not supported for two-sample difference case.")
    }
    if (is.null(x) || is.null(n)) {
      stop("Exact interval requires counts 'x' and sample size 'n'.")
    }
    ci_bounds <- exact_ci(x, n, conf.level = ifelse(one_sided, 0.90, 0.95))
    ci_lower <- ci_bounds[1]
    ci_upper <- ci_bounds[2]
  }
  
  # For two-sample difference, back-transform difference to odds ratio scale
  if (two_sample_case) {
    estimate_exp <- exp(estimate)
    ci_lower <- exp(estimate - crit * se)
    ci_upper <- exp(estimate + crit * se)
  } else {
    estimate_exp <- inv_logit(estimate)
  }
  
  # Round outputs and prepare result vector
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
  names(result)[names(result) == "p"] <- ifelse(one_sided, "p_one", "p")
  
  return(result)
}
