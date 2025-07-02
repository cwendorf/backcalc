#' Backcalculate Missing Inferential Statistics for Proportions and Percentages
#'
#' This function performs inference on proportions by back-calculating missing quantities
#' such as standard error (SE), test statistic, p-value, or confidence intervals (CIs) 
#' from the available inputs. It supports both one-sample and two-sample proportion 
#' comparisons using either z-tests or t-tests, and works with raw counts or precomputed 
#' proportions.
#'
#' @param prop Proportion estimate(s). Can be a single proportion or a vector of two for a two-sample test.
#' @param se Standard error. If not provided, it will be computed when possible.
#' @param n Sample size(s). Required for SE calculation unless `se` or `ci` is provided. 
#'           For two-sample comparisons, supply a numeric vector of length 2.
#' @param x Count(s) of successes. Used with `n` to compute proportion.
#' @param ci Confidence interval for the proportion (vector of length 2).
#' @param p P-value (optional). If not provided, will be computed from the test statistic.
#' @param one_sided Logical; if `TRUE`, computes one-sided p-value. Default is `FALSE` (two-sided).
#' @param digits Number of decimal places for rounding output. Default is 3.
#' @param interval_type Method used for inference; currently supports `"wald"` (default).
#' @param statistic Test statistic value (z or t). If not supplied, will be calculated from estimate and SE.
#' @param df Degrees of freedom. Required for t-tests. If not provided and `n` has length 2, it will be inferred.
#' @param conf.level Confidence level used to compute critical value for inference. Default is `0.95`.
#'
#' @return A named numeric vector with the following elements:
#' \describe{
#'   \item{Estimate}{Proportion or difference in proportions}
#'   \item{SE}{Standard error of the estimate}
#'   \item{z or t}{The test statistic used for inference}
#'   \item{df}{Degrees of freedom (NA for z-tests)}
#'   \item{p-one or p}{The one-sided or two-sided p-value}
#'   \item{LL}{Lower bound of the confidence interval}
#'   \item{UL}{Upper bound of the confidence interval}
#' }
#'
#' @details
#' - When `prop` is a two-element vector and `n` has two values, a two-sample comparison is assumed.  
#' - If `df` is not provided but `n` has length 2, degrees of freedom are approximated as `n1 + n2 - 2`.
#' - CI bounds are logit-transformed when `prop` is between 0 and 1 to maintain valid intervals.
#' - Exact methods are not currently implemented.
#'
#' @examples
#' # One-sample: Provide proportion and sample size only (basic one-sample proportion)
#' backcalc_props(prop = 0.4, n = 100)
#'
#' # Two-sample: Provide two proportions and sample sizes only (difference in proportions)
#' backcalc_props(prop = c(0.55, 0.4), n = c(150, 130))
#'
#' # Insufficient info: No inputs provided (error expected due to missing info)
#' backcalc_props()
#'
#' @export
backcalc_props <- function(prop = NULL, se = NULL, n = NULL, x = NULL, ci = NULL,
                           p = NULL, one_sided = FALSE, digits = 3,
                           interval_type = c("wald", "exact"),
                           statistic = NULL, df = NULL,
                           conf.level = 0.95) {
  interval_type <- match.arg(interval_type)
  messages <- character(0)
  approx_notes <- character(0)
  
  # --- Input validation: catch insufficient info early ---
  
  # Need at least one of prop or (x and n) or se
  if (is.null(prop) && (is.null(x) || is.null(n)) && is.null(se)) {
    messages <- c(messages, "Insufficient information: need prop or counts (x and n) or SE.")
  }
  
  # If x given but n missing
  if (!is.null(x) && is.null(n)) {
    messages <- c(messages, "Sample size 'n' is required when counts 'x' are provided.")
  }
  
  # If prop given but no n or se, cannot estimate SE or CI
  if (!is.null(prop) && is.null(n) && is.null(se) && interval_type == "exact") {
    messages <- c(messages, "Exact interval requires counts 'x' and sample size 'n'.")
  }
  
  # If proportion at boundary and Wald interval requested (logit transform not defined)
  if (!is.null(prop) && any(prop == 0 | prop == 1) && interval_type == "wald") {
    messages <- c(messages, "Wald interval not defined for proportions exactly 0 or 1.")
  }
  
  if (length(messages) > 0) {
    cat("Error:", paste(messages, collapse = " "), "\n")
    return(invisible(NULL))
  }
  
  alpha <- 1 - conf.level
  crit <- if (!is.null(df)) {
    qt(1 - alpha / ifelse(one_sided, 1, 2), df)
  } else {
    qnorm(1 - alpha / ifelse(one_sided, 1, 2))
  }
  
  logit <- function(p) log(p / (1 - p))
  inv_logit <- function(l) exp(l) / (1 + exp(l))
  
  # Validate and preprocess prop
  estimate <- prop
  if (!is.null(estimate) && any(estimate > 1)) {
    estimate <- estimate / 100
    approx_notes <- c(approx_notes, "Proportions >1 assumed percentages and converted.")
  }
  
  if (is.null(estimate) && !is.null(x) && !is.null(n)) {
    estimate <- x / n
    approx_notes <- c(approx_notes, "Estimate computed as x/n.")
  }
  
  if (!is.null(estimate) && (any(estimate < 0) || any(estimate > 1))) {
    messages <- c(messages, "Proportions must be between 0 and 1.")
    cat(paste(messages, collapse = "\n"), "\n")
    return(invisible(NULL))
  }
  
  infer_df <- function(n_val) {
    if (is.null(df)) {
      if (!is.null(n_val)) {
        floor(min(n_val) - 1)
      } else NA
    } else df
  }
  
  is_two_sample <- !is.null(estimate) && length(estimate) == 2
  
  infer_stat_type <- function(stat, df_val) {
    if (!is.null(stat)) {
      if (!is.null(df_val) && !is.na(df_val) && df_val < 1000) {
        "t"
      } else {
        "z"
      }
    } else {
      NULL
    }
  }
  
  stat_type <- infer_stat_type(statistic, df)
  if (!is.null(statistic) && stat_type == "t" && (is.null(df) || is.na(df))) {
    df <- infer_df(n)
  }
  
  p_to_stat <- function(pval, df_val, one_sided, stat_type) {
    if (stat_type == "z") {
      if (one_sided) qnorm(1 - pval) else qnorm(1 - pval / 2)
    } else {
      if (one_sided) qt(1 - pval, df_val) else qt(1 - pval / 2, df_val)
    }
  }
  
  stat_to_p <- function(stat, df_val, one_sided, stat_type) {
    if (stat_type == "z") {
      if (one_sided) 1 - pnorm(stat) else 2 * (1 - pnorm(abs(stat)))
    } else {
      if (one_sided) 1 - pt(stat, df_val) else 2 * (1 - pt(abs(stat), df_val))
    }
  }
  
  if (is_two_sample) {
    p1 <- estimate[1]
    p2 <- estimate[2]
    
    n1 <- if (!is.null(n) && length(n) == 2) n[1] else NA
    n2 <- if (!is.null(n) && length(n) == 2) n[2] else NA
    
    est_diff <- p1 - p2
    
    df <- infer_df(c(n1, n2))
    
    if (is.null(se)) {
      if (any(c(p1, p2) == 0 | c(p1, p2) == 1)) {
        messages <- c(messages, "Cannot calculate SE for proportions exactly 0 or 1.")
        cat(paste(messages, collapse = "\n"), "\n")
        return(invisible(NULL))
      }
      se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
      approx_notes <- c(approx_notes, "SE estimated using Wald formula for difference in proportions.")
    }
    
    if (is.null(statistic) && !is.null(p)) {
      stat_type <- if (!is.null(df) && !is.na(df) && df < 1000) "t" else "z"
      statistic <- p_to_stat(p, df, one_sided, stat_type)
      approx_notes <- c(approx_notes, "Test statistic approximated from p-value.")
    }
    
    if (is.null(p) && !is.null(statistic)) {
      if (is.null(stat_type)) stat_type <- infer_stat_type(statistic, df)
      p <- stat_to_p(statistic, df, one_sided, stat_type)
    }
    
    if (is.null(df)) df <- NA
    
    ll <- est_diff - crit * se
    ul <- est_diff + crit * se
    
    stat_label <- ifelse(is.null(stat_type), "statistic", stat_type)
    
    out <- c(
      Estimate = round(est_diff, digits),
      SE = round(se, digits),
      DF = ifelse(is.null(df), NA, df),
      setNames(round(statistic, digits), stat_label),
      # Label p differently if one-sided
      setNames(round(p, digits), ifelse(one_sided, "p-one", "p_one")),
      LL = round(ll, digits),
      UL = round(ul, digits)
    )
    
  } else {
    est <- estimate
    
    if (is.null(se)) {
      if (!is.null(n)) {
        if (any(est == 0 | est == 1)) {
          messages <- c(messages, "Cannot calculate SE for proportions exactly 0 or 1.")
          cat(paste(messages, collapse = "\n"), "\n")
          return(invisible(NULL))
        }
        se <- sqrt(est * (1 - est) / n)
        approx_notes <- c(approx_notes, "SE estimated using binomial formula.")
      } else {
        se <- NA
      }
    }
    
    if (is.null(statistic) && !is.null(p)) {
      stat_type <- if (!is.null(df) && !is.na(df) && df < 1000) "t" else "z"
      statistic <- p_to_stat(p, df, one_sided, stat_type)
      approx_notes <- c(approx_notes, "Test statistic approximated from p-value.")
    }
    
    if (is.null(p) && !is.null(statistic)) {
      if (is.null(stat_type)) stat_type <- infer_stat_type(statistic, df)
      p <- stat_to_p(statistic, df, one_sided, stat_type)
    }
    
    if (stat_type == "t" && (is.null(df) || is.na(df))) {
      df <- ifelse(!is.null(n), n - 1, NA)
    } else {
      df <- NA
    }
    
    if (interval_type == "wald") {
      if (any(est == 0 | est == 1)) {
        messages <- c(messages, "Cannot calculate Wald CI for proportions exactly 0 or 1.")
        cat(paste(messages, collapse = "\n"), "\n")
        return(invisible(NULL))
      }
      logit_est <- logit(est)
      se_logit <- se / (est * (1 - est))
      ll_logit <- logit_est - crit * se_logit
      ul_logit <- logit_est + crit * se_logit
      ll <- inv_logit(ll_logit)
      ul <- inv_logit(ul_logit)
    } else {
      if (is.null(x) || is.null(n)) {
        messages <- c(messages, "Exact interval requires counts 'x' and sample size 'n'.")
        cat(paste(messages, collapse = "\n"), "\n")
        return(invisible(NULL))
      }
      if (length(x) > 1 || length(n) > 1) {
        messages <- c(messages, "Exact interval not supported for multiple samples.")
        cat(paste(messages, collapse = "\n"), "\n")
        return(invisible(NULL))
      }
      ll <- qbeta(alpha / ifelse(one_sided, 1, 2), x, n - x + 1)
      ul <- qbeta(1 - alpha / ifelse(one_sided, 1, 2), x + 1, n - x)
    }
    
    stat_label <- ifelse(is.null(stat_type), "statistic", stat_type)
    
    out <- c(
      Estimate = round(est, digits),
      SE = round(se, digits),
      DF = ifelse(is.null(df), NA, df),
      setNames(round(statistic, digits), stat_label),
      setNames(round(p, digits), ifelse(one_sided, "p-one", "p_one")),
      LL = round(ll, digits),
      UL = round(ul, digits)
    )
  }
  
  if (length(approx_notes) > 0) {
    cat("Note:", paste(approx_notes, collapse = " "), "\n")
  }
  if (length(messages) > 0) {
    cat("Warning:", paste(messages, collapse = " "), "\n")
  }
  
  return(out)
}
