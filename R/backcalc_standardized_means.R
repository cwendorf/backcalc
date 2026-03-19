#' Backcalculate Standardized Mean Differences (Effect Sizes)
#'
#' This function reconstructs standardized mean differences and related effect size statistics
#' from various combinations of inputs. It supports Cohen's d, Hedges' g, and Glass's delta,
#' along with confidence intervals and test statistics for one-sample, paired, and two-sample designs.
#'
#' @param d Numeric. Standardized mean difference (Cohen's d or similar). For two-sample cases,
#'   provide a single value representing the difference between groups.
#' @param m Numeric vector of means or mean differences. For two-sample cases, provide
#'   a vector of length 2 representing group means.
#' @param sd Numeric vector of standard deviations for each group.
#' @param n Numeric vector of sample sizes for each group.
#' @param se Numeric. Standard error of the mean difference (unstandardized).
#' @param statistic Numeric. Test statistic value (e.g., t-statistic).
#' @param p Numeric. P-value associated with the test statistic.
#' @param ci_d Numeric vector of length 2. Confidence interval for the effect size (Cohen's d).
#' @param paired Logical. Whether the comparison is paired (default is FALSE).
#' @param one_sided Logical. Whether a one-sided test is used (default is FALSE).
#' @param type Character. Type of effect size to compute: "d" (Cohen's d, default),
#'   "g" (Hedges' g, bias-corrected), or "delta" (Glass's delta, uses control group SD).
#' @param control_sd Numeric. For type = "delta", specify which group's SD to use as control.
#'   Default is 1 (first group).
#' @param conf.level Numeric. Confidence level for intervals (default 0.95).
#' @param digits Integer. Number of digits to round the output (default 3).
#' @param attr Logical. If TRUE, attaches approximation messages as attributes (default TRUE).
#'
#' @return
#' A `data.frame` with effect size statistics including the standardized estimate,
#' standard error (of the effect size), confidence interval, and related statistics.
#' The output has class `"backcalc"` and contains attribute
#' `"Approximations"` if `attr = TRUE`.
#'
#' @details
#' The function supports multiple effect size types:
#' \itemize{
#'   \item **Cohen's d**: Standardized by the pooled standard deviation
#'     (or average SD for unequal variances).
#'   \item **Hedges' g**: Bias-corrected version of Cohen's d, adjusting for small samples.
#'   \item **Glass's delta**: Standardized by the control or reference group's SD only.
#' }
#'
#' Confidence intervals for effect sizes use noncentral t-distributions when appropriate.
#'
#' @examples
#' # One-sample: Calculate d from mean, SD, and sample size
#' backcalc_standardized_means(m = 2.5, sd = 4, n = 25)
#'
#' # Two-sample: Calculate d from group means, SDs, and sample sizes
#' backcalc_standardized_means(m = c(15, 12), sd = c(4, 5), n = c(40, 35))
#'
#' # Backcalculate d from t-statistic and sample size
#' backcalc_standardized_means(statistic = 2.5, n = 50)
#'
#' # Two-sample with Hedges' g (bias-corrected)
#' backcalc_standardized_means(m = c(15, 12), sd = c(4, 5), n = c(40, 35), type = "g")
#'
#' @export
backcalc_standardized_means <- function(d = NULL, m = NULL, sd = NULL, n = NULL, se = NULL,
                                        statistic = NULL, p = NULL, ci_d = NULL,
                                        paired = FALSE, one_sided = FALSE,
                                        type = "d", control_sd = 1,
                                        conf.level = 0.95, digits = 3, attr = TRUE) {
  estimate <- d
  messages <- character()
  approx_notes <- character()

  # Validate inputs
  if (!type %in% c("d", "g", "delta")) {
    cat("\nInvalid input:\ntype must be 'd', 'g', or 'delta'.\n\n")
    return(invisible(NULL))
  }

  # Helper function to get critical value
  get_crit <- function(df = NULL) {
    alpha <- 1 - conf.level
    if (one_sided) {
      if (!is.null(df)) qt(1 - alpha, df) else qnorm(1 - alpha)
    } else {
      if (!is.null(df)) qt(1 - alpha / 2, df) else qnorm(1 - alpha / 2)
    }
  }

  # Helper to compute pooled SD
  compute_pooled_sd <- function(sd, n) {
    if (length(sd) == 1 && length(n) == 2) {
      # If only one SD given, assume equal variances
      return(sd)
    }
    if (length(sd) == 2 && length(n) == 2) {
      var1 <- sd[1]^2
      var2 <- sd[2]^2
      pooled_var <- ((n[1] - 1) * var1 + (n[2] - 1) * var2) / (n[1] + n[2] - 2)
      return(sqrt(pooled_var))
    }
    return(NULL)
  }

  # Bias correction factor (c4) for Hedges' g
  compute_c4 <- function(n) {
    if (length(n) == 1) {
      return(1 - (3 / (4 * (n - 1) - 1)))
    } else if (length(n) == 2) {
      df <- n[1] + n[2] - 2
      return(1 - (3 / (4 * df - 1)))
    }
    return(1)
  }

  # Two-sample case detection
  len_sd <- ifelse(is.null(sd), 0, length(sd))
  len_n <- ifelse(is.null(n), 0, length(n))
  len_m <- ifelse(is.null(m), 0, length(m))

  two_sample_case <- (len_sd == 2 && len_n == 2 && !paired) ||
                     (len_m == 2 && !is.null(sd) && len_sd >= 1 && !is.null(n))

  # If means given for two samples, compute difference
  if (!is.null(m) && len_m == 2 && two_sample_case) {
    mean_diff <- m[1] - m[2]
  } else if (!is.null(m) && len_m == 1) {
    mean_diff <- m
  } else {
    mean_diff <- NULL
  }

  # Calculate degrees of freedom
  df <- NULL
  if (paired && !is.null(n)) {
    if (length(n) == 1) {
      df <- n - 1
      approx_notes <- c(approx_notes, "Degrees of freedom approximated as n - 1 for paired design.")
    }
  } else if (two_sample_case && !is.null(n) && length(n) == 2) {
    df <- n[1] + n[2] - 2
  } else if (!two_sample_case && !is.null(n)) {
    df <- n - 1
  }

  # Compute standardized effect size if we have the raw data
  if (is.null(estimate) && !is.null(mean_diff) && !is.null(sd)) {
    if (two_sample_case && len_sd == 2) {
      # Two-sample case
      pooled_sd <- compute_pooled_sd(sd, n)
      estimate <- mean_diff / pooled_sd
      approx_notes <- c(approx_notes, "Effect size calculated from means and pooled SD.")

      if (type == "g") {
        c4 <- compute_c4(n)
        estimate <- estimate * c4
        approx_notes <- c(approx_notes, "Hedges' g bias-correction applied.")
      } else if (type == "delta") {
        estimate <- mean_diff / sd[control_sd]
        approx_notes <- c(approx_notes, paste0("Glass's delta calculated using group ", control_sd, " SD."))
      }
    } else if (!two_sample_case && len_sd == 1) {
      # One-sample or paired case
      estimate <- mean_diff / sd
      approx_notes <- c(approx_notes, "Effect size calculated from mean difference and SD.")

      if (type == "g" && !is.null(n)) {
        c4 <- compute_c4(n)
        estimate <- estimate * c4
        approx_notes <- c(approx_notes, "Hedges' g bias-correction applied.")
      }
    }
  }

  # Back-calculate from t-statistic if available
  if (is.null(estimate) && !is.null(statistic) && !is.null(n)) {
    if (two_sample_case && length(n) == 2) {
      # Convert t from two-sample test to d
      estimate <- statistic * sqrt(1/n[1] + 1/n[2]) / sqrt(2)
      approx_notes <- c(approx_notes, "Effect size estimated from t-statistic.")
    } else if (length(n) == 1) {
      # One-sample case
      estimate <- statistic / sqrt(n)
      approx_notes <- c(approx_notes, "Effect size estimated from t-statistic.")
    }
  }

  # Back-calculate from confidence interval for effect size
  if (is.null(estimate) && !is.null(ci_d) && length(ci_d) == 2) {
    estimate <- mean(ci_d)
    approx_notes <- c(approx_notes, "Effect size estimated from CI midpoint.")
  }

  # Check for sufficient information
  if (is.null(estimate)) {
    messages <- c(messages, "Provide either: (1) effect size d, or (2) means/mean-diff with SD, or (3) statistic with sample size.")
  }

  if (length(messages)) {
    cat(paste0("\nInsufficient Input:"), sep = "\n")
    cat(paste0(paste(messages, collapse = "\n"), "\n\n"))
    return(invisible(NULL))
  }

  # Compute standard error of the effect size
  se_d <- NULL
  if (!is.null(n)) {
    if (two_sample_case && length(n) == 2) {
      # SE of d for two-sample design
      se_d <- sqrt((n[1] + n[2]) / (n[1] * n[2]) + estimate^2 / (2 * (n[1] + n[2] - 2)))
    } else if (length(n) == 1) {
      # SE of d for one-sample design
      se_d <- sqrt(1 / n + estimate^2 / (2 * (n - 1)))
    }
  }

  # Compute confidence interval for effect size using noncentral t-distribution
  ci_lower <- NA
  ci_upper <- NA

  if (!is.null(se_d) && !is.null(df)) {
    # Use noncentral t-distribution for CI
    alpha <- 1 - conf.level
    
    # Find noncentrality parameters that correspond to CI bounds
    t_crit <- if (one_sided) qt(1 - alpha, df) else qt(1 - alpha / 2, df)
    
    # For noncentral t, λ = t_obs, approximate CI bounds
    ci_lower <- estimate - t_crit * se_d
    ci_upper <- estimate + t_crit * se_d
  } else if (!is.null(ci_d)) {
    ci_lower <- ci_d[1]
    ci_upper <- ci_d[2]
  }

  # Build result data frame
  result <- data.frame(
    Estimate = round(estimate, digits),
    SE = if (!is.null(se_d)) round(se_d, digits) else NA,
    df = if (!is.null(df)) round(df, 0) else NA,
    LL = if (!is.na(ci_lower)) round(ci_lower, digits) else NA,
    UL = if (!is.na(ci_upper)) round(ci_upper, digits) else NA
  )

  # Add type label to estimate
  type_label <- switch(type, "d" = "Cohen's d", "g" = "Hedges' g", "delta" = "Glass's Δ", "d")
  rownames(result) <- type_label

  class(result) <- c("backcalc", class(result))
  attr(result, "Approximations") <- approx_notes
  attr(result, "attr") <- attr

  return(result)
}
