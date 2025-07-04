#' Backcalculate Missing Inferential Statistics for Medians
#'
#' This function estimates standard errors, test statistics, p-values, confidence intervals,
#' and degrees of freedom for median-based statistics using summary-level data.
#' It supports robust measures of spread such as interquartile range (IQR), median absolute deviation (MAD),
#' and range, applying normal approximation for inference without relying on parametric assumptions.
#' When parametric inputs (e.g., standard deviations) are provided, t-distribution based inference is used.
#'
#' @param m Numeric scalar or vector. Median(s) of the group(s). For two-sample comparisons, provide a length-2 vector.
#' @param se Numeric scalar or vector. Standard error(s) of the estimate(s).
#' @param sd Numeric scalar or vector. Standard deviation(s) of the group(s).
#' @param n Numeric scalar or vector. Sample size(s).
#' @param df Numeric scalar. Degrees of freedom for t-distribution inference.
#' @param p Numeric scalar. p-value for the test statistic.
#' @param ci Numeric vector of length 2. Confidence interval bounds (lower, upper).
#' @param statistic Numeric scalar. Test statistic (z or t).
#' @param iqr Numeric scalar or vector. Interquartile range(s).
#' @param mad Numeric scalar or vector. Median absolute deviation(s).
#' @param range Numeric scalar or vector. Range(s) of the data.
#' @param paired Logical. Whether the data are paired (default FALSE).
#' @param one_sided Logical. Whether the test is one-sided (default FALSE).
#' @param conf.level Numeric scalar. Confidence level for intervals (default 0.95).
#' @param digits Integer. Number of decimal places to round results (default 3).
#'
#' @return A named numeric vector with components:
#' \describe{
#'   \item{Estimate}{Back-calculated estimate (median difference or median).}
#'   \item{SE}{Standard error of the estimate.}
#'   \item{z or t}{Test statistic, z for robust inference, t for parametric inference with df.}
#'   \item{df}{Degrees of freedom (if applicable; NA otherwise).}
#'   \item{p or p-one}{Two-sided or one-sided p-value.}
#'   \item{LL}{Lower confidence limit.}
#'   \item{UL}{Upper confidence limit.}
#' }
#'
#' @details
#' The function supports inference for medians using IQR, MAD, or range to approximate SE
#' with normal-based confidence intervals and z-tests. When parametric inputs like SD and sample size
#' are given without robust measures, Welch's t-test approximation is used.
#'
#' @examples
#' # One-sample: Median, IQR, and sample size
#' backcalc_medians(m = 50, iqr = 20, n = 30)
#'
#' # Two-sample: Group medians, MAD, and sample size
#' backcalc_medians(m = c(75, 68), mad = 9, n = 40)
#'
#' # Insufficient info: Only median and sample size, no dispersion
#' backcalc_medians(m = c(52, 49), n = 30)
#' 
#' @export
backcalc_medians <- function(m = NULL, se = NULL, sd = NULL, n = NULL, df = NULL,
                             p = NULL, ci = NULL, statistic = NULL,
                             iqr = NULL, mad = NULL, range = NULL,
                             paired = FALSE, one_sided = FALSE,
                             conf.level = 0.95, digits = 3) {
  estimate <- m
  messages <- character(0)
  approx_notes <- character(0)
  
  # Determine if parametric inference (mean/SD) or robust (median/IQR/MAD/range)
  robust_input <- !is.null(iqr) || !is.null(mad) || !is.null(range)
  parametric_input <- !is.null(sd) || !is.null(se)
  
  # Critical value function depending on inference type
  get_crit <- function(df = NULL, robust = FALSE) {
    alpha <- 1 - conf.level
    if (robust) {
      # Robust uses normal approx
      if (one_sided) qnorm(1 - alpha) else qnorm(1 - alpha / 2)
    } else {
      # Parametric uses t if df known, else normal
      if (one_sided) {
        if (!is.null(df)) qt(1 - alpha, df) else qnorm(1 - alpha)
      } else {
        if (!is.null(df)) qt(1 - alpha / 2, df) else qnorm(1 - alpha / 2)
      }
    }
  }
  
  # Normalize lengths for two-sample cases
  if (!is.null(sd) && length(sd) == 2 && !is.null(n) && length(n) == 1) {
    n <- rep(n, 2)
  }
  if (!is.null(n) && length(n) == 2 && !is.null(sd) && length(sd) == 1) {
    sd <- rep(sd, 2)
  }
  if (!is.null(iqr) && length(iqr) == 2 && !is.null(n) && length(n) == 1) {
    n <- rep(n, 2)
  }
  if (!is.null(n) && length(n) == 2 && !is.null(iqr) && length(iqr) == 1) {
    iqr <- rep(iqr, 2)
  }
  if (!is.null(mad) && length(mad) == 2 && !is.null(n) && length(n) == 1) {
    n <- rep(n, 2)
  }
  if (!is.null(n) && length(n) == 2 && !is.null(mad) && length(mad) == 1) {
    mad <- rep(mad, 2)
  }
  if (!is.null(range) && length(range) == 2 && !is.null(n) && length(n) == 1) {
    n <- rep(n, 2)
  }
  if (!is.null(n) && length(n) == 2 && !is.null(range) && length(range) == 1) {
    range <- rep(range, 2)
  }
  
  two_sample_case <- FALSE
  if (paired) {
    # paired test usually one sample of diffs
    two_sample_case <- FALSE
  } else {
    # Check if two groups with appropriate input lengths
    group_lengths <- c(
      sd_len = ifelse(is.null(sd), 0, length(sd)),
      iqr_len = ifelse(is.null(iqr), 0, length(iqr)),
      mad_len = ifelse(is.null(mad), 0, length(mad)),
      range_len = ifelse(is.null(range), 0, length(range)),
      n_len = ifelse(is.null(n), 0, length(n))
    )
    if (group_lengths["n_len"] == 2 && 
        (group_lengths["sd_len"] == 2 || group_lengths["iqr_len"] == 2 || 
         group_lengths["mad_len"] == 2 || group_lengths["range_len"] == 2)) {
      two_sample_case <- TRUE
    }
  }
  
  # Handle estimate difference if two-sample
  if (two_sample_case && !is.null(m) && length(m) == 2) {
    estimate <- m[1] - m[2]
  } else if (!two_sample_case && !is.null(m) && length(m) == 2) {
    # For one sample with vector, take difference anyway
    estimate <- m[1] - m[2]
  }
  
  # Approximate SE from dispersion measures (robust)
  if (robust_input) {
    if (is.null(se)) {
      if (!is.null(iqr) && !is.null(n)) {
        # Approx SE from IQR: SE ≈ IQR / (1.349 * sqrt(n))
        # 1.349 ≈ IQR of standard normal
        if (two_sample_case) {
          se <- sqrt( (iqr[1] / (1.349 * sqrt(n[1])))^2 + (iqr[2] / (1.349 * sqrt(n[2])))^2 )
        } else {
          se <- iqr / (1.349 * sqrt(n))
        }
        approx_notes <- c(approx_notes, "SE approximated from IQR and sample size using normal approximation.")
      } else if (!is.null(mad) && !is.null(n)) {
        # MAD ≈ 0.6745 * SD in normal data, so SE ≈ MAD / (0.6745 * sqrt(n))
        if (two_sample_case) {
          se <- sqrt( (mad[1] / (0.6745 * sqrt(n[1])))^2 + (mad[2] / (0.6745 * sqrt(n[2])))^2 )
        } else {
          se <- mad / (0.6745 * sqrt(n))
        }
        approx_notes <- c(approx_notes, "SE approximated from MAD and sample size using normal approximation.")
      } else if (!is.null(range) && !is.null(n)) {
        # Range SE ≈ range / (4 * sqrt(n)) (rough approx)
        if (two_sample_case) {
          se <- sqrt( (range[1] / (4 * sqrt(n[1])))^2 + (range[2] / (4 * sqrt(n[2])))^2 )
        } else {
          se <- range / (4 * sqrt(n))
        }
        approx_notes <- c(approx_notes, "SE approximated from range and sample size using rough normal approximation.")
      } else {
        messages <- c(messages, "Cannot approximate SE: provide sample size with IQR, MAD, or range.")
      }
    }
  }
  
  # If parametric input only and no robust input, handle parametric SE
  if (!robust_input && parametric_input && is.null(se) && !is.null(sd) && !is.null(n)) {
    if (length(sd) == length(n) || length(sd) == 1 || length(n) == 1) {
      if (two_sample_case) {
        var1 <- sd[1]^2
        var2 <- sd[2]^2
        n1 <- n[1]
        n2 <- n[2]
        if (!is.null(m) && length(m) == 2) {
          estimate <- m[1] - m[2]
        }
        if (var1 == var2 && n1 == n2) {
          pooled_var <- ((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2)
          se <- sqrt(pooled_var * (1 / n1 + 1 / n2))
          if (is.null(df)) df <- n1 + n2 - 2
        } else {
          se <- sqrt(var1 / n1 + var2 / n2)
          if (is.null(df)) {
            numerator <- (var1 / n1 + var2 / n2)^2
            denominator <- ((var1 / n1)^2) / (n1 - 1) + ((var2 / n2)^2) / (n2 - 1)
            df <- numerator / denominator
            approx_notes <- c(approx_notes, "Welch-Satterthwaite approximation used for df.")
          }
        }
      } else {
        # One sample parametric
        se <- sd / sqrt(n)
      }
    } else {
      messages <- c(messages, "Length mismatch between sd and n.")
    }
  }
  
  # If CI provided and se missing, approximate se from CI width
  if (!is.null(ci)) {
    if (length(ci) != 2) {
      messages <- c(messages, "Confidence interval must be length 2.")
    } else {
      if (is.null(estimate)) estimate <- mean(ci)
      if (is.null(se)) {
        crit <- get_crit(df, robust = robust_input)
        se <- abs(ci[2] - ci[1]) / (2 * crit)
        approx_notes <- c(approx_notes, "SE approximated from CI width.")
      }
    }
  }
  
  # Approximate estimate from test statistic and se
  if (!is.null(statistic) && !is.null(se) && is.null(estimate)) {
    estimate <- statistic * se
    approx_notes <- c(approx_notes, "Estimate approximated from test statistic and SE.")
  }
  
  # Approximate se from estimate and test statistic
  if (!is.null(statistic) && !is.null(estimate) && is.null(se)) {
    se <- abs(estimate / statistic)
    approx_notes <- c(approx_notes, "SE approximated from test statistic and estimate.")
  }
  
  # Check for insufficient information
  if (is.null(estimate) || (is.null(se) && is.null(p) && is.null(ci) && is.null(statistic))) {
    message("Insufficient information: Provide estimate and at least one of SE, p-value, CI, or test statistic.")
    return(invisible(NULL))
  }
  
  # Determine statistic type
  # Use z for robust inference, t for parametric with df
  if (robust_input) {
    statistic_type <- "z"
  } else {
    statistic_type <- if (!is.null(df)) "t" else "z"
  }
  
  # Calculate test statistic if missing
  if (is.null(statistic) && !is.null(estimate) && !is.null(se)) {
    statistic <- estimate / se
  } else if (is.null(statistic) && !is.null(p) && !is.null(estimate)) {
    crit_val <- if (one_sided) {
      if (!is.null(df) && !robust_input) qt(1 - p, df) else qnorm(1 - p)
    } else {
      if (!is.null(df) && !robust_input) qt(1 - p / 2, df) else qnorm(1 - p / 2)
    }
    statistic <- sign(estimate) * crit_val
    se <- abs(estimate / statistic)
    approx_notes <- c(approx_notes, "Test statistic and SE approximated from p-value and estimate.")
  }
  
  # Calculate p-value if missing
  if (is.null(p) && !is.null(statistic)) {
    if (statistic_type == "t") {
      p <- if (one_sided) 1 - pt(statistic, df) else 2 * (1 - pt(abs(statistic), df))
    } else {
      p <- if (one_sided) 1 - pnorm(statistic) else 2 * (1 - pnorm(abs(statistic)))
    }
  }
  
  crit <- get_crit(df, robust = robust_input)
  ci_lower <- estimate - crit * se
  ci_upper <- estimate + crit * se
  
  # Compose output
  result <- c(
    Estimate = round(estimate, digits),
    SE = round(se, digits),
    statistic = round(statistic, digits),
    df = if (!is.null(df) && !robust_input) round(df, 0) else NA,
    p = round(p, digits),
    LL = round(ci_lower, digits),
    UL = round(ci_upper, digits)
  )
  
  # Rename keys for reporting
  names(result)[names(result) == "p"] <- ifelse(one_sided, "p-one", "p")
  names(result)[names(result) == "statistic"] <- statistic_type
  
  if (length(messages)) cat(paste(messages, collapse = "\n"), "\n")
  if (length(approx_notes)) cat("Note(s):\n", paste(approx_notes, collapse = "\n"), "\n", sep = "")
  
  return(result)
}
