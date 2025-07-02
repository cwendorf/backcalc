#' Backcalculate Missing Inferential Statistics for Correlations
#'
#' \code{backcalc_corrs()} reconstructs missing inferential statistics for correlation coefficients
#' using Fisher's z-transformation. It supports both one-sample and two-sample correlation comparisons
#' and allows flexible combinations of inputs to infer standard errors, test statistics, p-values, 
#' confidence intervals, and degrees of freedom.
#'
#' @param r Numeric. Correlation coefficient. For one-sample cases, a single value. For two-sample comparisons, a vector of length 2.
#' @param se Numeric. Standard error of the Fisher z-transformed correlation.
#' @param n Numeric. Sample size. For two-sample comparisons, provide a vector of length 2. Used to infer \code{se} and \code{df} when missing.
#' @param df Numeric. Degrees of freedom (optional). If not provided and \code{n} is available, it will be inferred.
#' @param p Numeric. p-value (optional). If not provided, it will be inferred from other inputs.
#' @param ci Numeric vector of length 2. Confidence interval for the correlation coefficient (on the raw correlation scale, not Fisher z).
#' @param statistic Numeric. Test statistic (usually a t- or z-value). Can be used to infer \code{se} if missing.
#' @param one_sided Logical. Whether the test is one-sided (default is \code{FALSE}). Affects p-value and confidence interval calculation.
#' @param conf.level Numeric between 0 and 1. Confidence level for the confidence interval (default is \code{0.95}).
#' @param digits Integer. Number of significant digits for rounding results (default = \code{3}).
#'
#' @return A named numeric vector containing:
#' \describe{
#'   \item{Estimate}{Estimated correlation (or Fisher z-difference if comparing two correlations).}
#'   \item{SE}{Standard error of the Fisher z-transformed correlation.}
#'   \item{t or z}{Test statistic (type depends on whether \code{df} is available).}
#'   \item{df}{Degrees of freedom (if available or inferred).}
#'   \item{p or p-one}{Two-sided or one-sided p-value.}
#'   \item{LL}{Lower bound of the confidence interval (on correlation scale).}
#'   \item{UL}{Upper bound of the confidence interval (on correlation scale).}
#' }
#'
#' @details
#' The function uses Fisher's r-to-z transformation (\code{atanh()}) for inferential computations and back-transforms using \code{tanh()}.
#' If only a subset of values is provided (e.g., only \code{r} and \code{statistic}), the function attempts to infer the rest.
#'
#' In two-sample cases, the difference between Fisher z-transformed correlations is used as the estimate, and the standard error is derived accordingly.
#'
#' Informative notes are printed to indicate any approximations or inferred values used during the calculation.
#'
#' @examples
#' # One-sample: r + p-value + df (infer se, CI)
#' backcalc_corrs(r = 0.52, p = 0.02, df = 18)
#'
#' # Two-sample: Two correlations + unequal n (infer difference, se, df, p, CI)
#' backcalc_corrs(r = c(0.70, 0.50), n = c(40, 25))
#'
#' # Insufficient information: Two correlations but missing sample sizes (error)
#' backcalc_corrs(r = c(0.3, 0.5))
#'
#' @export
backcalc_corrs <- function(r = NULL, se = NULL, n = NULL, df = NULL,
                           p = NULL, ci = NULL, one_sided = FALSE, 
                           digits = 3, statistic = NULL, conf.level = 0.95) {
  
  approx_notes <- character(0)
  messages <- character(0)
  
  # Validate r
  if (is.null(r)) {
    stop("Insufficient information: Correlation coefficient (r) must be provided.")
  }
  if (any(abs(r) > 1, na.rm = TRUE)) {
    stop("Invalid input: Correlation coefficients must be between -1 and 1.")
  }
  
  # Validate conf.level
  if (!is.numeric(conf.level) || length(conf.level) != 1 || conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be a single numeric value between 0 and 1 (exclusive).")
  }
  
  # Infer df from n if missing
  if (is.null(df) && !is.null(n)) {
    if (length(n) == 1) {
      df <- n - 2
      approx_notes <- c(approx_notes, "df approximated as n - 2.")
    } else if (length(n) == 2) {
      df <- min(n) - 3
      approx_notes <- c(approx_notes, "df approximated as min(n) - 3 for two-sample case.")
    }
  }
  
  # Check CI validity if provided
  if (!is.null(ci)) {
    if (!is.numeric(ci) || length(ci) != 2 || any(abs(ci) >= 1)) {
      stop("Invalid input: CI must be a numeric vector of length 2 with values between -1 and 1.")
    }
  }
  
  # Convert correlation(s) to Fisher z if two-sample or single
  estimate <- r
  
  # Two-sample case
  if (length(estimate) == 2) {
    if (is.null(n) || length(n) != 2) {
      stop("Insufficient information: Two correlations provided but sample sizes (n) missing or incomplete.")
    }
    
    z1 <- atanh(estimate[1])
    z2 <- atanh(estimate[2])
    estimate <- z1 - z2
    
    # SE from difference formula, unless se provided
    if (is.null(se)) {
      se <- sqrt(1 / (n[1] - 3) + 1 / (n[2] - 3))
      approx_notes <- c(approx_notes,
                        "SE derived from Fisher z difference formula.")
    }
    
    # df already inferred above or user-provided
    
    # If statistic provided, override calculation of stat below
    if (!is.null(statistic)) {
      stat <- statistic
    } else {
      stat <- estimate / se
    }
    
  } else {
    # One-sample case
    estimate <- atanh(r)
    
    # Infer or validate SE
    if (is.null(se)) {
      # From n
      if (!is.null(n)) {
        se <- 1 / sqrt(n - 3)
        approx_notes <- c(approx_notes, "SE approximated using 1 / sqrt(n - 3).")
      } else if (!is.null(ci)) {
        # From CI in Fisher z scale
        z_ci <- atanh(ci)
        crit <- if (!is.null(df)) {
          qt(1 - (1 - conf.level)/2, df)
        } else {
          qnorm(1 - (1 - conf.level)/2)
        }
        se <- diff(range(z_ci)) / (2 * crit)
        approx_notes <- c(approx_notes, "SE approximated from CI in Fisher z scale.")
      } else if (!is.null(p) && !is.null(df)) {
        # From p-value and df using t quantile
        stat_val <- if (one_sided) qt(1 - p, df) else qt(1 - p/2, df)
        se <- estimate / stat_val
        approx_notes <- c(approx_notes, "SE approximated from p-value and df.")
      } else if (!is.null(statistic) && !is.null(df)) {
        se <- estimate / statistic
        approx_notes <- c(approx_notes, "SE approximated from test statistic and df.")
      } else {
        stop("Insufficient information: Provide se, n, ci, or p with df, or statistic with df to infer missing statistics.")
      }
    }
    
    # Calculate test statistic if not provided
    if (is.null(statistic)) {
      stat <- estimate / se
    } else {
      stat <- statistic
    }
  }
  
  # Determine test type: t if df available, else z
  stat_type <- if (!is.null(df)) "t" else "z"
  
  # Calculate p-value if missing
  if (is.null(p)) {
    if (stat_type == "t") {
      p <- if (one_sided) 1 - pt(stat, df) else 2 * (1 - pt(abs(stat), df))
    } else {
      p <- if (one_sided) 1 - pnorm(stat) else 2 * (1 - pnorm(abs(stat)))
    }
  }
  
  # Calculate critical value for CI
  crit <- if (!is.null(df)) {
    qt(1 - (1 - conf.level)/2, df)
  } else {
    qnorm(1 - (1 - conf.level)/2)
  }
  
  # Confidence interval on Fisher z scale
  ci_z <- estimate + c(-1, 1) * crit * se
  ci_r <- tanh(ci_z)
  
  # Final r to report (for two-sample, back-transform difference to r difference)
  final_r <- if (length(r) == 2) tanh(estimate) else r
  
  # Assemble result
  result <- c(
    Estimate = round(final_r, digits),
    SE = round(se, digits),
    statistic = round(stat, digits),
    df = if (!is.null(df)) round(df, 0) else NA,
    p_value = round(p, digits),
    LL = round(ci_r[1], digits),
    UL = round(ci_r[2], digits)
  )
  
  names(result)[names(result) == "statistic"] <- stat_type
  names(result)[names(result) == "p_value"] <- if (one_sided) "p-one" else "p"
  
  # Print messages and approximation notes separately
  if (length(messages)) cat(paste(messages, collapse = "\n"), "\n")
  if (length(approx_notes)) cat("\nNote(s):\n", paste(approx_notes, collapse = "\n"), "\n", sep = "")
  
  return(result)
}
