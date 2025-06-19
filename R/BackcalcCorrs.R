#' Backcalc Missing Inferential Statistics for Correlations
#'
#' \code{backcalc_corrs()} reconstructs inferential statistics for correlation coefficients
#' using Fisher's z-transform. It supports reconstructing standard errors, confidence intervals,
#' p-values, and test statistics partial or varied combinations of summary and inferential statistics.
#'
#' @param r Numeric. Correlation coefficient. Can be a single value or a vector of length 2 (for group comparisons).
#' @param se Numeric. Standard error of the Fisher z-transformed correlation.
#' @param n Numeric. Sample size. For two-sample cases, provide a vector of length 2.
#' @param df Numeric. Degrees of freedom (optional). If not provided, will be inferred from \code{n}.
#' @param p Numeric. p-value (optional). If not provided, it will be inferred.
#' @param ci Numeric vector of length 2. Confidence interval for the correlation (on raw scale, not Fisher z).
#' @param one_sided Logical. Whether the test is one-sided (default is FALSE).
#' @param sig_digits Integer. Number of significant digits for rounding results (default = 3).
#'
#' @return A named numeric vector with the following elements:
#' \describe{
#'   \item{r}{Estimated correlation or difference in correlations.}
#'   \item{se}{Standard error of the Fisher z-transformed correlation.}
#'   \item{df}{Degrees of freedom (if available or inferred).}
#'   \item{ci_ll}{Lower bound of the confidence interval (on correlation scale).}
#'   \item{ci_ul}{Upper bound of the confidence interval (on correlation scale).}
#'   \item{t or z}{Test statistic (depending on whether df is available).}
#'   \item{p or p-one}{Two-sided or one-sided p-value.}
#' }
#'
#' @examples
#' # One-sample: r + SE + n (df inferred)
#' backcalc_corrs(r = 0.45, se = 0.1, n = 25, sig_digits = 3)
#' # One-sample: r + p-value + df provided (t-test)
#' backcalc_corrs(r = 0.52, p = 0.02, df = 18, sig_digits = 3)
#' # One-sample: r + confidence interval + df
#' backcalc_corrs(r = 0.35, ci = c(0.10, 0.55), df = 20, sig_digits = 3)
#' # Two-sample: Provide two rs, equal n, infer difference & df
#' backcalc_corrs(r = c(0.60, 0.40), n = c(30, 30), sig_digits = 3)
#' # Two-sample: Provide two rs with unequal ns, infer df
#' backcalc_corrs(r = c(0.70, 0.50), n = c(40, 25), sig_digits = 3)
#' # Two-sample: r difference + p-value + df (t-test)
#' backcalc_corrs(r = 0.18, p = 0.03, df = 45, sig_digits = 3)
#'
#' @export
backcalc_corrs <- function(r = NULL, se = NULL, n = NULL, df = NULL,
                           p = NULL, ci = NULL, one_sided = FALSE, sig_digits = 3) {

  approx_notes <- character(0)
  messages <- character(0)

  # Check for missing or invalid r
  if (is.null(r)) {
    cat("Insufficient information: Correlation coefficient (r) must be provided.\n")
    return(invisible(NULL))
  }

  if (any(abs(r) > 1, na.rm = TRUE)) {
    cat("Invalid input: Correlation coefficients must be between -1 and 1.\n")
    return(invisible(NULL))
  }

  estimate <- r

  # Two-sample case: need sample sizes for both groups
  if (length(estimate) == 2) {
    if (is.null(n) || length(n) != 2) {
      cat("Insufficient information: Two correlations provided but sample sizes (n) missing or incomplete.\n")
      return(invisible(NULL))
    }

    z1 <- atanh(estimate[1])
    z2 <- atanh(estimate[2])
    estimate <- z1 - z2
    se <- sqrt(1 / (n[1] - 3) + 1 / (n[2] - 3))
    df <- min(n) - 3
    approx_notes <- c(approx_notes,
                      "Estimate calculated as Fisher z-difference of two correlations.",
                      "SE derived from z difference formula. df conservatively approximated as min(n) - 3.")
  } else {
    # One-sample case

    # If no se provided, try to infer it from n, ci, or p+df
    if (is.null(se)) {
      if (!is.null(n)) {
        se <- 1 / sqrt(n - 3)
        df <- if (is.null(df)) n - 2 else df
        approx_notes <- c(approx_notes, "SE approximated using 1 / sqrt(n - 3).", "df approximated as n - 2.")
      } else if (!is.null(ci)) {
        if (length(ci) == 2 && all(abs(ci) < 1)) {
          z_ci <- atanh(ci)
          crit <- if (!is.null(df)) qt(1 - 0.05 / ifelse(one_sided, 1, 2), df)
                  else qnorm(1 - 0.05 / ifelse(one_sided, 1, 2))
          se <- (diff(range(z_ci))) / (2 * crit)
          approx_notes <- c(approx_notes, "SE approximated from CI in Fisher z scale.")
        } else {
          cat("Invalid input: CI must be a numeric vector of length 2 with values between -1 and 1.\n")
          return(invisible(NULL))
        }
      } else if (!is.null(p) && !is.null(df)) {
        stat <- if (one_sided) qt(1 - p, df) else qt(1 - p / 2, df)
        estimate <- atanh(r)
        se <- estimate / stat
        approx_notes <- c(approx_notes, "SE approximated from p-value and df.")
      } else {
        cat("Insufficient information: Provide se, n, ci, or p with df to infer missing statistics.\n")
        return(invisible(NULL))
      }
    } else {
      if (is.null(df) && !is.null(n)) {
        df <- n - 2
        approx_notes <- c(approx_notes, "df approximated as n - 2.")
      }
    }

    estimate <- atanh(r)
  }

  # Calculate test statistic and type
  stat <- estimate / se
  stat_type <- if (!is.null(df)) "t" else "z"

  # Calculate p-value if missing
  if (is.null(p)) {
    if (stat_type == "t") {
      p <- if (one_sided) 1 - pt(stat, df) else 2 * (1 - pt(abs(stat), df))
    } else {
      p <- if (one_sided) 1 - pnorm(stat) else 2 * (1 - pnorm(abs(stat)))
    }
  }

  # Confidence interval on Fisher z scale, then back-transform
  crit <- if (!is.null(df)) qt(1 - 0.05 / ifelse(one_sided, 1, 2), df)
          else qnorm(1 - 0.05 / ifelse(one_sided, 1, 2))
  ci_z <- estimate + c(-1, 1) * crit * se
  ci_r <- tanh(ci_z)

  # Final r to report
  final_r <- if (length(r) == 2) tanh(estimate) else r

  # Assemble result
  result <- c(
    r = round(final_r, sig_digits),
    se = round(se, sig_digits),
    df = if (!is.null(df)) round(df, 0) else NA,
    ci_ll = round(ci_r[1], sig_digits),
    ci_ul = round(ci_r[2], sig_digits),
    statistic = round(stat, sig_digits),
    p_value = round(p, sig_digits)
  )

  names(result)[names(result) == "statistic"] <- stat_type
  names(result)[names(result) == "p_value"] <- if (one_sided) "p-one" else "p"

  # Print any approximation notes
  if (length(approx_notes)) {
    cat("Note(s):\n", paste(approx_notes, collapse = "\n"), "\n")
  }

  return(result)
}
