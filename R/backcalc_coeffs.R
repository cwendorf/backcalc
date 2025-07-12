#' Backcalculate Missing Inferential Statistics for Regression Coefficients
#'
#' This function reconstructs inferential statistics for a regression coefficient.
#' It allows for partial input of summary or inferential statistics and infers missing values
#' such as standard errors, test statistics, confidence intervals, p-values, and degrees of freedom.
#' It supports both unstandardized and standardized coefficients.
#'
#' @param b Numeric. Unstandardized regression coefficient.
#' @param se Numeric. Standard error of the unstandardized coefficient.
#' @param std_beta Numeric. Standardized regression coefficient.
#' @param se_std Numeric. Standard error of the standardized coefficient.
#' @param sd_x,sd_y Numeric. Standard deviations of the predictor and outcome variables. Used to convert between standardized and unstandardized forms.
#' @param df Numeric. Degrees of freedom for a t-distribution. If omitted but \code{n} is provided, it is inferred as \code{df = n - 1}.
#' @param n Integer. Sample size. If \code{df} is missing, \code{df} is inferred as \code{n - 1}. If \code{df} is provided but \code{n} is missing, \code{n = df + 1}.
#' @param p Numeric. P-value associated with the test statistic.
#' @param ci Numeric vector of length 2. Confidence interval bounds (lower, upper).
#' @param statistic Numeric. Test statistic (t or z value), if already known.
#' @param one_sided Logical. Whether the hypothesis test is one-sided. Default is \code{FALSE} (two-sided).
#' @param conf.level Numeric between 0 and 1. Confidence level for the confidence interval. Default is \code{0.95}.
#' @param digits Integer. Number of decimal digits to round the output. Default is \code{3}.
#' @param attr Logical; if TRUE, attaches approximation messages as an attribute (default TRUE).
#'
#' @return
#' A \code{data.frame} with the back-calculated statistics including Estimate, SE,
#' test statistic (t or z), degrees of freedom (df), p-value, and confidence interval bounds.
#' The output has class \code{"backcalc"} and contains attribute
#' \code{"Approximations"} if \code{attr = TRUE}.
#'
#' @details
#' The function accepts a flexible combination of inputs. If sufficient information is not provided,
#' it attempts to approximate missing values where logically possible. Standardized coefficients can be
#' derived from unstandardized ones using the provided standard deviations. Missing SEs or test statistics
#' may be inferred from confidence intervals or p-values. If both \code{n} and \code{df} are missing,
#' a z-distribution is assumed.
#'
#' @examples
#' # Unstandardized beta example: coefficient and SE, z-test assumed
#' backcalc_coeffs(b = 0.5, se = 0.1)
#'
#' # Standardized beta and conversion example: unstandardized beta and SE with SDs to infer standardized beta
#' backcalc_coeffs(b = 1.1, se = 0.3, sd_x = 2.5, sd_y = 5)
#'
#' # Insufficient information example: only p-value provided, no estimate or SE (function returns NULL with warning)
#' backcalc_coeffs(p = 0.05)
#'
#' @export
backcalc_coeffs <- function(b = NULL, se = NULL,
                            std_beta = NULL, se_std = NULL,
                            sd_x = NULL, sd_y = NULL,
                            df = NULL, n = NULL,
                            p = NULL,
                            ci = NULL,
                            statistic = NULL,
                            one_sided = FALSE,
                            conf.level = 0.95,
                            digits = 3, attr = TRUE) {
  messages <- character(0)
  approx_notes <- character(0)

  # --- Handle df and n interconversion ---
  if (is.null(df) && !is.null(n)) {
    df <- n - 1
    approx_notes <- c(approx_notes, "df inferred from sample size (df = n - 1).")
  } else if (!is.null(df) && is.null(n)) {
    n <- df + 1
    approx_notes <- c(approx_notes, "Sample size inferred from df (n = df + 1).")
  }

  # --- Helper: critical value based on df and conf.level ---
  get_crit <- function(df = NULL) {
    alpha <- 1 - conf.level
    if (one_sided) {
      if (!is.null(df)) qt(1 - alpha, df) else qnorm(1 - alpha)
    } else {
      if (!is.null(df)) qt(1 - alpha / 2, df) else qnorm(1 - alpha / 2)
    }
  }

  # --- Choose estimate and SE (standardized if available) ---
  estimate <- if (!is.null(std_beta)) std_beta else b
  se_val <- if (!is.null(std_beta)) se_std else se

  # --- Infer standardized beta if possible ---
  if (is.null(std_beta) && !is.null(b) && !is.null(sd_x) && !is.null(sd_y)) {
    estimate <- b * (sd_x / sd_y)
    approx_notes <- c(approx_notes, "Standardized beta approximated from unstandardized beta and standard deviations.")
    if (!is.null(se)) {
      se_val <- se * (sd_x / sd_y)
      approx_notes <- c(approx_notes, "SE of standardized beta approximated from unstandardized SE and SDs.")
    }
  }

  # --- Infer from CI ---
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

  # --- Determine stat type: t or z ---
  stat_type <- if (!is.null(df)) "t" else "z"

  # --- Infer SE from estimate and provided statistic ---
  if (!is.null(statistic) && is.null(se_val) && !is.null(estimate)) {
    se_val <- estimate / statistic
    approx_notes <- c(approx_notes, "SE approximated from estimate and provided statistic.")
  }

  # --- Infer statistic if missing ---
  if (is.null(statistic) && !is.null(estimate) && !is.null(se_val)) {
    statistic <- estimate / se_val
  }

  # --- Infer p-value if missing ---
  if (is.null(p) && !is.null(statistic)) {
    p <- if (stat_type == "t") {
      if (one_sided) 1 - pt(statistic, df) else 2 * (1 - pt(abs(statistic), df))
    } else {
      if (one_sided) 1 - pnorm(statistic) else 2 * (1 - pnorm(abs(statistic)))
    }
    approx_notes <- c(approx_notes, "p-value computed from statistic.")
  }

  # --- Infer statistic and SE from p-value if still missing ---
  if (is.null(statistic) && !is.null(p) && !is.null(estimate)) {
    crit_val <- if (one_sided) {
      if (!is.null(df)) qt(1 - p, df) else qnorm(1 - p)
    } else {
      if (!is.null(df)) qt(1 - p / 2, df) else qnorm(1 - p / 2)
    }
    statistic <- sign(estimate) * crit_val
    approx_notes <- c(approx_notes, "Statistic approximated from p-value and estimate.")
    if (is.null(se_val)) {
      se_val <- estimate / statistic
      approx_notes <- c(approx_notes, "SE approximated from estimate and reconstructed statistic.")
    }
  }

  # --- Compute CI ---
  if (!is.null(estimate) && !is.null(se_val)) {
    crit <- get_crit(df)
    ci_lower <- estimate - crit * se_val
    ci_upper <- estimate + crit * se_val
  } else {
    ci_lower <- NA
    ci_upper <- NA
  }

  # --- Final check for minimal info ---
  if (is.null(estimate) || is.null(se_val)) {
    messages <- c(messages, "Cannot estimate coefficient or SE.")
    cat(paste0("\nInsufficient Input:"), sep = "\n")
    cat(paste0(paste(messages, collapse = "\n"), "\n\n"))
    return(invisible(NULL))
  }

  # --- Assemble result ---

result <- data.frame(
  Estimate = round(estimate, digits),
  SE = round(se_val, digits),
  Statistic = round(statistic, digits),
  df = if (!is.null(df)) round(df, 0) else NA,
  p_value = round(p, digits),
  LL = round(ci_lower, digits),
  UL = round(ci_upper, digits)
)

names(result)[3] <- stat_type
names(result)[5] <- if (one_sided) "p-one" else "p"
  rownames(result) <- "Outcome"

  class(result) <- c("backcalc", class(result))
  attr(result, "Approximations") <- approx_notes
  attr(result, "attr") <- attr

  return(result)
}