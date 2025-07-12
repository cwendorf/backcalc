#' Backcalculate Inferential Statistics for Ratio Measures
#'
#' This function reconstructs inferential statistics (e.g., SE, test statistic,
#' p-value, confidence interval) for ratio-type measures (such as odds ratios or risk ratios)
#' using the log transformation internally. The function allows flexible input and determines
#' the appropriate test type (z or t) based on the presence of degrees of freedom.
#'
#' @param ratio Numeric scalar (for one group) or numeric vector of length 2 (for comparison).
#'   When a vector of two ratios is supplied, the function compares them via their log difference.
#' @param se Numeric. Standard error on the log scale. Can be a scalar or length 2 (for two-group case).
#' @param n Integer. Sample size (used to infer degrees of freedom if \code{df} not given).
#' @param df Numeric. Degrees of freedom. Scalar (for t-test) or length 2 (for Welch’s approximation).
#' @param statistic Numeric. t or z test statistic. Used to infer SE if not supplied.
#' @param p Numeric. p-value (one-sided or two-sided). Used only for display if provided.
#' @param ci Numeric vector of length 2. Confidence interval (on the ratio scale). Used to infer SE if SE not provided.
#' @param one_sided Logical. Whether the hypothesis test is one-sided. Default is \code{FALSE}.
#' @param digits Integer. Number of digits to round outputs to. Default is \code{3}.
#' @param conf.level Numeric. Confidence level used to compute interval. Default is \code{0.95}.
#' @param attr Logical; if TRUE, attaches approximation messages as attributes (default TRUE).
#'
#' @return
#' A \code{data.frame} with the back-calculated statistics including Estimate, SE,
#' test statistic (t or z), degrees of freedom (df), p-value, and confidence interval bounds.
#' The output has class \code{"backcalc"} and contains attribute
#' \code{"Approximations"} if \code{attr = TRUE}.
#'
#' @details
#' This function works on the log-transformed scale of ratios. It supports partial information
#' input and will infer missing values when possible. In the two-sample case, if both SEs and
#' dfs are provided, a Welch–Satterthwaite approximation is used to compute the test df.
#'
#' If insufficient information is provided (e.g., no SE, CI, or statistic), the function will halt.
#' When possible, missing pieces are backcalculated using available data.
#'
#' @examples
#' # One-sample example: Ratio and SE provided (no inference needed)
#' backcalc_ratios(ratio = 2.5, se = 0.2)
#'
#' # Two-sample example: Ratios, SEs, and dfs provided (Welch-Satterthwaite df calculation)
#' backcalc_ratios(ratio = c(3.2, 1.9), se = c(0.25, 0.15), df = c(25, 30))
#'
#' # Insufficient information example: Ratio provided but no SE, statistic, or CI
#' backcalc_ratios(ratio = 2.3)
#'
#' @export
backcalc_ratios <- function(ratio = NULL, se = NULL, n = NULL, df = NULL,
                            p = NULL, ci = NULL, statistic = NULL,
                            one_sided = FALSE, digits = 3, conf.level = 0.95, attr = TRUE) {

  messages <- character(0)
  approx_notes <- character(0)

  # Validate ratio input
  if (is.null(ratio)) {
    messages <- c(messages, "A ratio (or two ratios) must be provided.")
  } else if (!is.numeric(ratio) || any(is.na(ratio)) || any(ratio <= 0)) {
    messages <- c(messages, "Ratios must be positive numeric values without NA.")
  }

  get_crit <- function(df_local = NULL) {
    alpha <- 1 - conf.level
    if (!is.null(df_local)) {
      qt(1 - alpha / ifelse(one_sided, 1, 2), df_local)
    } else {
      qnorm(1 - alpha / ifelse(one_sided, 1, 2))
    }
  }

  # Early exit if fatal input issue
  if (length(messages) > 0) {
    cat(paste0("\nInsufficient Input:"), sep = "\n")
    cat(paste0(paste(messages, collapse = "\n"), "\n\n"))
    return(invisible(NULL))
  }

  # Infer df from n if not provided
  if (is.null(df) && !is.null(n)) {
    if (length(n) == 1L) {
      df <- n - 1
      approx_notes <- c(approx_notes, "df approximated as n - 1.")
    } else if (length(n) == 2L && length(ratio) == 2L) {
      df <- n - 1
      approx_notes <- c(approx_notes, "df vector approximated as n - 1 for each group.")
    }
  }

  # Handle two-ratio case
  if (length(ratio) == 2) {
    estimate <- log(ratio[1]) - log(ratio[2])
    approx_notes <- c(approx_notes, "Estimate calculated as log ratio difference between two ratios.")

    if (!is.null(se)) {
      if (length(se) == 2) {
        se <- sqrt(se[1]^2 + se[2]^2)
        approx_notes <- c(approx_notes, "SE combined using sqrt(se1^2 + se2^2).")
      } else if (length(se) != 1) {
        messages <- c(messages, "SE length inconsistent with ratio length.")
      }
    }

    if (!is.null(df) && length(df) == 2 && length(se) == 1 && length(se) != length(df)) {
      approx_notes <- c(approx_notes, "df vector provided but SE of difference only; df not adjusted.")
      df <- sum(df) / 2
    }

  } else {
    estimate <- log(ratio)
  }

  if (!is.null(ci) && length(ci) == 2 && is.null(se)) {
    if (all(ci > 0)) {
      crit <- get_crit(df)
      se <- abs(log(ci[2]) - log(ci[1])) / (2 * crit)
      approx_notes <- c(approx_notes, "SE approximated from CI using log scale and critical value.")
    } else {
      messages <- c(messages, "Confidence interval values must be positive for log transformation. Ignoring CI.")
    }
  }

  if (is.null(se) && !is.null(statistic)) {
    se <- abs(estimate / statistic)
    approx_notes <- c(approx_notes, "SE approximated from statistic and estimate.")
  }

  if (is.null(se)) {
    messages <- c(messages, "SE or CI must be provided or inferable.")
  }

  # Final check: print and exit if any fatal issues were recorded
  if (length(messages) > 0) {
    cat(paste0("\nInsufficient Input:"), sep = "\n")
    cat(paste0(paste(messages, collapse = "\n"), "\n\n"))
    return(invisible(NULL))
  }

  stat_type <- if (!is.null(df)) "t" else "z"

  if (is.null(p)) {
    if (stat_type == "t") {
      p <- if (one_sided) 1 - pt(estimate / se, df) else 2 * (1 - pt(abs(estimate / se), df))
    } else {
      p <- if (one_sided) 1 - pnorm(estimate / se) else 2 * (1 - pnorm(abs(estimate / se)))
    }
  }

  if (is.null(statistic)) {
    statistic <- estimate / se
  }

  crit <- get_crit(df)
  ci_lower <- exp(estimate - crit * se)
  ci_upper <- exp(estimate + crit * se)
  estimate_exp <- exp(estimate)

  result <- data.frame(
    Estimate = round(estimate_exp, digits),
    SE = round(se, digits),
    statistic = round(statistic, digits),
    df = if (!is.null(df)) round(df, 0) else NA,
    p = round(p, digits),
    LL = round(ci_lower, digits),
    UL = round(ci_upper, digits)
  )

  names(result)[names(result) == "statistic"] <- stat_type
  names(result)[names(result) == "p"] <- if (one_sided) "p-one" else "p"
  rownames(result) <- "Outcome"

  class(result) <- c("backcalc", class(result))
  attr(result, "Approximations") <- approx_notes
  attr(result, "attr") <- attr

  return(result)
}
