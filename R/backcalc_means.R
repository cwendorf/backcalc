#' Backcalculate Missing Inferential Statistics for Means
#'
#' \code{backcalc_means()} reconstructs inferential statistics related to means from partial information.
#' It can estimate standard errors, confidence intervals, p-values, test statistics (t or z), degrees of freedom,
#' and point estimates when some components are missing, supporting one-sample, paired, and two-sample cases.
#'
#' @param m Numeric or length-2 numeric vector. Point estimate(s) (e.g., mean for one group,
#' or two values for two groups where difference = m[1] - m[2]).
#' @param se Numeric. Standard error of the estimate.
#' @param sd Numeric or length-2 numeric vector. Standard deviation(s) for one or two groups.
#' @param n Numeric or length-2 numeric vector. Sample size(s) for one or two groups.
#' @param df Numeric. Degrees of freedom.
#' @param p Numeric. P-value.
#' @param ci Numeric vector of length 2. Confidence interval as c(lower, upper).
#' @param paired Logical. Whether data come from a paired/matched design (default FALSE).
#' @param one_sided Logical. Whether the test is one-sided (default FALSE).
#' @param digits Integer. Number of decimal digits to round the results (default 3).
#' @param statistic Numeric. Test statistic value (t or z).
#' @param conf.level Numeric. Confidence level for intervals (default 0.95).
#'
#' @return Named numeric vector containing:
#' \describe{
#'   \item{Estimate}{Point estimate (difference if two values provided)}
#'   \item{SE}{Standard error of estimate}
#'   \item{t / z}{Test statistic (t if df provided, otherwise z)}
#'   \item{df}{Degrees of freedom (if available)}
#'   \item{p / p-one}{Two-sided or one-sided p-value}
#'   \item{LL}{Lower bound of the confidence interval}
#'   \item{UL}{Upper bound of the confidence interval}
#' }
#'
#' @details
#' The function infers missing statistics using provided inputs. For two-sample tests,
#' it calculates standard error and degrees of freedom using pooled or Welch's approximation as appropriate.
#' When sample sizes are provided, it defaults to using the t-distribution for inference rather than normal approximation.
#' The confidence interval is computed based on the specified \code{conf.level}.
#'
#' @examples
#' # One-sample example: Mean, SE, and sample size given (uses t-distribution)
#' backcalc_means(m = 25.4, se = 2.1, n = 30)
#' 
#' # Two-sample example: Means, SDs, and sample sizes given (Welch t-test inference)
#' backcalc_means(m = c(15, 12), sd = c(4, 5), n = c(40, 35))
#' 
#' # Insufficient input example: Mean provided but no SE, p-value, or CI (warns user)
#' backcalc_means(m = 25)
#'
#' @export
backcalc_means <- function(m = NULL, se = NULL, sd = NULL, n = NULL, df = NULL,
                           p = NULL, ci = NULL, statistic = NULL,
                           paired = FALSE, one_sided = FALSE,
                           conf.level = 0.95, digits = 3) {
  estimate <- m
  messages <- character(0)
  approx_notes <- character(0)

  get_crit <- function(df = NULL) {
    alpha <- 1 - conf.level
    if (one_sided) {
      if (!is.null(df)) qt(1 - alpha, df) else qnorm(1 - alpha)
    } else {
      if (!is.null(df)) qt(1 - alpha / 2, df) else qnorm(1 - alpha / 2)
    }
  }

  if (!is.null(estimate) && length(estimate) == 2) {
    estimate <- estimate[1] - estimate[2]
  }

  len_sd <- ifelse(is.null(sd), 0, length(sd))
  len_n <- ifelse(is.null(n), 0, length(n))

  # Handle special case: two means + two SDs + single n => assume equal sample sizes
  two_sample_case <- (len_sd == 2 && len_n == 2 && !paired)
  if (!two_sample_case && len_sd == 2 && len_n == 1 && !paired && !is.null(m) && length(m) == 2) {
    n <- rep(n, 2)
    len_n <- 2
    approx_notes <- c(approx_notes, "Assumed equal sample sizes for both groups.")
    two_sample_case <- TRUE
  }

  if (paired) {
    if (is.null(df) && !is.null(n)) {
      df <- n - 1
      approx_notes <- c(approx_notes, "Degrees of freedom approximated as n - 1 for paired design.")
    }
    if (is.null(df)) {
      approx_notes <- c(approx_notes, "Normal approximation used due to missing df in paired design.")
    }
  }

  if (two_sample_case) {
    var1 <- sd[1]^2
    var2 <- sd[2]^2
    n1 <- n[1]
    n2 <- n[2]
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
  }

  if (!two_sample_case && is.null(se) && !is.null(sd)) {
    if (is.null(n)) {
      messages <- c(messages, "Cannot compute SE from SD without sample size (n).")
    } else if (length(sd) != length(n) && length(sd) != 1 && length(n) != 1) {
      messages <- c(messages, "Length mismatch: sd and n must have equal length or one of them must be length 1.")
    } else {
      se <- sd / sqrt(n)
      approx_notes <- c(approx_notes, "SE approximated from sd and n.")
    }
  }

  # FIX: Infer df if SE has been computed and df still missing
  if (is.null(df) && !is.null(n)) {
    if (length(n) == 1) {
      df <- n - 1
      approx_notes <- c(approx_notes, "Degrees of freedom approximated as n - 1.")
    } else if (length(n) == 2) {
      df <- n[1] + n[2] - 2
      approx_notes <- c(approx_notes, "Degrees of freedom approximated as n1 + n2 - 2.")
    }
  }

  if (!is.null(ci)) {
    if (length(ci) != 2) {
      messages <- c(messages, "Confidence interval must be length 2.")
    } else {
      if (is.null(estimate)) estimate <- mean(ci)
      if (is.null(se)) {
        crit <- get_crit(df)
        se <- abs(ci[2] - ci[1]) / (2 * crit)
        approx_notes <- c(approx_notes, "SE approximated from CI width.")
      }
    }
  }

  if (!is.null(statistic) && !is.null(se) && is.null(estimate)) {
    estimate <- statistic * se
    approx_notes <- c(approx_notes, "Estimate approximated from test statistic and SE.")
  }

  if (!is.null(statistic) && !is.null(estimate) && is.null(se)) {
    se <- estimate / statistic
    approx_notes <- c(approx_notes, "SE approximated from test statistic and estimate.")
  }

  if (is.null(estimate) || (is.null(se) && is.null(p) && is.null(ci) && is.null(statistic))) {
    messages <- c(messages, "Insufficient input: Provide estimate and at least one of SE, p-value, CI, or test statistic.")
    if (length(messages)) cat(paste(messages, collapse = "\n"), "\n")
    return(invisible(NULL))
  }

  statistic_type <- if (!is.null(df)) "t" else "z"

  if (is.null(statistic) && !is.null(estimate) && !is.null(se)) {
    statistic <- estimate / se
  } else if (is.null(statistic) && !is.null(p) && !is.null(estimate)) {
    crit_val <- if (one_sided) {
      if (!is.null(df)) qt(1 - p, df) else qnorm(1 - p)
    } else {
      if (!is.null(df)) qt(1 - p / 2, df) else qnorm(1 - p / 2)
    }
    statistic <- sign(estimate) * crit_val
    se <- estimate / statistic
    approx_notes <- c(approx_notes, "Test statistic and SE approximated from p-value and estimate.")
  }

  if (is.null(p) && !is.null(statistic)) {
    if (statistic_type == "t") {
      p <- if (one_sided) 1 - pt(statistic, df) else 2 * (1 - pt(abs(statistic), df))
    } else {
      p <- if (one_sided) 1 - pnorm(statistic) else 2 * (1 - pnorm(abs(statistic)))
    }
  }

  crit <- get_crit(df)
  ci_lower <- estimate - crit * se
  ci_upper <- estimate + crit * se

  result <- c(
    Estimate = round(estimate, digits),
    SE = round(se, digits),
    statistic = round(statistic, digits),
    df = if (!is.null(df)) round(df, 0) else NA,
    p = round(p, digits),
    LL = round(ci_lower, digits),
    UL = round(ci_upper, digits)
  )

  names(result)[names(result) == "p"] <- ifelse(one_sided, "p-one", "p")
  names(result)[names(result) == "statistic"] <- statistic_type

  if (length(messages)) cat(paste(messages, collapse = "\n"), "\n")
  if (length(approx_notes)) cat("Note(s):\n", paste(approx_notes, collapse = "\n"), "\n", sep = "")

  return(result)
}
