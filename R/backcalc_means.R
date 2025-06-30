#' Backcalc Missing Inferential Statistics for Means
#'
#' \code{backcalc_means()} reconstructs inferential statistics for means. It supports 
#' recovering standard errors, confidence intervals, p-values, and test statistics 
#' from partial or varied combinations of summary and inferential statistics.
#'
#' @param m Numeric or length-2 numeric vector. The point m(s) (e.g., mean for one group,
#' or two values for two groups where difference = m[1] - m[2]).
#' @param se Numeric. Standard error of the estimate.
#' @param sd Numeric or length-2 vector. Standard deviation(s) for one or two groups.
#' @param n Numeric or length-2 vector. Sample size(s) for one or two groups.
#' @param df Numeric. Degrees of freedom.
#' @param p Numeric. p-value.
#' @param ci Numeric vector of length 2. Confidence interval as c(lower, upper).
#' @param paired Logical. Whether the data are from a paired/matched design.
#' @param one_sided Logical. Whether the test is one-sided (default is two-sided).
#' @param digits Integer. Number of decimal digits to round results to (default = 3).
#'
#' @return Named numeric vector with:
#' \describe{
#'   \item{Estimate}{Point estimate (difference if two values provided)}
#'   \item{SE}{Standard error
#'   \item{t / z}{Test statistic}
#'   \item{df}{Degrees of freedom (if available)}
#'   \item{p / p-one}{Two- or one-sided p-value}
#'   \item{LL}{Lower bound of the confidence interval}
#'   \item{UL}{Upper bound of the confidence interval}
#' }
#'
#' @examples
#' # One-sample: m + SE only (z-test)
#' backcalc_means(m = 40.8, se = 1.19, digits = 3)
#' 
#' # Two-sample: Provide two ms, function calculates difference
#' backcalc_means(m = c(10.5, 7.3), sd = c(5.0, 6.0), n = c(30, 40), digits = 3)
#' 
#' # One-sample: m + p-value + df (t-test)
#' backcalc_means(m = 2.99, p = 0.045, df = 19, digits = 4)
#' 
#' # Paired-sample: m + SE + df (direct calculation)
#' backcalc_means(m = 0.15, se = 0.05, df = 39, paired = TRUE, digits = 2)
#' 
#' # Paired-sample: m + p + n (df inferred)
#' backcalc_means(m = 1.2, p = 0.03, n = 12, paired = TRUE, digits = 3)
#' 
#' # Two-sample: m + SDs + unequal ns (Welch t-test, df inferred)
#' backcalc_means(m = c(5.0, 2.6), sd = c(5.0, 6.0), n = c(30, 40), digits = 2)
#' 
#' # Two-sample: m + p + df (SE inferred)
#' backcalc_means(m = 0.32, p = 0.005, df = 58, digits = 4)
#'
#' @export
backcalc_means <- function(m = NULL, se = NULL, sd = NULL, n = NULL, df = NULL,
                           p = NULL, ci = NULL, paired = FALSE, one_sided = FALSE,
                           digits = 3) {
  estimate <- m
  messages <- character(0)
  approx_notes <- character(0)

  get_crit <- function(df = NULL) {
    alpha <- 0.05
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

  if (paired) {
    if (is.null(df) && !is.null(n)) {
      df <- n - 1
      approx_notes <- c(approx_notes, "Degrees of freedom approximated as n - 1 for paired design.")
    }
    if (is.null(df)) {
      approx_notes <- c(approx_notes, "Normal approximation used due to missing df in paired design.")
    }
  }

  two_sample_case <- (len_sd == 2 && len_n == 2 && !paired)
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

  if (!two_sample_case && is.null(se) && !is.null(sd) && !is.null(n)) {
    if (length(sd) != length(n) && length(sd) != 1 && length(n) != 1) {
      messages <- c(messages, "Length mismatch: sd and n must have equal length or one of them must be length 1.")
    } else {
      se <- sd / sqrt(n)
      approx_notes <- c(approx_notes, "SE approximated from sd and n.")
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

  if (is.null(estimate) || (is.null(se) && is.null(p) && is.null(ci))) {
    messages <- c(messages, "Insufficient input: Provide estimate and at least one of SE, p-value, or CI.")
    if (length(messages)) cat(paste(messages, collapse = "\n"), "\n")
    return(invisible(NULL))
  }

  statistic_type <- if (!is.null(df)) "t" else "z"

  if (!is.null(estimate) && !is.null(se)) {
    statistic <- estimate / se
  } else if (!is.null(p) && !is.null(estimate)) {
    crit_val <- if (one_sided) {
      if (!is.null(df)) qt(1 - p, df) else qnorm(1 - p)
    } else {
      if (!is.null(df)) qt(1 - p / 2, df) else qnorm(1 - p / 2)
    }
    statistic <- sign(estimate) * crit_val
    se <- estimate / statistic
    approx_notes <- c(approx_notes, "Test statistic and SE approximated from p-value and estimate.")
  } else {
    statistic <- NA
  }

  if (is.null(p) && !is.na(statistic)) {
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
