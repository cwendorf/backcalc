#' Backcalculate Missing Inferential Statistics for Proportions
#'
#' \code{backcalc_means()} reconstructs inferential statistics related to proportions from partial information.
#' It can estimate standard errors, confidence intervals, p-values, test statistics (t or z), degrees of freedom,
#' and point estimates when some components are missing, supporting one-sample, paired, and two-sample cases.
#'
#' @param prop Numeric vector of proportions. Can be length 1 or 2 for one- or two-sample cases.
#' @param se Numeric standard error (not used directly here but for future extensions).
#' @param n Numeric vector of sample sizes. Length 1 or 2 matching the number of groups.
#' @param x Numeric vector of counts of successes/events. Length 1 or 2 for one- or two-sample.
#' @param ci Logical or numeric, confidence interval level (default 0.95).
#' @param p Numeric p-value associated with the test statistic (optional).
#' @param one_sided Logical, whether to use one-sided test and confidence interval (default FALSE).
#' @param digits Integer, number of decimal places to round results (default 3).
#' @param interval_type Character string indicating the type of interval (currently only "wald" supported).
#' @param statistic Numeric test statistic value (optional).
#' @param df Numeric degrees of freedom for t-distribution (optional).
#' @param conf.level Numeric confidence level (default 0.95).
#' @param method Character string specifying the method: "wald", "exact", "rr", or "or".
#' @param continuity Logical, whether to apply continuity correction in Wald method (default FALSE).
#'
#' @details
#' The function supports:
#' \itemize{
#'   \item One- and two-sample proportion comparisons via Wald method.
#'   \item Exact binomial confidence intervals for single proportions.
#'   \item Risk ratios ("rr") and odds ratios ("or") for two groups with counts.
#' }
#' When insufficient inputs are provided, a warning is issued and the function returns invisibly NULL.
#'
#' @return
#' A named numeric vector with the following elements:
#' \describe{
#'   \item{Estimate}{Estimate of the proportion, difference, log risk ratio, or log odds ratio.}
#'   \item{SE}{Standard error of the estimate.}
#'   \item{z or t}{Test statistic (z or t) depending on method and inputs.}
#'   \item{df}{Degrees of freedom (if applicable, otherwise NA).}
#'   \item{p or p-one}{P-value, adjusted for one-sided test if requested.}
#'   \item{LL}{Lower confidence limit.}
#'   \item{UL}{Upper confidence limit.}
#' }
#'
#' @examples
#' # One-sample: Provide proportion and sample size only (basic one-sample proportion)
#' backcalc_props(prop = 0.4, n = 100)
#'
#' # Two-sample: Provide two proportions and sample sizes only (difference in proportions)
#' backcalc_props(prop = c(0.55, 0.4), n = c(150, 130))
#'
#' # Insufficient info: Provide n only, no prop, counts, or se
#' backcalc_props(n = 100)
#'
#' @export
backcalc_props <- function(prop = NULL, se = NULL, n = NULL, x = NULL, ci = NULL,
                           p = NULL, one_sided = FALSE, digits = 3,
                           interval_type = "wald", statistic = NULL, df = NULL,
                           conf.level = 0.95, method = "wald", continuity = FALSE) {
  if (!method %in% c("wald", "exact", "rr", "or")) {
    cat("Invalid method. Choose one of 'wald', 'exact', 'rr', or 'or'.\n")
    return(invisible(NULL))
  }

  alpha <- 1 - conf.level
  crit_val <- function(df = NULL) {
    if (!is.null(df)) {
      if (one_sided) qt(1 - alpha, df) else qt(1 - alpha / 2, df)
    } else {
      if (one_sided) qnorm(1 - alpha) else qnorm(1 - alpha / 2)
    }
  }

  messages <- character(0)
  approx_notes <- character(0)

  # Calculate proportions from counts if needed
  if (!is.null(x) && !is.null(n)) {
    prop_calc <- x / n
  } else {
    prop_calc <- prop
  }

  # Handle one or two samples
  two_sample <- !is.null(prop_calc) && length(prop_calc) == 2 && !is.null(n) && length(n) == 2

  # Validate counts for rr/or
  if (method %in% c("rr", "or")) {
    if (is.null(x) || is.null(n) || length(x) != 2 || length(n) != 2) {
      cat("For method 'rr' and 'or', x and n must be numeric vectors of length 2.\n")
      return(invisible(NULL))
    }
  }

  # One-sample exact binomial
  if (method == "exact") {
    if (is.null(x) || is.null(n) || length(x) != 1 || length(n) != 1) {
      cat("Exact method requires single x and n values.\n")
      return(invisible(NULL))
    }
    est <- x / n
    ci_exact <- binom.test(x, n, conf.level = conf.level)$conf.int
    se_exact <- (ci_exact[2] - ci_exact[1]) / (2 * qnorm(1 - alpha / 2))
    statistic_val <- (est - 0.5) / se_exact  # placeholder null of 0.5
    p_val <- binom.test(x, n)$p.value
    stat_type <- "z"

    result <- c(
      Estimate = round(est, digits),
      SE = round(se_exact, digits),
      statistic = round(statistic_val, digits),
      df = NA,
      p = round(p_val, digits),
      LL = round(ci_exact[1], digits),
      UL = round(ci_exact[2], digits)
    )

    names(result)[names(result) == "statistic"] <- stat_type
    names(result)[names(result) == "p"] <- ifelse(one_sided, "p-one", "p")

    if (length(messages)) cat(paste(messages, collapse = "\n"), "\n")
    if (length(approx_notes)) cat("Note(s):\n", paste(approx_notes, collapse = "\n"), "\n", sep = "")

    return(result)
  }

  # Wald method
  if (method == "wald") {
    # Case: prop + p + df (no n or x)
    if (!is.null(prop_calc) && length(prop_calc) == 2 && !is.null(p) && !is.null(df)) {
      est <- prop_calc[1] - prop_calc[2]
      stat_type <- "t"

      if (one_sided) {
        statistic <- qt(1 - p, df = df)
      } else {
        statistic <- qt(1 - p / 2, df = df)
      }

      statistic <- sign(est) * abs(statistic)
      se_calc <- abs(est / statistic)

      crit <- crit_val(df)
      ci_lower <- est - crit * se_calc
      ci_upper <- est + crit * se_calc

      result <- c(
        Estimate = round(est, digits),
        SE = round(se_calc, digits),
        statistic = round(statistic, digits),
        df = round(df, 0),
        p = round(p, digits),
        LL = round(ci_lower, digits),
        UL = round(ci_upper, digits)
      )

      names(result)[names(result) == "statistic"] <- stat_type
      names(result)[names(result) == "p"] <- ifelse(one_sided, "p-one", "p")

      if (length(messages)) cat(paste(messages, collapse = "\n"), "\n")
      if (length(approx_notes)) cat("Note(s):\n", paste(approx_notes, collapse = "\n"), "\n", sep = "")

      return(result)
    }

    if (two_sample) {
      p1 <- prop_calc[1]
      p2 <- prop_calc[2]
      n1 <- n[1]
      n2 <- n[2]

      if (continuity) {
        cc <- 0.5 / n1 + 0.5 / n2
      } else {
        cc <- 0
      }

      est <- p1 - p2
      se_calc <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)

      if (continuity) {
        se_calc <- sqrt(se_calc^2 + cc)
      }

      if (is.null(statistic)) statistic <- est / se_calc
      if (is.null(p)) {
        if (one_sided) {
          p <- 1 - pnorm(statistic)
        } else {
          p <- 2 * (1 - pnorm(abs(statistic)))
        }
      }

      stat_type <- "z"
      df_out <- NA

      crit <- crit_val(NULL)
      ci_lower <- est - crit * se_calc
      ci_upper <- est + crit * se_calc

      result <- c(
        Estimate = round(est, digits),
        SE = round(se_calc, digits),
        statistic = round(statistic, digits),
        df = df_out,
        p = round(p, digits),
        LL = round(ci_lower, digits),
        UL = round(ci_upper, digits)
      )

      names(result)[names(result) == "statistic"] <- stat_type
      names(result)[names(result) == "p"] <- ifelse(one_sided, "p-one", "p")

      if (length(messages)) cat(paste(messages, collapse = "\n"), "\n")
      if (length(approx_notes)) cat("Note(s):\n", paste(approx_notes, collapse = "\n"), "\n", sep = "")

      return(result)
    } else {
      # One-sample
      if (is.null(prop_calc) && !is.null(x) && !is.null(n)) {
        prop_calc <- x / n
      }

      # --- NEW: infer n if missing and possible ---
      if (!is.null(prop_calc) && (is.null(n) || n <= 0)) {
        p0 <- 0.5  # Null hypothesis proportion; you could make this a param
        if (!is.null(statistic)) {
          n <- prop_calc * (1 - prop_calc) / ((prop_calc - p0) / statistic)^2
          n <- round(n)
          messages <- c(messages, paste0("Inferred n = ", n, " from prop and statistic"))
        } else if (!is.null(p)) {
          stat_val <- ifelse(one_sided, qnorm(1 - p), qnorm(1 - p / 2))
          stat_val <- sign(prop_calc - p0) * abs(stat_val)
          n <- prop_calc * (1 - prop_calc) / ((prop_calc - p0) / stat_val)^2
          n <- round(n)
          messages <- c(messages, paste0("Inferred n = ", n, " from prop and p-value"))
        }
      }
      # ------------------------

      if (is.null(prop_calc) || is.null(n)) {
        cat("Insufficient input: Provide 'prop' or ('x' and 'n').\n")
        return(invisible(NULL))
      }

      est <- prop_calc
      se_calc <- sqrt(est * (1 - est) / n)

      if (is.null(statistic)) statistic <- est / se_calc
      if (is.null(p)) {
        if (one_sided) {
          p <- 1 - pnorm(statistic)
        } else {
          p <- 2 * (1 - pnorm(abs(statistic)))
        }
      }

      stat_type <- "z"
      df_out <- NA

      crit <- crit_val(NULL)
      ci_lower <- est - crit * se_calc
      ci_upper <- est + crit * se_calc

      result <- c(
        Estimate = round(est, digits),
        SE = round(se_calc, digits),
        statistic = round(statistic, digits),
        df = df_out,
        p = round(p, digits),
        LL = round(ci_lower, digits),
        UL = round(ci_upper, digits)
      )

      names(result)[names(result) == "statistic"] <- stat_type
      names(result)[names(result) == "p"] <- ifelse(one_sided, "p-one", "p")

      if (length(messages)) cat(paste(messages, collapse = "\n"), "\n")
      if (length(approx_notes)) cat("Note(s):\n", paste(approx_notes, collapse = "\n"), "\n", sep = "")

      return(result)
    }
  }

  # Risk ratio (log scale)
  if (method == "rr") {
    p1 <- x[1] / n[1]
    p2 <- x[2] / n[2]
    est <- log(p1 / p2)
    se_calc <- sqrt((1 - p1) / x[1] + (1 - p2) / x[2])
    if (is.null(statistic)) statistic <- est / se_calc
    if (is.null(p)) {
      if (one_sided) {
        p <- 1 - pnorm(statistic)
      } else {
        p <- 2 * (1 - pnorm(abs(statistic)))
      }
    }

    stat_type <- "z"
    df_out <- NA

    crit <- crit_val(NULL)
    ci_lower <- est - crit * se_calc
    ci_upper <- est + crit * se_calc

    result <- c(
      Estimate = round(est, digits),
      SE = round(se_calc, digits),
      statistic = round(statistic, digits),
      df = df_out,
      p = round(p, digits),
      LL = round(ci_lower, digits),
      UL = round(ci_upper, digits)
    )

    names(result)[names(result) == "statistic"] <- stat_type
    names(result)[names(result) == "p"] <- ifelse(one_sided, "p-one", "p")

    if (length(messages)) cat(paste(messages, collapse = "\n"), "\n")
    if (length(approx_notes)) cat("Note(s):\n", paste(approx_notes, collapse = "\n"), "\n", sep = "")

    return(result)
  }

  # Odds ratio (log scale)
  if (method == "or") {
    a <- x[1]
    b <- n[1] - x[1]
    c <- x[2]
    d <- n[2] - x[2]
    est <- log((a * d) / (b * c))
    se_calc <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
    if (is.null(statistic)) statistic <- est / se_calc
    if (is.null(p)) {
      if (one_sided) {
        p <- 1 - pnorm(statistic)
      } else {
        p <- 2 * (1 - pnorm(abs(statistic)))
      }
    }

    stat_type <- "z"
    df_out <- NA

    crit <- crit_val(NULL)
    ci_lower <- est - crit * se_calc
    ci_upper <- est + crit * se_calc

    result <- c(
      Estimate = round(est, digits),
      SE = round(se_calc, digits),
      statistic = round(statistic, digits),
      df = df_out,
      p = round(p, digits),
      LL = round(ci_lower, digits),
      UL = round(ci_upper, digits)
    )

    names(result)[names(result) == "statistic"] <- stat_type
    names(result)[names(result) == "p"] <- ifelse(one_sided, "p-one", "p")

    if (length(messages)) cat(paste(messages, collapse = "\n"), "\n")
    if (length(approx_notes)) cat("Note(s):\n", paste(approx_notes, collapse = "\n"), "\n", sep = "")

    return(result)
  }

  cat("Unsupported combination of inputs or method.\n")
  return()
}
