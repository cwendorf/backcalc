#' Backcalculate Missing Inferential Statistics for Proportions
#'
#' This function reconstructs inferential statistics related to proportions from partial information.
#' It can estimate standard errors, confidence intervals, p-values, test statistics (t or z), degrees of freedom,
#' and point estimates when some components are missing, supporting one-sample, paired, and two-sample cases.
#'
#' @param prop Numeric vector of proportion(s). For one-sample, a single value; for two-sample, a vector of length 2.
#' @param se Standard error of the estimate, if known.
#' @param n Sample size(s). A single value for one-sample tests, or a vector of length 2 for two-sample tests.
#' @param x Count(s) of "successes" (used to compute proportions). A single value or vector of length 2.
#' @param ci Confidence interval (numeric vector of length 2: lower and upper limits), if already known.
#' @param p P-value for the test statistic. If not provided, it will be computed when possible.
#' @param one_sided Logical. Is the test one-sided? Defaults to \code{FALSE} (i.e., two-sided).
#' @param digits Number of decimal places to round results. Default is 3.
#' @param interval_type Type of confidence interval. Currently unused (placeholder).
#' @param statistic Observed test statistic (z or t), if known.
#' @param df Degrees of freedom, required if using a t-statistic.
#' @param conf.level Confidence level for the confidence interval. Default is 0.95.
#' @param method Character. Either \code{"wald"} (default) for normal approximations or \code{"exact"} for exact binomial CI (one-sample only).
#' @param continuity Logical. Whether to apply continuity correction in two-sample Wald test. Default is \code{FALSE}.
#' @param attr Logical; if TRUE, attaches approximation messages as attributes (default TRUE).
#' 
#' @return
#' A \code{data.frame} with the back-calculated statistics including Estimate, SE,
#' test statistic (t or z), degrees of freedom (df), p-value, and confidence interval bounds.
#' The output has class \code{"backcalc"} and contains attribute
#' \code{"Approximations"} if \code{attr = TRUE}.
#'
#' @details
#' This function is designed to handle partial or minimal inputs by inferring missing values when possible.
#' For instance, if only a proportion and p-value are provided, it can back-calculate the sample size or test statistic.
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
                           conf.level = 0.95, method = "wald", continuity = FALSE, attr = TRUE) {
  if (!method %in% c("wald", "exact")) {
    messages <- "Invalid method. Choose 'wald' or 'exact'."
    cat(paste0("\nInsufficient Input:"), sep = "\n")
    cat(paste0(paste(messages, collapse = "\n"), "\n\n"))
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

  # One-sample exact binomial
  if (method == "exact") {
    if (is.null(x) || is.null(n) || length(x) != 1 || length(n) != 1) {
      messages <- c(messages, "Exact method requires single x and n values.")
    cat(paste0("\nInsufficient Input:"), sep = "\n")
    cat(paste0(paste(messages, collapse = "\n"), "\n\n"))
      return(invisible(NULL))
    }
    est <- x / n
    ci_exact <- binom.test(x, n, conf.level = conf.level)$conf.int
    se_exact <- (ci_exact[2] - ci_exact[1]) / (2 * qnorm(1 - alpha / 2))
    statistic_val <- (est - 0.5) / se_exact  # placeholder null of 0.5
    p_val <- binom.test(x, n)$p.value
    stat_type <- "z"

    result <- data.frame(
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
    rownames(result) <- "Outcome"

    class(result) <- c("backcalc", class(result))
    attr(result, "Approximations") <- approx_notes
    attr(result, "attr") <- attr

    if (length(messages)) {
    cat(paste0("\nInsufficient Input:"), sep = "\n")
    cat(paste0(paste(messages, collapse = "\n"), "\n\n"))
    }

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

      approx_notes <- c(approx_notes, "Standard error and confidence interval estimated from test statistic and p-value.")

      result <- data.frame(
        Estimate = round(est, digits),
        SE = round(se_calc, digits),
        statistic = round(statistic, digits),
        df = round(df, 0),
        p = round(p, digits),
        LL = round(ci_lower, digits),
        UL = round(ci_upper, digits)
      )

      rownames(result) <- "Outcome"

      class(result) <- c("backcalc", class(result))
      attr(result, "Approximations") <- approx_notes
      attr(result, "attr") <- attr

      if (length(messages)) {
    cat(paste0("\nInsufficient Input:"), sep = "\n")
    cat(paste0(paste(messages, collapse = "\n"), "\n\n"))
      }

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
        approx_notes <- c(approx_notes, "Continuity correction applied to standard error.")
      }

      if (is.null(statistic)) statistic <- est / se_calc
      if (is.null(p)) {
        if (one_sided) {
          p <- 1 - pnorm(statistic)
        } else {
          p <- 2 * (1 - pnorm(abs(statistic)))
        }
        approx_notes <- c(approx_notes, "P-value computed from estimated test statistic.")
      }

      stat_type <- "z"
      df_out <- NA

      crit <- crit_val(NULL)
      ci_lower <- est - crit * se_calc
      ci_upper <- est + crit * se_calc

      result <- data.frame(
        Estimate = round(est, digits),
        SE = round(se_calc, digits),
        statistic = round(statistic, digits),
        df = df_out,
        p = round(p, digits),
        LL = round(ci_lower, digits),
        UL = round(ci_upper, digits)
      )

      rownames(result) <- "Outcome"

      class(result) <- c("backcalc", class(result))
      attr(result, "Approximations") <- approx_notes
      attr(result, "attr") <- attr

      if (length(messages)) {
    cat(paste0("\nInsufficient Input:"), sep = "\n")
    cat(paste0(paste(messages, collapse = "\n"), "\n\n"))
      }

      return(result)
    } else {
      # One-sample
      if (is.null(prop_calc) && !is.null(x) && !is.null(n)) {
        prop_calc <- x / n
      }

      if (!is.null(prop_calc) && (is.null(n) || n <= 0)) {
        p0 <- 0.5  # Null hypothesis proportion
        if (!is.null(statistic)) {
          n <- prop_calc * (1 - prop_calc) / ((prop_calc - p0) / statistic)^2
          n <- round(n)
          messages <- c(messages, paste0("Inferred n = ", n, " from prop and statistic"))
          approx_notes <- c(approx_notes, "Sample size inferred from statistic assuming null proportion = 0.5.")
        } else if (!is.null(p)) {
          stat_val <- ifelse(one_sided, qnorm(1 - p), qnorm(1 - p / 2))
          stat_val <- sign(prop_calc - p0) * abs(stat_val)
          n <- prop_calc * (1 - prop_calc) / ((prop_calc - p0) / stat_val)^2
          n <- round(n)
          messages <- c(messages, paste0("Inferred n = ", n, " from prop and p-value"))
          approx_notes <- c(approx_notes, "Sample size inferred from p-value assuming null proportion = 0.5.")
        }
      }

      if (is.null(prop_calc) || is.null(n)) {
        messages <- c(messages, "Provide 'prop' or ('x' and 'n').")
    cat(paste0("\nInsufficient Input:"), sep = "\n")
    cat(paste0(paste(messages, collapse = "\n"), "\n\n"))
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
        approx_notes <- c(approx_notes, "P-value computed from estimated test statistic.")
      }

      stat_type <- "z"
      df_out <- NA

      crit <- crit_val(NULL)
      ci_lower <- est - crit * se_calc
      ci_upper <- est + crit * se_calc

      result <- data.frame(
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
      rownames(result) <- "Outcome"

      class(result) <- c("backcalc", class(result))
      attr(result, "Approximations") <- approx_notes
      attr(result, "attr") <- attr

      return(result)
    }
  }

  messages <- c(messages, "Unsupported combination of inputs or method.")
    cat(paste0("\nInsufficient Input:"), sep = "\n")
    cat(paste0(paste(messages, collapse = "\n"), "\n\n"))
  return(invisible(NULL))
}
