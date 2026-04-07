#' Backcalculate Missing Inferential Statistics for Multiple Regression Coefficients
#'
#' This function attempts to infer regression coefficients, standard errors, test statistics,
#' p-values, and confidence intervals for multiple regression predictors given partial input.
#' It supports a variety of input forms, including unstandardized coefficients (`b` and `se`),
#' standardized betas (`std_beta` and `se_std`), confidence intervals, p-values, and statistics.
#' It can also standardize coefficients if standard deviations of predictors (`sd_x`) and outcome (`sd_y`) are provided.
#'
#' The function returns a data.frame with rows for the intercept and each predictor, and columns:
#' \code{Estimate}, \code{SE}, \code{Statistic}, \code{p}, \code{LL}, and \code{UL}.
#' Approximation notes describing inferred values are attached as an attribute and printed by a
#' compatible print method.
#'
#' @param intercept Numeric, estimate of the intercept.
#' @param intercept_se Numeric, standard error of the intercept.
#' @param b Numeric vector of unstandardized regression coefficients for predictors.
#' @param se Numeric vector of standard errors corresponding to \code{b}.
#' @param std_beta Numeric vector of standardized regression coefficients.
#' @param se_std Numeric vector of standard errors for standardized coefficients.
#' @param sd_x Numeric vector of standard deviations of predictor variables (used for standardizing).
#' @param sd_y Numeric scalar, standard deviation of outcome variable.
#' @param df Numeric degrees of freedom for t-distribution; if NULL, normal distribution assumed.
#' @param n Numeric sample size; used to infer df if df not provided.
#' @param p Numeric vector of p-values.
#' @param ci List of numeric vectors of length 2 giving confidence intervals for predictors.
#' @param statistic Numeric vector of test statistics (t or z).
#' @param one_sided Logical, whether p-values/statistics are one-sided (default FALSE).
#' @param conf.level Numeric confidence level for intervals (default 0.95).
#' @param digits Integer, number of decimal places for output rounding (default 3).
#' @param attr Logical, whether to attach approximation notes as an attribute (default TRUE).
#'
#' @return A \code{data.frame} with rows for \code{Intercept} and predictors named \code{X1, X2, ...}.
#' Columns include \code{Estimate}, \code{SE}, \code{Statistic}, \code{df}, \code{p}, \code{LL}, and \code{UL}.
#' Approximation notes are attached as the \code{"Approximations"} attribute.
#'
#' @details
#' The function tries to infer missing values using various rules:
#' - Standardized betas can be computed from unstandardized betas and SDs.
#' - SEs can be approximated from confidence intervals.
#' - Test statistics and p-values can be derived from each other.
#' - Confidence intervals can be reconstructed from estimates and SEs.
#' 
#' If insufficient information is provided to compute estimates or SEs, the function prints a message and returns \code{NULL}.
#'
#' @examples
#' # Example 1: Intercept plus unstandardized betas with SEs and sample size
#' backcalc_multreg(
#'   intercept = 2.5, intercept_se = 0.4,
#'   b = c(0.7, -0.3), se = c(0.15, 0.1),
#'   n = 100
#' )
#'
#' # Example 2: Standardized betas with SEs and degrees of freedom
#' backcalc_multreg(
#'   intercept = 1.2, intercept_se = 0.3,
#'   std_beta = c(0.25, 0.4), se_std = c(0.05, 0.07),
#'   df = 50
#' )
#'
#' # Example 3: Confidence intervals for predictors and sample size
#' backcalc_multreg(
#'   intercept = 3.0, intercept_se = 0.5,
#'   ci = list(c(0.1, 0.5), c(-0.2, 0.4)),
#'   n = 80
#' )
#'
#' @export
backcalc_multreg <- function(
  intercept = NULL, intercept_se = NULL,
  b = NULL, se = NULL,
  std_beta = NULL, se_std = NULL,
  sd_x = NULL, sd_y = NULL,
  df = NULL, n = NULL,
  p = NULL,
  ci = NULL,
  statistic = NULL,
  one_sided = FALSE,
  conf.level = 0.95,
  digits = 3,
  attr = TRUE
) {
  # Helper to get critical value for CI
  get_crit <- function(df = NULL) {
    alpha <- 1 - conf.level
    if (one_sided) {
      if (!is.null(df)) qt(1 - alpha, df) else qnorm(1 - alpha)
    } else {
      if (!is.null(df)) qt(1 - alpha/2, df) else qnorm(1 - alpha/2)
    }
  }
  
  messages <- character(0)
  
  # Number of predictors
  n_preds <- max(length(b), length(se), length(std_beta), length(se_std), length(p), length(statistic))
  n_preds <- max(n_preds, ifelse(is.null(ci), 0, length(ci)))
  
  var_names <- c("Intercept", paste0("X", seq_len(n_preds)))
  
  # Ensure vectors have correct length or NULL
  extend_vec <- function(x, n) {
    if (is.null(x)) return(rep(NA_real_, n))
    if (length(x) == 1 && n > 1) return(rep(x, n))
    if (length(x) < n) return(c(x, rep(NA_real_, n - length(x))))
    x[1:n]
  }
  
  b <- extend_vec(b, n_preds)
  se <- extend_vec(se, n_preds)
  std_beta <- extend_vec(std_beta, n_preds)
  se_std <- extend_vec(se_std, n_preds)
  p <- extend_vec(p, n_preds)
  statistic <- extend_vec(statistic, n_preds)
  
  if (is.null(ci)) {
    ci <- replicate(n_preds, c(NA_real_, NA_real_), simplify = FALSE)
  } else if (length(ci) < n_preds) {
    # Pad with NA CIs
    ci <- c(ci, replicate(n_preds - length(ci), c(NA_real_, NA_real_), simplify = FALSE))
  }
  
  # SDs for predictors and outcome (for standardization)
  if (is.null(sd_x)) sd_x <- rep(NA_real_, n_preds)
  if (length(sd_x) == 1 && n_preds > 1) sd_x <- rep(sd_x, n_preds)
  if (length(sd_x) < n_preds) sd_x <- c(sd_x, rep(NA_real_, n_preds - length(sd_x)))
  
  if (is.null(sd_y)) sd_y <- NA_real_
  
  # Handle df and n interconversion
  if (is.null(df) && !is.null(n)) {
    df <- n - 1
  } else if (!is.null(df) && is.null(n)) {
    n <- df + 1
  }
  
  stat_type <- if (!is.null(df)) "t" else "z"
  crit_val <- get_crit(df)
  
  # Initialize output vectors
  est_out <- numeric(n_preds + 1)
  se_out <- numeric(n_preds + 1)
  stat_out <- numeric(n_preds + 1)
  p_out <- numeric(n_preds + 1)
  ll_out <- numeric(n_preds + 1)
  ul_out <- numeric(n_preds + 1)
  
  approx_per_var <- vector("list", n_preds + 1)
  names(approx_per_var) <- var_names
  
  # Function to process one coefficient (including intercept)
  process_coef <- function(est, se_, stdb, sse, sdx, sdy, pval, ci_, stat, varname, is_intercept = FALSE) {
    approx_notes <- character(0)
    
    # For intercept, standardized coef usually not used
    estimate <- est
    se_val <- se_
    
    # Infer standardized beta if missing and not intercept
    if (!is_intercept) {
      if (is.na(stdb) && !is.na(estimate) && !is.na(sdx) && !is.na(sdy)) {
        stdb <- estimate * (sdx / sdy)
        approx_notes <- c(approx_notes, "Standardized beta approximated from unstandardized beta and SDs.")
      }
      if (is.na(sse) && !is.na(se_val) && !is.na(sdx) && !is.na(sdy)) {
        sse <- se_val * (sdx / sdy)
        approx_notes <- c(approx_notes, "SE of standardized beta approximated from unstandardized SE and SDs.")
      }
    }
    
    # Use standardized beta and SE if available (prefer)
    if (!is.na(stdb) && !is.na(sse)) {
      estimate <- stdb
      se_val <- sse
    }
    
    # Infer estimate from CI midpoint if missing
    if (is.na(estimate) && !any(is.na(ci_))) {
      estimate <- mean(ci_)
      approx_notes <- c(approx_notes, "Estimate approximated as midpoint of CI.")
    }
    
    # Infer SE from CI if missing
    if (is.na(se_val) && !any(is.na(ci_))) {
      se_val <- abs(ci_[2] - ci_[1]) / (2 * crit_val)
      approx_notes <- c(approx_notes, "SE approximated from CI width.")
    }
    
    # Infer SE from estimate and statistic if possible
    if (!is.na(stat) && is.na(se_val) && !is.na(estimate) && stat != 0) {
      se_val <- estimate / stat
      approx_notes <- c(approx_notes, "SE approximated from estimate and statistic.")
    }
    
    # Infer statistic if missing
    if (is.na(stat) && !is.na(estimate) && !is.na(se_val) && se_val != 0) {
      stat <- estimate / se_val
    }
    
    # Infer p-value if missing
    if (is.na(pval) && !is.na(stat)) {
      if (stat_type == "t") {
        if (one_sided) {
          pval <- 1 - pt(stat, df)
        } else {
          pval <- 2 * (1 - pt(abs(stat), df))
        }
      } else {
        if (one_sided) {
          pval <- 1 - pnorm(stat)
        } else {
          pval <- 2 * (1 - pnorm(abs(stat)))
        }
      }
      approx_notes <- c(approx_notes, "p-value computed from statistic.")
    }
    
    # Infer statistic and SE from p-value if missing
    if (is.na(stat) && !is.na(pval) && !is.na(estimate)) {
      crit_val_p <- if (one_sided) {
        if (!is.null(df)) qt(1 - pval, df) else qnorm(1 - pval)
      } else {
        if (!is.null(df)) qt(1 - pval/2, df) else qnorm(1 - pval/2)
      }
      stat <- sign(estimate) * crit_val_p
      approx_notes <- c(approx_notes, "Statistic approximated from p-value and estimate.")
      if (is.na(se_val) && stat != 0) {
        se_val <- estimate / stat
        approx_notes <- c(approx_notes, "SE approximated from estimate and reconstructed statistic.")
      }
    }
    
    # Compute CI if possible
    if (!is.na(estimate) && !is.na(se_val)) {
      LL <- estimate - crit_val * se_val
      UL <- estimate + crit_val * se_val
    } else {
      LL <- NA_real_
      UL <- NA_real_
    }
    
    # Check minimal info
    if (is.na(estimate) || is.na(se_val)) {
      cat(paste0("\nInsufficient Input for ", varname, ":"), sep = "\n")
      cat("Cannot estimate coefficient or SE.\n\n")
      return(NULL)
    }
    
    return(list(
      Estimate = round(estimate, digits),
      SE = round(se_val, digits),
      t = round(stat, digits),
      df = if (!is.null(df)) df else NA,
      p = round(pval, digits),
      LL = round(LL, digits),
      UL = round(UL, digits),
      Notes = approx_notes
    ))
  }
  
  # Process intercept first
  intercept_res <- process_coef(intercept, intercept_se, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, c(NA_real_, NA_real_), NA_real_, "Intercept", TRUE)
  if (is.null(intercept_res)) return(invisible(NULL))
  
  # Prepare result containers
  res_list <- vector("list", n_preds + 1)
  res_list[[1]] <- intercept_res
  
  # Process each predictor
  for (i in seq_len(n_preds)) {
    res <- process_coef(
      b[i], se[i],
      std_beta[i], se_std[i],
      sd_x[i], sd_y,
      p[i], ci[[i]],
      statistic[i],
      var_names[i + 1],
      FALSE
    )
    if (is.null(res)) return(invisible(NULL))
    res_list[[i + 1]] <- res
  }
  
  # Build data frame output
  out_df <- do.call(rbind, lapply(res_list, function(x) {
    as.data.frame(x[1:7], stringsAsFactors = FALSE)
  }))
  rownames(out_df) <- var_names
  
  # Collect notes
  approx_notes <- unlist(lapply(seq_along(res_list), function(i) {
    if (length(res_list[[i]]$Notes) == 0) return(NULL)
    paste0(var_names[i], ": ", paste(res_list[[i]]$Notes, collapse = " "))
  }))
  
  class(out_df) <- c("backcalc", class(out_df))
  if (attr) {
    attr(out_df, "Approximations") <- approx_notes
    attr(out_df, "attr") <- TRUE
  }
  
  return(out_df)
}
