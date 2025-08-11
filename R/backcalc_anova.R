#' Backcalculate Missing Inferential Statistics for ANOVA (Vectorised & Design-Aware)
#'
#' Handles between-subjects, within-subjects, factorial, and mixed designs, with optional
#' sphericity corrections. Works with multiple effects at once.
#'
#' @param F Numeric vector of F-statistics.
#' @param df1 Numeric vector of numerator degrees of freedom.
#' @param df2 Numeric vector of denominator degrees of freedom.
#' @param p Numeric vector of p-values.
#' @param eta2 Numeric vector of partial eta-squared values.
#' @param f Numeric vector of Cohen's f values.
#' @param design Character vector: "between", "within", or "mixed".
#' @param n Numeric vector: total sample size(s) or vector of group Ns.
#' @param levels List: each element is an integer vector of factor levels for the effect.
#' @param subjects Numeric vector: number of subjects (for within/mixed designs).
#' @param epsilon Numeric vector: sphericity correction factors.
#' @param effect Character vector: "main" or "interaction".
#' @param conf.level Confidence level for eta-squared CI.
#' @param digits Rounding digits for output.
#' @param attr Attach approximation notes as attributes.
#'
#' @return A data.frame with one row per effect, containing F, df1, df2, p, 
#' Cohen's f, partial eta-squared, and CI bounds for eta-squared. Attributes store approximation notes.
#'
#' @export
backcalc_anova <- function(F = NULL, df1 = NULL, df2 = NULL,
                           p = NULL, eta2 = NULL, f = NULL,
                           design = "between",
                           n = NULL, levels = NULL, subjects = NULL,
                           epsilon = 1, effect = "main",
                           conf.level = 0.95, digits = 3, attr = TRUE) {
  
  # --- Helper: recycle singletons ---
  recycle <- function(x, len) {
    if (is.null(x)) x <- rep(NA_real_, len)
    if (length(x) %in% c(0, 1)) rep(x, len) else x
  }
  
  # Determine number of rows
  len <- max(
    length(F), length(df1), length(df2), length(p),
    length(eta2), length(f),
    length(design), length(n), length(levels),
    length(subjects), length(epsilon), length(effect)
  )
  
  # Recycle all
  F <- recycle(F, len)
  df1 <- recycle(df1, len)
  df2 <- recycle(df2, len)
  p <- recycle(p, len)
  eta2 <- recycle(eta2, len)
  f <- recycle(f, len)
  design <- recycle(design, len)
  n <- recycle(n, len)
  subjects <- recycle(subjects, len)
  epsilon <- recycle(epsilon, len)
  effect <- recycle(effect, len)
  
  # Ensure levels is list
  if (is.null(levels)) {
    levels <- vector("list", len)
  } else if (!is.list(levels)) {
    levels <- rep(list(levels), len)
  } else if (length(levels) != len) {
    levels <- rep(levels, length.out = len)
  }
  
  results <- vector("list", len)
  approx_all <- vector("list", len)
  
  for (i in seq_len(len)) {
    approx_notes <- character()
    
    # --- DF derivation if missing ---
    if ((is.na(df1[i]) || is.na(df2[i])) && !is.null(levels[[i]]) && !all(is.na(levels[[i]]))) {
      lv <- levels[[i]]
      if (design[i] == "between") {
        k <- prod(lv)
        if (effect[i] == "main") {
          df1[i] <- lv[1] - 1
        } else {
          df1[i] <- prod(lv - 1)
        }
        N <- if (length(n[i]) > 1) sum(n[[i]]) else n[i]
        df2[i] <- N - k
      }
      if (design[i] == "within") {
        if (is.na(subjects[i])) {
          # no message saved
        } else {
          if (effect[i] == "main") {
            df1[i] <- (lv[1] - 1)
          } else {
            df1[i] <- prod(lv - 1)
          }
          df2[i] <- df1[i] * (subjects[i] - 1)
          df1[i] <- df1[i] * epsilon[i]
          df2[i] <- df2[i] * epsilon[i]
          approx_notes <- c(approx_notes, paste0("Applied sphericity correction ε = ", epsilon[i], "."))
        }
      }
      if (design[i] == "mixed") {
        if (is.na(subjects[i]) || is.na(n[i])) {
          # no message saved
        } else {
          if (effect[i] == "main") {
            df1[i] <- (lv[1] - 1)
            df2[i] <- n[i] - lv[1]
          } else {
            df1[i] <- prod(lv - 1)
            df2[i] <- df1[i] * (subjects[i] - 1)
            df1[i] <- df1[i] * epsilon[i]
            df2[i] <- df2[i] * epsilon[i]
            approx_notes <- c(approx_notes, paste0("Applied sphericity correction ε = ", epsilon[i], "."))
          }
        }
      }
    }
    
    # --- Back-calculation logic ---
    if (!is.na(F[i]) && !is.na(df1[i]) && !is.na(df2[i]) && is.na(p[i])) {
      p[i] <- 1 - pf(F[i], df1[i], df2[i])
    }
    if (is.na(F[i]) && !is.na(p[i]) && !is.na(df1[i]) && !is.na(df2[i])) {
      F[i] <- qf(1 - p[i], df1[i], df2[i])
      approx_notes <- c(approx_notes, "F-statistic approximated from p-value.")
    }
    if (is.na(eta2[i]) && !is.na(F[i]) && !is.na(df1[i]) && !is.na(df2[i])) {
      eta2[i] <- (F[i] * df1[i]) / (F[i] * df1[i] + df2[i])
      approx_notes <- c(approx_notes, "Partial eta-squared computed from F, df1, df2.")
    }
    if (is.na(F[i]) && !is.na(eta2[i]) && !is.na(df1[i]) && !is.na(df2[i])) {
      F[i] <- (eta2[i] / (1 - eta2[i])) * (df2[i] / df1[i])
      approx_notes <- c(approx_notes, "F-statistic computed from partial eta-squared.")
    }
    if (is.na(f[i]) && !is.na(eta2[i])) {
      f[i] <- sqrt(eta2[i] / (1 - eta2[i]))
      approx_notes <- c(approx_notes, "Cohen's f computed from partial eta-squared.")
    }
    if (is.na(eta2[i]) && !is.na(f[i])) {
      eta2[i] <- f[i]^2 / (1 + f[i]^2)
      approx_notes <- c(approx_notes, "Partial eta-squared computed from Cohen's f.")
    }
    
    # --- CI for eta2 ---
    ci_eta2 <- c(NA, NA)
    if (!is.na(eta2[i]) && !is.na(df1[i]) && !is.na(df2[i]) && !is.na(F[i])) {
      alpha <- 1 - conf.level
      lower_F <- qf(alpha/2, df1[i], df2[i])
      upper_F <- qf(1 - alpha/2, df1[i], df2[i])
      ci_eta2 <- c(
        (lower_F * df1[i]) / (lower_F * df1[i] + df2[i]),
        (upper_F * df1[i]) / (upper_F * df1[i] + df2[i])
      )
      approx_notes <- c(approx_notes, "CI for partial eta-squared is approximate.")
    }
    
    results[[i]] <- data.frame(
      F = round(F[i], digits),
      df1 = round(df1[i], 3),
      df2 = round(df2[i], 3),
      p = round(p[i], digits),
      f = round(f[i], digits),
      eta2 = round(eta2[i], digits),
      LL = round(ci_eta2[1], digits),
      UL = round(ci_eta2[2], digits)
    )
    approx_all[[i]] <- approx_notes
  }
  
  final <- do.call(rbind, results)
  rownames(final) <- paste0("Effect", seq_len(len))
  class(final) <- c("backcalc", class(final))
  attr(final, "Approximations") <- approx_notes
  attr(final, "attr") <- attr
  return(final)
}
