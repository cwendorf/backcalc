#' backcalc: Reconstruct Inferential Statistics from Partial Summary Data
#'
#' This package provides a set of functions to reconstruct key inferential statistics
#' such as standard errors, confidence intervals, test statistics, and p-values
#' from partial or incomplete summary information commonly reported in scientific
#' literature. The functions support a variety of data types and designs, including
#' single-sample, two-sample (independent or paired), one- and two-sided tests,
#' and accommodate both z and t distributions.
#'
#' Specifically, the package offers tailored functions for:
#' \itemize{
#'   \item Means (\code{backcalc_means})
#'   \item Regression coefficients (\code{backcalc_coeffs})
#'   \item Correlations (\code{backcalc_corrs})
#'   \item Ratio measures (\code{backcalc_ratios})
#'   \item Proportions (\code{backcalc_props})
#' }
#'
#' These tools facilitate meta-analysts, researchers, and students in recovering missing
#' inferential details from published results to enable accurate synthesis, reporting,
#' or further analysis.
#'
#' @keywords internal
"_PACKAGE"
