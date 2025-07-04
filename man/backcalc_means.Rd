% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/backcalc_means.R
\name{backcalc_means}
\alias{backcalc_means}
\title{Backcalculate Missing Inferential Statistics for Means}
\usage{
backcalc_means(
  m = NULL,
  se = NULL,
  sd = NULL,
  n = NULL,
  df = NULL,
  p = NULL,
  ci = NULL,
  statistic = NULL,
  paired = FALSE,
  one_sided = FALSE,
  conf.level = 0.95,
  digits = 3
)
}
\arguments{
\item{m}{Numeric or length-2 numeric vector. Point estimate(s) (e.g., mean for one group,
or two values for two groups where difference = m\link{1} - m\link{2}).}

\item{se}{Numeric. Standard error of the estimate.}

\item{sd}{Numeric or length-2 numeric vector. Standard deviation(s) for one or two groups.}

\item{n}{Numeric or length-2 numeric vector. Sample size(s) for one or two groups.}

\item{df}{Numeric. Degrees of freedom.}

\item{p}{Numeric. P-value.}

\item{ci}{Numeric vector of length 2. Confidence interval as c(lower, upper).}

\item{statistic}{Numeric. Test statistic value (t or z).}

\item{paired}{Logical. Whether data come from a paired/matched design (default FALSE).}

\item{one_sided}{Logical. Whether the test is one-sided (default FALSE).}

\item{conf.level}{Numeric. Confidence level for intervals (default 0.95).}

\item{digits}{Integer. Number of decimal digits to round the results (default 3).}
}
\value{
Named numeric vector containing:
\describe{
\item{Estimate}{Point estimate (difference if two values provided)}
\item{SE}{Standard error of estimate}
\item{t / z}{Test statistic (t if df provided, otherwise z)}
\item{df}{Degrees of freedom (if available)}
\item{p / p-one}{Two-sided or one-sided p-value}
\item{LL}{Lower bound of the confidence interval}
\item{UL}{Upper bound of the confidence interval}
}
}
\description{
\code{backcalc_means()} reconstructs inferential statistics related to means from partial information.
It can estimate standard errors, confidence intervals, p-values, test statistics (t or z), degrees of freedom,
and point estimates when some components are missing, supporting one-sample, paired, and two-sample cases.
}
\details{
The function infers missing statistics using provided inputs. For two-sample tests,
it calculates standard error and degrees of freedom using pooled or Welch's approximation as appropriate.
When sample sizes are provided, it defaults to using the t-distribution for inference rather than normal approximation.
The confidence interval is computed based on the specified \code{conf.level}.
}
\examples{
# One-sample example: Mean, SE, and sample size given (uses t-distribution)
backcalc_means(m = 25.4, se = 2.1, n = 30)

# Two-sample example: Means, SDs, and sample sizes given (Welch t-test inference)
backcalc_means(m = c(15, 12), sd = c(4, 5), n = c(40, 35))

# Insufficient input example: Mean provided but no SE, p-value, or CI (warns user)
backcalc_means(m = 25)

}
