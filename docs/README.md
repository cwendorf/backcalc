# [`backcalc`](https://github.com/cwendorf/backcalc/)

## Overview

`backcalc` is an R package designed to help researchers and students reconstruct key inferential statistics from partial or incomplete summary data commonly found in scientific literature. 

- [The Challenge](#the-challenge)
- [The Solution](#the-solution)
- [Examples](#examples)

------------------------------------------------------------------------

### The Challenge

Scientific articles rarely report complete inferential statistics. You might find:

- Means and *n*, but no standard errors or confidence intervals
- *p*-values and sample sizes, but no test statistics or effect sizes  
- Confidence intervals, but no standard errors for meta-analysis
- *p* < .05, with no exact values
- Graphs with error bars, but limited numerical summaries
- ANOVA *F* and *p***, but no effect sizes or confidence intervals

This makes it difficult to:
- Conduct meta-analyses requiring SE or CI
- Verify reported statistics for errors
- Teach students statistical relationships
- Extract effect sizes from older literature

### The Solution

`backcalc` reconstructs missing statistics from whatever information is available. It leverages mathematical relationships between statistics to back-calculate what's missing.

Unlike calculators requiring specific inputs, `backcalc` works with whatever you have:
- Mean + SE + *n*
- Mean + CI + df
- Mean + *p* + df  
- Mean + *t* + SE
- Any other valid combination

`backcalc` is transparent about approximations. When exact reconstruction isn't possible, it clearly notes:
- Which values were approximated
- What assumptions were made
- Why certain inferences are uncertain
- What information is missing

### Examples

This package contains a set of examples to demonstrate its use:

- [Means Examples](./MeansExamples.md) - Means and mean differences
- [Correlations Examples](./CorrelationsExamples.md) - Correlations and correlation differences
- [Coefficients Examples](./CoefficientsExamples.md) - Raw and standardized regresssion coefficients
- [Medians Examples](./MediansExamples.md) - Medians and median differences
- [Proportions Examples](./ProportionsExamples.md) - Proportions and proportion differences
- [Ratios Examples](./RatiosExamples.md) - Ratios and ratio differences
- [ANOVA Examples](./ANOVAExamples.md) - Between, Within, Factorial, and Mixed Design ANOVAs
