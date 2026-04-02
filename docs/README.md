# [`backcalc`](https://github.com/cwendorf/backcalc/)

## Introduction

`backcalc` provides functions to reconstruct key inferential statistics such as standard errors,
confidence intervals, test statistics, and p-values from partial or incomplete summary information
commonly reported in scientific literature. It also includes functions to calculate standardized
effect sizes (Cohen's d, Hedges' g, Glass's delta) from various inputs.

- [The Challenge](#the-challenge)
- [The Solution](#the-solution)
- [Resources](#resources)

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

### Resources

- [Reference](./man/README.md): Documentation for all exported functions, including usage, arguments, and return values.
- [Articles](./vignettes/README.md): Examples demonstrating how to use the package.

