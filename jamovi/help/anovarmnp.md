# Repeated Measures ANOVA (Non-parametric)

## Overview

The Friedman test compares three or more measurements taken from the same
people, without assuming the data are normally distributed. It ranks the
measurements *within* each person and asks whether those ranks differ
systematically across conditions.

It is the non-parametric counterpart of the repeated measures ANOVA, and the
extension of the Wilcoxon signed-rank test to more than two conditions. As with
the Kruskal-Wallis test, it handles a single explanatory variable only.

## When to use it

Use this when you would have run a repeated measures ANOVA, but the data make
you uneasy: skewed distributions, outliers, small samples, or ordinal outcomes
such as rating scales.

- If the assumptions are tenable, use **Repeated Measures ANOVA** — it is more
  powerful, handles factorial within-subject designs, and accepts
  between-subject factors and covariates.
- With exactly two measurements, use **Paired Samples T-Test**, which offers
  the Wilcoxon signed-rank test.
- If every condition was measured on *different* people, use **One-Way ANOVA
  (Non-parametric)**.

Because ranking happens within each person, the test is unaffected by
differences in how individuals use a scale — one participant rating everything
high and another rating everything low contributes nothing to the result. That
is a genuine strength with subjective ratings.

## Assumptions

- Each person's measurements come from the same set of conditions, with no
  missing cells.
- People are independent of one another.
- The dependent variable is at least ordinal.

There is no normality assumption and no sphericity assumption — sphericity
being the one that most often causes trouble in the parametric version.

Data must be in wide format: one row per person, one column per condition.

## Options

### Measures

The variables holding the repeated measurements, one per condition. Order
matters for interpretation but not for the test itself.

### Pairwise comparisons (Durbin-Conover)

Having found that the conditions differ, this identifies which pairs differ.
The Durbin-Conover procedure is the standard follow-up to a Friedman test.

### Descriptives

A table of summary statistics per condition — useful because the Friedman test
itself reports only the overall result, and gives no indication of which
conditions were higher.

### Descriptive plot

Plots the conditions, with **Plot Type** choosing whether to show **Means** or
**Medians**. Prefer medians here: if the data warranted a mean, they would
probably have warranted the parametric test.

## Results

### Friedman

A single row giving χ², its degrees of freedom and a p-value.

Ranking happens within each person, which is what makes the test immune to
individual differences in scale use: only the ordering of a person's own
responses contributes.

A small p-value means the conditions are not all equivalent. It does not say
which differ, or in which direction — the pairwise comparisons and the
descriptives answer that.

Note that this is one test for the whole set of measures, not one row per
variable: the measures are the conditions being compared, not separate
analyses.

### Pairwise Comparisons (Durbin-Conover)

One row per pair of conditions, with the test statistic and a p-value. Read
these alongside the descriptives to establish the direction of each difference.

### Descriptives

Summary statistics per condition.

### Descriptive Plot

The plot requested above, showing means or medians per condition.

## Example

Using the `bugs` dataset, we ask whether people's ratings differ across the
four bug types, without assuming normality — the ratings are subjective, and
participants vary in how they use the scale.

Put `LDLF`, `LDHF`, `HDLF` and `HDHF` into **Measures**.

Tick **Descriptives** and **Descriptive plot**, and set **Plot Type** to
Medians. Then tick **Pairwise comparisons (Durbin-Conover)** to see which bug
types actually differ from which.

Note that this treats the four columns as four conditions in a row. The 2 × 2
structure — disgust crossed with frequency — is not available here; testing the
interaction between those two factors requires the parametric **Repeated
Measures ANOVA**.

## References

- Friedman, M. (1937). The use of ranks to avoid the assumption of normality
  implicit in the analysis of variance. *Journal of the American Statistical
  Association, 32*(200), 675–701.
- Conover, W. J. (1999). *Practical Nonparametric Statistics* (3rd ed.). Wiley.

The Friedman test uses R's own implementation.

- R Core Team (2026). *A Language and Environment for Statistical Computing*
  (Version 4.6) [Computer software].
