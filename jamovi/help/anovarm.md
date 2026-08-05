# Repeated Measures ANOVA

## Overview

Tests an outcome measured several times on the same people, under different
conditions or at different times. Because each person acts as their own
control, differences between people are removed from the error term, which
usually makes this analysis considerably more sensitive than the
between-subjects equivalent.

Between-subject factors and covariates can be included alongside the repeated
measures, giving a mixed design.

## When to use it

Use this analysis when each row of your data is a person, and several columns
hold repeated measurements of the same thing.

- With exactly two measurements and nothing else in the design, **Paired
  Samples T-Test** answers the same question more directly.
- If every condition was measured on *different* people, use **ANOVA**.
- If the assumptions are badly violated, use **Repeated Measures ANOVA
  (Non-parametric)**.

Data must be in wide format: one row per person, one column per condition. If
your data have one row per observation, they will need restructuring first.

## Assumptions

- Observations from different people are independent, though measurements
  within a person are not — that is the point of the design.
- The residuals are normally distributed.
- **Sphericity**: the variances of the differences between every pair of
  conditions are equal.

Sphericity is the assumption specific to this analysis and the one most often
violated. It cannot apply to a factor with only two levels, so it only becomes
a concern from three levels upward. When it is violated the F test becomes too
liberal — you get significant results too easily — which is what the sphericity
corrections address.

Adding between-subject factors brings back the assumption that residual
variance is equal across groups.

## Options

### Repeated Measures Factors

Define the within-subject structure here: name each factor and its levels. A
2 × 2 design means two factors of two levels each, not four conditions in a
row. Get this right first — everything else follows from it.

### Repeated Measures Cells

Assign each existing variable in your data to a cell of the design you just
defined. One variable per cell.

### Between Subject Factors

Categorical variables that split *people* into groups — a treatment group, a
gender. Adding one turns the analysis into a mixed design and produces a
separate Between Subjects Effects table.

### Covariates

Continuous variables to adjust for.

### Effect Size

The same three measures as factorial ANOVA:

$$\begin{aligned}
\eta^2 &= \frac{SS_{effect}}{SS_{total}} \\[4pt]
\eta_p^2 &= \frac{SS_{effect}}{SS_{effect} + SS_{error}} \\[4pt]
\omega^2 &= \frac{SS_{effect} - df_{effect} \cdot MS_{error}}{SS_{total} + MS_{error}}
\end{aligned}$$

Partial η² is the most commonly reported for repeated measures and is the
largest of the three. It matters more here than elsewhere which you quote:
because the design removes between-subject variance from the error term, the
error in the partial η² denominator is small, and the resulting values are
substantially higher than the η² for the same effect.

**Dependent Variable Label** sets the name used for the outcome in the output.
It is cosmetic, but worth setting since the cells are named after conditions
rather than after the thing being measured.

### Model

**Repeated Measures Components** and **Between Subjects Components** control
which terms are fitted on each side of the design. **Sum of squares** sets how
variance is attributed in unbalanced designs; Type 3 is the default.

### Assumption Checks

- **Sphericity tests** — Mauchly's test. A significant result indicates
  sphericity is violated.
- **Sphericity corrections** — **None**, **Greenhouse-Geisser** or
  **Huynh-Feldt**. Both multiply the degrees of freedom by an estimate of how
  far sphericity is violated, which always makes the test more conservative.
  Greenhouse-Geisser is the more conservative of the two; Huynh-Feldt is less
  so and preferred when the violation is mild.
- **Homogeneity test** — Levene's test, relevant when between-subject factors
  are present.
- **Q-Q Plot** — residual quantiles against a normal distribution.

Mauchly's test has low power in small samples and excessive power in large
ones, so many analysts skip it and simply apply a correction by default. If you
do report a correction, the fractional degrees of freedom in the output are the
corrected ones.

### Contrasts

Planned comparisons within a factor, defined before seeing the data. The same
types are available as in factorial ANOVA; **Polynomial** is especially useful
here, since repeated measures are so often ordered in time or dose and a trend
is the natural hypothesis.

### Post Hoc Tests

All-pairs comparisons with a correction for their number. **Tukey**,
**Bonferroni**, **Holm** and **No correction** are available.

### Estimated Marginal Means

Model-predicted means for the terms you specify, averaging over the rest of the
design, with **Marginal means plots** and **Marginal means tables**. For a
within-between interaction, the plot is usually the clearest thing you can
show. **Equal cell weights** averages cells equally rather than by size, and
**Confidence interval** sets the interval width.

### Options

**Group summary** reports the number of cases in each between-subject group,
which is worth checking before interpreting a mixed design.

## Results

### Within Subjects Effects

The repeated measures factors and their interactions, with sums of squares,
degrees of freedom, F and p-values. When a sphericity correction is applied,
the corrected degrees of freedom appear here and will be fractional.

Read interactions before main effects: a within-between interaction means the
conditions behaved differently for different groups of people, and the main
effect averages over that difference.

### Between Subjects Effects

The between-subject factors and covariates, tested against the between-subjects
error term. Absent when the design has no between-subject factors.

### Assumptions

Mauchly's sphericity test and any homogeneity test. Here a *large* p-value is
the reassuring one.

### Contrasts

One row per planned comparison, with its estimate, standard error, t and
p-value. Not corrected for multiple comparisons.

### Post Hoc Tests

Every pair of levels with corrected p-values. The correction is already
applied; do not adjust again.

### Estimated Marginal Means

Model-predicted means with confidence intervals, one table or plot per term.

### Group Summary

Case counts per between-subject group.

## Example

Using the `bugs` dataset, we ask whether people's ratings depend on how
disgusting a bug is, how frequently it is encountered, and whether those two
things interact.

Under **Repeated Measures Factors**, define two factors: `Disgust` with levels
`Low` and `High`, and `Frequency` with levels `Low` and `High`. Then in
**Repeated Measures Cells**, assign `LDLF`, `LDHF`, `HDLF` and `HDHF` to the
four cells this creates.

Tick **partial η²** under Effect Size. Under Estimated Marginal Means add a
term with both factors and tick **Marginal means plots** — the interaction plot
is what shows whether disgust matters more for frequently encountered bugs.

Both factors have only two levels here, so sphericity cannot be violated and
the corrections are unnecessary. With three or more levels you would tick
**Greenhouse-Geisser**.

## References

- Mauchly, J. W. (1940). Significance test for sphericity of a normal
  n-variate distribution. *The Annals of Mathematical Statistics, 11*(2),
  204–209.
- Greenhouse, S. W., & Geisser, S. (1959). On methods in the analysis of
  profile data. *Psychometrika, 24*(2), 95–112.
