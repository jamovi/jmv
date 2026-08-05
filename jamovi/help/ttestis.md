# Independent Samples T-Test

## Overview

Compares the means of two independent groups to test whether they differ in
the population. Each dependent variable is tested separately against the same
grouping variable.

Three tests are available. Student's t-test is the standard choice. Welch's
t-test relaxes the assumption that the two groups have equal variances. The
Mann-Whitney U test relaxes the assumption of normality, but tests a different
null hypothesis — that the two distributions are the same, not that the two
means are.

## When to use it

Use this analysis when each row of your data is a different person (or unit),
and a single grouping variable splits them into exactly two groups.

- If the same people were measured twice, use **Paired Samples T-Test** instead.
- If the grouping variable has three or more levels, use **One-Way ANOVA**.
- If you want to compare one group's mean against a fixed value, use
  **One Sample T-Test**.

## Assumptions

Student's t-test assumes that:

- observations are independent of one another,
- the dependent variable is normally distributed within each group,
- the two groups have equal variances.

The **Assumption Checks** options test the latter two. If normality is
violated, consider Mann-Whitney U. If the variances are unequal, use Welch's.
With reasonably large and similar-sized groups, the t-test is fairly robust to
non-normality.

## Options

### Dependent Variables

The variables to compare between the groups. A separate t-test is run for each
one; each appears as a row in the results table.

### Grouping Variable

The variable that splits the rows into two groups. It must be nominal or
ordinal, and it must have exactly two levels present in the data.

### Tests

- **Student's** — the standard t-test, assuming equal variances.
- **Bayes factor** — reports a Bayes factor alongside Student's t rather than
  a p-value, quantifying evidence for *both* hypotheses. The **Prior** is the
  Cauchy scale parameter (default 0.707); larger values place more weight on
  large effects.
- **Welch's** — does not assume equal variances. A safe default when group
  sizes or spreads differ.
- **Mann-Whitney U** — a rank-based test that does not assume normality. Note
  it tests whether the distributions differ, not whether the means do.

### Hypothesis

Selects the alternative hypothesis: that the two group means differ in either
direction (two-tailed, the default), or that one specific group is larger
(one-tailed). Group 1 is the first level of the grouping variable.

### Missing values

**Exclude cases analysis by analysis** uses every row for which that particular
dependent variable is present, so different variables may be based on different
numbers of cases. **Exclude cases listwise** drops any row with a missing value
on *any* variable in the analysis, so every test uses the same cases.

### Additional Statistics

- **Mean difference** — the difference between the two group means, optionally
  with a **Confidence interval**.
- **Effect size** — Cohen's d, optionally with a confidence interval. **The
  denominator differs between the two tests.** For Student's it is the pooled
  standard deviation, weighted by group size:

  $$\begin{aligned}
  d &= \frac{M_1 - M_2}{s_p} \\[4pt]
  s_p^2 &= \frac{(n_1 - 1)s_1^2 + (n_2 - 1)s_2^2}{n_1 + n_2 - 2}
  \end{aligned}$$

  For Welch's it is the unweighted average of the two variances:

  $$\begin{aligned}
  d &= \frac{M_1 - M_2}{s_a} \\[4pt]
  s_a^2 &= \frac{s_1^2 + s_2^2}{2}
  \end{aligned}$$

  The two agree when the groups are the same size and spread, and diverge
  otherwise — so a Student's d and a Welch's d in the same table are not
  directly comparable.

  For Mann-Whitney U the effect size is the rank biserial correlation:

  $$r_{rb} = 1 - \frac{2U}{n_1 n_2}$$
- **Descriptives** — group-wise N, mean, median, standard deviation and
  standard error.
- **Descriptives plots** — the group means with confidence intervals.

### Assumption Checks

- **Homogeneity test** — Levene's test for equality of variances. A significant
  result suggests using Welch's instead of Student's.
- **Normality test** — the Shapiro-Wilk test on the residuals. Significant
  results suggest the normality assumption is not met.
- **Q-Q plot** — plots the residual quantiles against a normal distribution.
  Points close to the diagonal indicate normality.

## Results

### Independent Samples T-Test

One row per dependent variable per selected test, giving the test statistic,
its degrees of freedom and a p-value, plus any additional statistics requested.

Welch's degrees of freedom are usually fractional, because they are
approximated from the two group variances rather than counted — that is
expected, not an error.

A small p-value (conventionally below .05) means the observed difference would
be unlikely if the two population means were equal.

### Assumptions

Results of the normality and homogeneity tests, one row per dependent variable.
Here a *large* p-value is the reassuring one — it means there is no evidence
the assumption is violated.

### Group Descriptives

Summary statistics for each group of each dependent variable. Useful for
reporting, and for seeing which group is higher.

### Plots

The descriptives plots and Q-Q plots requested above.

## Example

Using the `ToothGrowth` dataset, we ask whether tooth growth (`len`) differs
between the two supplement types (`supp`).

Put `len` into **Dependent Variables** and `supp` into **Grouping Variable**.

Tick **Mean difference** to see how far apart the groups are in the units of
the measurement, and **Effect size** to see the same gap in standard
deviations. Add **Descriptives** to check which group is higher — the t-test
itself does not tell you the direction.

Because the two supplement groups need not have equal spread, tick **Welch's**
as well and compare it against Student's.

## References

- Student. (1908). The probable error of a mean. *Biometrika, 6*(1), 1–25.
- Welch, B. L. (1947). The generalization of "Student's" problem when several
  different population variances are involved. *Biometrika, 34*, 28–35.
