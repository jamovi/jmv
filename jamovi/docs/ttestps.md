# Paired Samples T-Test

## Overview

Compares two measurements taken from the same people (or the same units) to
test whether they differ. Each pair of variables you specify is tested
separately.

The test works on the *differences* within each pair, not on the two sets of
measurements as separate groups. Student's paired t-test tests whether the mean
difference is zero. The Wilcoxon signed-rank test does the same job without
assuming the differences are normally distributed, though it tests a slightly
different null hypothesis — that the two sets of measurements follow the same
distribution.

## When to use it

Use this analysis when every value in the first variable belongs with a
specific value in the second — the same person before and after, the same
subject under two conditions, or matched pairs of participants.

- If the two sets of numbers come from *different* people, use **Independent
  Samples T-Test**.
- If there are three or more measurements per person, use **Repeated Measures
  ANOVA**.
- If you want to compare one measurement against a fixed value rather than
  against a second measurement, use **One Sample T-Test**.

Pairing matters: using the wrong test here is not a minor difference. The
paired test removes the variation between people, so it is usually the more
sensitive of the two when the design really is paired.

## Assumptions

Student's paired t-test assumes that:

- the **differences** between the pairs are normally distributed — this is an
  assumption about the differences, not about either measurement on its own,
  so a skewed variable is not by itself a problem;
- the pairs are independent of one another.

If the differences are clearly non-normal, use the Wilcoxon signed-rank test.
With larger samples the t-test is fairly robust to non-normality.

## Options

### Paired Variables

The pairs of measurements to compare. A separate test is run for each pair, and
each appears as a row in the results table.

### Tests

- **Student's** — the standard paired t-test on the mean difference.
- **Bayes factor** — reports a Bayes factor rather than a p-value, quantifying
  the evidence for *both* hypotheses. The **Prior** is the Cauchy scale
  parameter (default 0.707); larger values place more weight on large effects.
- **Wilcoxon rank** — the signed-rank test, which ranks the differences instead
  of using their values, and so does not assume normality.

Pairs whose two measurements are identical contribute a difference of zero.
These are dropped before the Wilcoxon test is computed, so its N can be smaller
than that of the t-test on the same data.

### Hypothesis

Selects the alternative hypothesis: that the two measurements differ in either
direction (two-tailed, the default), or that one specific measurement is larger
(one-tailed). Measure 1 is the first variable in the pair.

### Missing values

**Exclude cases analysis by analysis** uses every row where both members of
that particular pair are present, so different pairs may rest on different
numbers of cases. **Exclude cases listwise** drops any row with a missing value
on any variable in the analysis, so every pair uses the same cases.

### Additional Statistics

- **Mean difference** — the average difference within pairs, optionally with a
  **Confidence interval**. This is in the units of your measurement, which
  usually makes it the most interpretable number in the output.
- **Effect size** — Cohen's d, computed from the standard deviation of the
  **differences**:

  $$d = \frac{M_d}{s_d}$$

  This is the variant often written $d_z$. Other software — G*Power among them
  — frequently reports a version based on the standard deviations of the two
  raw measurements instead. Those values are *smaller* whenever the two
  measures correlate positively, which in a paired design they nearly always
  do, so figures may not match across packages. Say which you used.

  For Wilcoxon the effect size is the rank biserial correlation, computed after
  zero differences have been dropped.
- **Descriptives** — N, mean, median, standard deviation and standard error for
  each measurement.
- **Descriptives plots** — the two means with confidence intervals.

### Assumption Checks

- **Normality test** — the Shapiro-Wilk test on the pair differences. A
  significant result suggests the differences are not normally distributed.
- **Q-Q plot** — plots the quantiles of the differences against a normal
  distribution. Points close to the diagonal indicate normality.

See [checking assumptions](topics/assumption-checks.md) for why these tests
mislead in very small and very large samples, and what to do when one fails.

## Results

### Paired Samples T-Test

One row per pair per selected test, giving the test statistic, its degrees of
freedom and a p-value, plus any additional statistics requested.

A small p-value means the observed difference would be unlikely if the true
mean difference were zero. The sign of the statistic tells you which
measurement was larger, but the **Mean difference** and **Descriptives** are
easier to read for that.

### Normality Test (Shapiro-Wilk)

Tests whether the pair differences are normally distributed. Here a *large*
p-value is the reassuring one — it means there is no evidence against the
assumption. Note that with large samples this test will flag departures too
small to matter, so read it alongside the Q-Q plot rather than on its own.

### Descriptives

Summary statistics for each measurement in the pair. Useful for reporting and
for seeing which measurement was higher.

### Plots

The descriptives plots and Q-Q plots requested above, one set per pair.

## Example

Using the `bugs` dataset, we ask whether people react differently to
low-disgust and high-disgust bugs when the bugs are rare.

Put `LDLF` and `HDLF` in as a pair under **Paired Variables** — these are the
low-frequency ratings for low- and high-disgust bugs, measured on the same
people.

Tick **Mean difference** with its **Confidence interval** to see the size of
the gap in rating units, and **Descriptives** to see which condition scored
higher. Under Assumption Checks, tick **Q-Q plot** to check the differences
look normal before trusting the t-test; if they do not, tick **Wilcoxon rank**
and compare.

## References

- Student. (1908). The probable error of a mean. *Biometrika, 6*(1), 1–25.
- Wilcoxon, F. (1945). Individual comparisons by ranking methods. *Biometrics
  Bulletin, 1*(6), 80–83.
- Kerby, D. S. (2014). The simple difference formula: An approach to teaching
  nonparametric correlation. *Comprehensive Psychology, 3*.

Bayes factors are computed with BayesFactor.

- Morey, R. D., Rouder, J. N., Jamil, T., Urbanek, S., Forner, K., & Ly, A.
  (2026). *BayesFactor: Computation of Bayes Factors for Common Designs* [R
  package].
- Revelle, W. (2026). *psych: Procedures for Psychological, Psychometric, and
  Personality Research* [R package].
