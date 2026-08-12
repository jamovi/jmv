# One Sample T-Test

## Overview

Tests whether the mean of a variable differs from a particular value — by
default zero, but you can set any value you like. Each variable you specify is
tested separately against the same test value.

Student's one-sample t-test tests the mean. The Wilcoxon signed-rank test does
the same job without assuming normality, but tests the **median** against the
test value rather than the mean.

## When to use it

Use this analysis when you have one variable and a meaningful number to compare
it against — a scale midpoint, a published norm, a chance level, or zero for a
set of difference scores.

- If you are comparing two groups of people, use **Independent Samples
  T-Test**.
- If you are comparing two measurements from the same people, use **Paired
  Samples T-Test**.
- If you have no particular value in mind and just want to describe the
  variable, use **Descriptives**.

The analysis is only as meaningful as the test value. A significant result
against a value chosen arbitrarily tells you very little.

## Assumptions

Student's one-sample t-test assumes that:

- observations are independent of one another;
- the variable is normally distributed.

If normality is doubtful, use the Wilcoxon signed-rank test — but remember it
switches the question to the median. With larger samples the t-test is fairly
robust to non-normality.

## Options

### Dependent Variables

The variables to test. A separate test is run for each, and each appears as a
row in the results table. All of them are compared against the same test value.

### Tests

- **Student's** — the standard one-sample t-test on the mean.
- **Bayes factor** — reports a Bayes factor rather than a p-value, quantifying
  the evidence for *both* hypotheses. The **Prior** is the Cauchy scale
  parameter (default 0.707); larger values place more weight on large effects.
- **Wilcoxon rank** — the signed-rank test, which does not assume normality and
  tests the median against the test value.

Observations exactly equal to the test value contribute a difference of zero.
These are dropped before the Wilcoxon test is computed, so its N can be smaller
than that of the t-test on the same data.

### Hypothesis

**Test value** is the value each variable is compared against; the default of 0
is only sensible when your variable is a difference or a deviation score. Set
it to whatever your comparison actually is.

The three alternatives are:

| | Alternative (Hₐ) | Null (H₀) |
|---|---|---|
| ≠ Test value | Mean ≠ Test value | Mean = Test value |
| > Test value | Mean > Test value | Mean ≤ Test value |
| < Test value | Mean < Test value | Mean ≥ Test value |

The two-tailed option is the default and the safe choice. Only pick a
one-tailed alternative if the direction was decided before seeing the data.

### Missing values

**Exclude cases analysis by analysis** uses every row where that particular
variable is present, so different variables may rest on different numbers of
cases. **Exclude cases listwise** drops any row with a missing value on any
variable in the analysis, so every test uses the same cases.

### Additional Statistics

- **Mean difference** — how far the mean sits from the test value, optionally
  with a **Confidence interval**. In the units of your measurement, so usually
  the most interpretable number in the output.
- **Effect size** — Cohen's d, the distance from the test value in standard
  deviations:

  $$d = \frac{M - \mu_0}{s}$$

  where $\mu_0$ is the test value. For Wilcoxon the effect size is the rank
  biserial correlation, computed after observations equal to the test value
  have been dropped.
- **Descriptives** — N, mean, median, standard deviation and standard error.
- **Descriptives plots** — the mean with its confidence interval, drawn against
  the test value.

### Assumption Checks

- **Normality test** — the Shapiro-Wilk test. A significant result suggests the
  variable is not normally distributed.
- **Q-Q plot** — plots the variable's quantiles against a normal distribution.
  Points close to the diagonal indicate normality.

See [checking assumptions](topics/assumption-checks.md) for why these tests
mislead in very small and very large samples, and what to do when one fails.

## Results

### One Sample T-Test

One row per variable per selected test, giving the test statistic, its degrees
of freedom and a p-value, plus any additional statistics requested.

A small p-value means the observed mean would be unlikely if the true mean
equalled the test value. To see the *direction* of the difference, read the
**Mean difference** or the **Descriptives** — a p-value alone does not tell you
whether the mean was above or below the test value.

### Normality Test (Shapiro-Wilk)

Tests whether each variable is normally distributed. Here a *large* p-value is
the reassuring one — it means there is no evidence against the assumption. With
large samples this test flags departures too small to matter, so read it
alongside the Q-Q plot.

### Descriptives

Summary statistics for each variable, including the mean you are testing and
the spread that determines how precisely it is estimated.

### Plots

The descriptives plots requested above, showing each mean and its confidence
interval against the test value.

### Q-Q plots

The Q-Q plots requested under Assumption Checks, one per variable.

## Example

Using the `ToothGrowth` dataset, we ask whether the guinea pigs received more
than the lowest dose on average.

Put `dose` into **Dependent Variables**, and set **Test value** to `1` — the
lowest dose used in the study.

Tick **Mean difference** with its **Confidence interval** to see how far above
1 the average dose sat, and **Descriptives** to put that in context. Under
Assumption Checks, tick **Q-Q plot**: `dose` takes only three distinct values,
so the plot will show clear steps rather than a smooth diagonal, and the
Wilcoxon test is the safer choice here.

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
