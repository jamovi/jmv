# Proportion Test (2 Outcomes)

## Overview

Tests whether the proportion of one of two outcomes differs from an expected
value — whether a coin is fair, whether a pass rate exceeds a target, whether
a preference is stronger than chance.

This is the binomial test, and it is exact rather than approximate: it
calculates the probability directly from the binomial distribution rather than
relying on a large-sample approximation. That makes it reliable even with very
small samples.

## When to use it

Use this when you have one categorical variable with two levels and a specific
proportion to test it against.

- If the variable has three or more levels, use **Proportion Test (N
  Outcomes)**.
- To compare proportions between two *groups* rather than against a fixed
  value, use **Contingency Tables**.
- To predict a binary outcome from other variables, use **Binomial Logistic
  Regression**.

The test value is what gives the analysis meaning. The default of 0.5 tests
against an even split, which is right for a coin and wrong for most other
things.

## Assumptions

- Observations are independent of one another.
- Each observation falls into exactly one of the two categories.
- The probability of the outcome is the same for every observation.

There is no sample size requirement, since the test is exact. The third
assumption is the one most often overlooked: if the probability genuinely
varies across observations — different people, different conditions — the test
is answering a question about an average that may not describe anyone.

## Options

### Variables

The variables to test, each with exactly two levels. Each is tested separately
and appears as its own block of rows.

**Values are counts** tells jamovi that your data are already summarised as
counts rather than one row per observation.

### Hypothesis

**Test value** is the proportion to test against; 0.5 by default.

The three alternatives are **≠ Test value** (two-tailed, the default), **>
Test value** and **< Test value**. Choose a one-tailed test only if the
direction was predicted in advance.

### Additional Statistics

**Confidence intervals** for the observed proportion, at the width set by
**Interval**. More informative than the p-value alone, since it shows how
precisely the proportion is estimated — with a small sample that interval is
often startlingly wide.

### Bayesian Statistics

**Bayes factor** quantifies evidence for *both* hypotheses rather than only
against the null, which lets you distinguish evidence of no effect from absence
of evidence.

The **Prior** is a beta distribution set by its **a parameter** and **b
parameter**; the default of a = b = 1 is uniform, treating every proportion as
equally likely beforehand.

**Credible intervals** give the Bayesian counterpart of a confidence interval —
an interval containing the true proportion with the stated probability, which
is what most people mistakenly take a confidence interval to mean.

**Posterior plot** draws the resulting distribution for the proportion.

## Results

### Binomial Test

One row per level of each variable, giving the observed count, the total, the
observed proportion, and the p-value against the test value, plus any intervals
or Bayes factors requested.

Both levels are reported, and they are complementary — testing whether one
proportion exceeds 0.6 is the same test as whether the other falls below 0.4.
Read the row that matches your question and ignore the other.

### Posterior Plots

The posterior distribution for each proportion, if requested. The spread shows
how much the data have narrowed down the plausible values.

## Example

Using the `bugs` dataset, we ask whether the respondents were evenly split
between genders.

Put `Gender` into **Variables**, leaving **Values are counts** unticked since
the data are one row per person.

Leave **Test value** at 0.5, and tick **Confidence intervals** — the interval
around the observed proportion tells you far more than the p-value about
whether any imbalance is worth taking seriously. Tick **Bayes factor** as well
if you want to be able to say the data support an even split, rather than
merely failing to contradict it.

## References

- Clopper, C. J., & Pearson, E. S. (1934). The use of confidence or fiducial
  limits illustrated in the case of the binomial. *Biometrika, 26*(4), 404–413.
- Jeffreys, H. (1961). *Theory of Probability* (3rd ed.). Oxford University
  Press.
