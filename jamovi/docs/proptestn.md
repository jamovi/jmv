# Proportion Test (N Outcomes)

## Overview

Tests whether the observed counts across the levels of a single categorical
variable match a set of expected proportions. This is the χ² goodness of fit
test.

By default it tests whether the levels are equally frequent. You can also
specify any expected proportions you like — a known population distribution, a
theoretical prediction, the results of an earlier study.

## When to use it

Use this when you have one categorical variable and an expectation about how
its levels should be distributed.

- With exactly two levels, **Proportion Test (2 Outcomes)** gives an exact test
  rather than an approximate one, and is the better choice.
- To compare the distribution of one variable *across the levels of another*,
  use **Contingency Tables**.
- Simply to see the counts without testing anything, use **Descriptives** with
  frequency tables.

## Assumptions

- Observations are independent, and each falls into exactly one level.
- Expected counts are large enough — the usual rule is that all should exceed
  5, or that no more than 20% fall below 5.

The expected-count rule matters more here than elsewhere, because there is no
exact alternative offered in this analysis. With a level that is expected to be
rare, consider combining categories.

The test is sensitive to sample size: with enough data, a trivial departure
from the expected proportions will be significant. Look at the size of the
discrepancy in the proportions table, not only at the p-value.

## Options

### Variable

The categorical variable to test.

### Counts (optional)

If your data are already summarised — one row per level with a frequency — put
the frequency variable here. Otherwise leave it empty and jamovi counts the
rows.

### Expected counts

Adds the counts expected under the hypothesis to the output. Worth ticking:
it is how you check the assumption above, and it makes the comparison the test
is performing visible.

### Expected Proportions

Sets the proportions to test against, one **Ratio** per **Level**. These are
ratios rather than proportions, so entering 1, 1, 2 tests whether the third
level is twice as common as each of the others; jamovi converts them to
proportions summing to 1.

Leaving every ratio equal — the default — tests whether all levels are equally
frequent.

## Results

### Proportions

One row per level, with its observed count, the expected count if requested,
and the observed proportion.

This is where you see *what* differs. A significant χ² means the distribution
departs from expectation somewhere; comparing observed against expected counts
row by row is what tells you where.

### χ² Goodness of Fit

The χ² statistic, its degrees of freedom and a p-value.

A small p-value means the observed distribution differs from the expected one.
It does not say which levels are responsible, and it does not indicate how
large the departure is — with a large sample, a small one will register.

## Example

Using the `bugs` dataset, we ask whether respondents were drawn evenly from the
different regions.

Put `Region` into **Variable**, leaving **Counts** empty since the data are one
row per person.

Tick **Expected counts**. Leave the ratios under **Expected Proportions** equal
to test for an even split — then compare the observed and expected counts row
by row to see which regions are over- or under-represented, since the χ² test
itself will not tell you.

## References

- Pearson, K. (1900). On the criterion that a given system of deviations from
  the probable... *Philosophical Magazine, 50*(302), 157–175.
- Agresti, A. (2013). *Categorical Data Analysis* (3rd ed.). Wiley.

The goodness of fit test uses R's own implementation.

- R Core Team (2026). *A Language and Environment for Statistical Computing*
  (Version 4.6) [Computer software].
