# Paired Samples Contingency Tables

## Overview

Cross-tabulates two categorical measurements taken from the same people, and
tests whether the distribution changed between them, using McNemar's test.

The key difference from an ordinary contingency table is what the test looks
at. McNemar's test ignores the cases that gave the same answer both times and
examines only those that changed — the off-diagonal cells. Agreement carries no
information about change, so only the disagreements count.

## When to use it

Use this when each person contributes two categorical measurements: an opinion
before and after an intervention, a diagnosis by two raters, a behaviour at two
time points.

- If the two variables are measured on *different* people, use **Contingency
  Tables**.
- If the same people gave a *continuous* measurement twice, use **Paired
  Samples T-Test**.
- To measure agreement between raters rather than test for change, a kappa
  statistic is what you want; this analysis tests whether the marginal
  distributions differ, which is a different question.

Using an ordinary contingency table for paired data treats each person's two
answers as independent observations, which they are not. The result is a test
that is both wrong and usually less powerful than the correct one.

## Assumptions

- Pairs are independent of one another, though the two measurements within a
  pair are not.
- Both variables have the same categories, in the same order — the table must
  be square.
- The discordant cells hold enough cases. McNemar's χ² is an approximation; use
  the exact version when the number of changes is small.

There is no requirement that the two measurements be independent, which is the
whole point.

## Options

### Rows and Columns

The two categorical measurements. Both must have the same levels — typically
the same question asked twice.

### Counts (optional)

If your data are already summarised as one row per cell with a frequency, put
that frequency variable here. Otherwise leave it empty.

### Statistics

- **χ²** — McNemar's test statistic.
- **χ² continuity correction** — a more conservative version, for small numbers
  of discordant pairs.
- **Log odds ratio exact** — an exact test, appropriate when the discordant
  cells hold few cases and the χ² approximation cannot be trusted.

### Percentages

**Row** and **Column** percentages. As with any contingency table, choose the
direction that matches the question you are asking.

## Results

### Contingency Tables

The square cross-tabulation of the two measurements, with any percentages
requested.

Look at the off-diagonal cells. The diagonal holds the people who gave the same
answer twice and contributes nothing to the test; everything McNemar's test
uses is off it. If the two off-diagonal counts are similar, there is no
systematic change however large they are.

### McNemar Test

The test statistic, degrees of freedom and p-value.

A small p-value means change was systematic — more people moved one way than
the other. It says nothing about how many people changed in total, which can be
large even when the movement is balanced.

## Example

Using a dataset in which the same people answered a yes/no question before and
after an intervention, we ask whether the intervention shifted opinion.

Put the before-measurement into **Rows** and the after-measurement into
**Columns**, leaving **Counts** empty if the data are one row per person.

Tick **χ²** for McNemar's test. Read the two off-diagonal cells first: those
are the people who changed their mind, and the test is asking whether the two
directions of change are balanced. If only a handful of people changed, tick
**Log odds ratio exact** rather than relying on the χ² approximation.

## References

- McNemar, Q. (1947). Note on the sampling error of the difference between
  correlated proportions or percentages. *Psychometrika, 12*(2), 153–157.
- Agresti, A. (2013). *Categorical Data Analysis* (3rd ed.). Wiley.

The exact test is computed with exact2x2.

- Fay, M. P., Hunsberger, S. A., Nason, M., Gabriel, E., & Lumbard, K. (2025).
  *exact2x2: Exact Tests and Confidence Intervals for 2x2 Tables* [R package].
