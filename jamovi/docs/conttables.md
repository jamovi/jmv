# Contingency Tables

## Overview

Cross-tabulates two categorical variables and tests whether they are
associated — whether the distribution of one differs across the levels of the
other.

The χ² test compares the counts you observed against the counts expected if the
two variables were unrelated. A large χ² means the table departs from that
pattern; it does not say where or how, which is what the percentages, residuals
and comparative measures are for.

## When to use it

Use this when both variables are categorical and each person appears once in
the table.

- If the two variables are repeated measurements of the same thing on the same
  people — before and after, two raters — use **Paired Samples Contingency
  Tables**. Using this analysis instead violates its independence assumption.
- With a single categorical variable and no second one to cross it against, use
  **Proportion Test (N Outcomes)** or **Proportion Test (2 Outcomes)**.
- With more than two variables, or to model the counts themselves, use
  **Log-Linear Regression**.
- If one variable is continuous, you want a t-test or **ANOVA**, not this.

## Assumptions

- Each observation is independent, and each case contributes to exactly one
  cell.
- Expected counts are large enough. The usual rule is that all expected counts
  should exceed 5, or at least that no more than 20% fall below 5.

When expected counts are too small, use **Fisher's exact test**, which makes no
such assumption. jamovi flags nothing automatically here, so tick **Expected
counts** and look.

The test is sensitive to sample size in both directions: in a very large table
a trivial association will be significant, and in a small one a substantial
association may not be. Read the effect sizes — Cramer's V, the odds ratio —
alongside the p-value.

## Options

### Rows, Columns and Counts

**Rows** and **Columns** take the two categorical variables. If your data are
already summarised — one row per cell with a frequency — put the frequency
variable into **Counts (optional)**; otherwise leave it empty and jamovi counts
the rows itself. Supplying a counts variable adds a notice above the results
saying the data are weighted by it, which is confirmation rather than a
problem.

**Layers** splits the table by further variables, producing a separate
contingency table per combination. This is how to check whether an association
holds within subgroups.

### Statistics

**Tests**:

| Test | Notes |
|---|---|
| χ² | The standard test of association. |
| χ² continuity correction | Yates's correction, for 2×2 tables. Conservative, and increasingly regarded as unnecessary. |
| Likelihood ratio | An alternative to χ² that usually agrees closely. |
| Fisher's exact test | Exact rather than approximate; the right choice when expected counts are small. |

**z test for difference in 2 proportions** sits slightly apart from the four
above. Rather than testing whether the two variables are associated, it
compares two proportions directly, and is available for 2×2 tables only.

**Comparative Measures (2×2 only)** — available only for 2×2 tables:
**Difference in proportions**, **Log odds ratio**, **Odds ratio** and
**Relative risk**, with optional **Confidence intervals**. **Compare** chooses
whether rows or columns are compared, and **Alternative hypothesis** allows
one-tailed tests.

Odds ratio and relative risk answer different questions and are often confused.
Relative risk is the ratio of probabilities and is the more intuitive; the odds
ratio is the ratio of odds, is what logistic regression estimates, and is
further from 1 than the relative risk whenever the outcome is common.

**Nominal** effect sizes — **Contingency coefficient** and **Phi and Cramer's
V**. Cramer's V is the general-purpose choice, running from 0 to 1 regardless
of table size.

**Ordinal** measures — **Gamma** and **Kendall's tau-b** — apply when both
variables are ordered, and test for a monotonic trend rather than for
association in general. **Mantel-Haenszel** tests specifically for a linear
trend.

Using an ordinal measure on ordered data is more powerful than χ², which
ignores the ordering entirely.

### Cells

**Counts** — **Observed counts** are shown by default; **Expected counts** are
what you would see under independence, and are worth ticking to check the
assumption above.

**Percentages** — **Row**, **Column** and **Total**. Choose deliberately: row
percentages answer "of the people in this row, what fraction fell in each
column?", and column percentages answer the reverse. Picking the one that
matches your question is usually what makes the table readable.

### Post Hoc Tests

Residuals showing which cells depart most from independence — the "where" that
χ² does not answer.

- **Unstandardized residuals** — observed minus expected, in raw counts.
- **Pearson residuals** — scaled by the expected count.
- **Standardized residuals (adjusted Pearson)** — the ones to use. They are
  approximately standard normal, so a value beyond about ±2 marks a cell
  contributing unusually to the result.
- **Deviance residuals (Poisson GLM)** — an alternative scaling.

Each has a **Highlight values above** threshold that marks the cells exceeding
it.

### Plots

**Bar Plot** draws the table, with **Y-axis** as counts or percentages,
**X-axis** choosing which variable runs along the bottom, and **Bar Type**
selecting side-by-side or stacked bars. Stacked bars with percentages are
usually the clearest way to show that a distribution differs across groups.

## Results

### Contingency Tables

The cross-tabulation itself, with whichever counts and percentages you
requested, and row, column and grand totals.

### χ² Tests

The requested tests with their statistics, degrees of freedom and p-values. A
small p-value means the two variables are associated — not how strongly, and
not where.

### Comparative Measures

Odds ratios, relative risks and differences in proportions for 2×2 tables, with
confidence intervals. For ratios the null value is 1, so significance shows as
an interval excluding 1 rather than 0.

### Nominal

Contingency coefficient, phi and Cramer's V — the effect sizes to report
alongside a significant χ².

### Gamma, Kendall's Tau-b, Mantel-Haenszel Test for Trend

Ordinal measures, meaningful only when both variables are ordered.

### Post Hoc Tests

The residuals requested above, in table form. Read the standardized residuals:
cells beyond ±2 are the ones driving a significant χ².

### Plots

The bar plot requested above.

## Example

Using the `bugs` dataset, we ask whether education level is distributed
differently across regions.

Put `Region` into **Rows** and `Education` into **Columns**, leaving **Counts**
empty since the data are one row per person.

Tick **Expected counts** to check none are too small, and **Row** percentages
so each region's education profile is directly comparable. Add **Phi and
Cramer's V** for an effect size. If the χ² is significant, tick **Standardized
residuals (adjusted Pearson)** to see which cells are responsible.

## References

- Pearson, K. (1900). On the criterion that a given system of deviations from
  the probable... *Philosophical Magazine, 50*(302), 157–175.
- Agresti, A. (2013). *Categorical Data Analysis* (3rd ed.). Wiley.

Nominal effect sizes and odds ratios are computed with vcd, and gamma with
vcdExtra.

- Meyer, D., Zeileis, A., Hornik, K., Gerber, F., & Friendly, M. (2024). *vcd:
  Visualizing Categorical Data* [R package].
- Friendly, M., Turner, H., Zeileis, A., Murdoch, D., & Firth, D. (2026).
  *vcdExtra: 'vcd' Extensions and Additions* [R package].
