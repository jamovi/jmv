# Correlation Matrix

## Overview

Measures how strongly pairs of variables move together, for every pair among
the variables you supply. Each correlation runs from −1 to +1: zero means no
consistent relationship, and the sign says whether high values on one variable
go with high or low values on the other.

Three coefficients are available. **Pearson** measures linear association
between continuous variables. **Spearman** and **Kendall's tau-b** work on
ranks instead, which makes them suitable for ordinal data and robust to
outliers and non-linear-but-consistent relationships.

## When to use it

Use this when you want to see how several variables relate to one another,
without singling one out as the outcome.

- To predict one variable *from* the others, use **Linear Regression**.
- To measure a relationship while holding other variables constant, use
  **Partial Correlation**.
- If both variables are categorical, use **Contingency Tables**.
- To check whether a set of items hangs together as a scale, use **Reliability
  Analysis**.

Correlation is not causation, and the reminder is worth taking seriously here
because the matrix invites scanning for significant cells. A matrix of ten
variables contains 45 correlations; at the conventional threshold, two or three
will look significant by chance alone.

## Assumptions

For **Pearson**:

- the relationship is linear — a strong curved relationship can produce a
  correlation near zero;
- both variables are roughly normally distributed;
- observations are independent.

Pearson is highly sensitive to outliers: a single extreme point can create or
destroy a correlation. Always look at the scatterplots before trusting the
numbers.

**Spearman** and **Kendall's tau-b** need only that the variables are ordinal
and that the relationship is monotonic. Neither assumes normality, and both are
far less affected by outliers.

## Options

### Correlation Coefficients

- **Pearson** — the default; linear association between continuous variables.
- **Spearman** — Pearson's coefficient computed on ranks.
- **Kendall's tau-b** — also rank-based, but built from counting pairs of
  observations that agree in their ordering against those that disagree. More
  conservative than Spearman — values are typically smaller for the same data —
  and better behaved with many ties.

Tick more than one to compare. A large gap between Pearson and Spearman is
informative: it usually means outliers or a non-linear relationship.

Tick more than one to compare. A large gap between Pearson and Spearman is
informative: it usually means outliers or a non-linear relationship.

### Hypothesis

**Correlated** is the two-tailed default. **Correlated positively** and
**Correlated negatively** give one-tailed tests, and should only be chosen if
the direction was predicted before seeing the data.

### Additional Options

- **Report significance** — p-values for each correlation.
- **Flag significant correlations** — marks them with asterisks.
- **N** — the number of cases each correlation is based on. Worth showing when
  data are missing, since different cells can rest on different numbers of
  cases.
- **Confidence intervals** — an interval for each correlation, at the width set
  by **Interval**. More informative than a p-value: it shows how precisely the
  correlation is estimated.

### Plot

**Correlation matrix** draws a scatterplot for every pair, which is the
fastest way to spot the non-linearity and outliers the coefficients hide.
**Densities for variables** adds each variable's distribution along the
diagonal, and **Statistics** prints the coefficients onto the plot.

## Results

### Correlation Matrix

A square matrix with one row and column per variable, and the requested
statistics in each cell. Only the lower triangle is meaningful — the matrix is
symmetric, and the diagonal is always 1.

Read the size of the coefficients, not only their significance. With a large
sample, a correlation of .08 can be significant while explaining well under one
percent of the variance. Squaring the coefficient gives the proportion of
variance shared.

### Plot

The scatterplot matrix requested above. Check each panel for curvature and for
isolated points far from the rest — both change how the corresponding
coefficient should be read.

## Example

Using the `iris` dataset, we ask how the four flower measurements relate to one
another.

Put `Sepal.Length`, `Sepal.Width`, `Petal.Length` and `Petal.Width` into
**Variables**.

Tick **Confidence intervals** alongside the default Pearson coefficient, and
tick **Correlation matrix** under Plot. The scatterplots are the point of the
exercise here: the sepal-width relationships look odd because the dataset
contains three species, and pooling them across groups distorts the
correlations — something no coefficient in the table would reveal.

## References

- Pearson, K. (1895). Notes on regression and inheritance in the case of two
  parents. *Proceedings of the Royal Society of London, 58*, 240–242.
- Kendall, M. G. (1938). A new measure of rank correlation. *Biometrika,
  30*(1/2), 81–93.
