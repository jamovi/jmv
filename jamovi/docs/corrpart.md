# Partial Correlation

## Overview

Measures the relationship between two variables while holding one or more other
variables constant. It answers the question a plain correlation cannot: is this
association still there once the obvious alternative explanation is removed?

A **partial** correlation removes the control variables from *both* variables
being correlated. A **semipartial** correlation removes them from only one,
which changes what the number means — see Correlation Type below.

## When to use it

Use this when you suspect a correlation is explained by something else — an
association between two measures that both increase with age, or two test
scores that both reflect general ability.

- With nothing to control for, use **Correlation Matrix**.
- To model one outcome from several predictors, use **Linear Regression** —
  each of its coefficients is a partial relationship, and it does more besides.
- To adjust group comparisons for a covariate rather than adjust a correlation,
  use **ANCOVA**.

Controlling for a variable is a claim about how the world works, not a neutral
technical step. Controlling for something that lies *on* the causal path
between your two variables will remove the very effect you are looking for.

## Assumptions

The same as Pearson's correlation, extended to the control variables:

- relationships between all variables involved are linear;
- the variables are roughly normally distributed;
- observations are independent.

Because the control variables are removed by linear regression, a non-linear
relationship with a control will not be fully removed — leaving some of the
association you thought you had eliminated. Use the rank-based coefficients if
linearity is doubtful.

Each control variable costs a degree of freedom, so controlling for many
variables in a small sample produces unstable estimates.

## Options

### Variables

The variables to correlate with one another. Every pair among them is reported.

### Control Variables

The variables to hold constant. These are removed from the relationships in
**Variables**, and are not correlated with each other in the output.

### Correlation Coefficients

**Pearson** (the default), **Spearman** and **Kendall's tau-b**, as in the
Correlation Matrix. The rank-based options are the ones to use with ordinal
data or when outliers are a concern.

### Correlation Type

- **Partial** — the control variables are removed from *both* variables. The
  result is the correlation between the two residuals: the relationship that
  remains among people matched on the controls.
- **Semipartial** — the controls are removed from only one of the two. The
  result is the correlation between one full variable and the other's residual,
  and it is interpretable as the *unique* contribution of that variable.

Partial correlations are generally larger than semipartial ones for the same
data. Semipartial is what corresponds to the increase in R² when a predictor is
added to a regression.

### Hypothesis

**Correlated** is the two-tailed default; the positive and negative options
give one-tailed tests, and should only be used when the direction was predicted
in advance.

### Additional Options

- **Report significance** — p-values for each correlation.
- **Flag significant correlations** — marks them with asterisks.
- **N** — the number of cases each correlation rests on.

## Results

### Correlation

A matrix of the variables in **Variables**, with each cell holding the
correlation between that pair after the control variables have been removed,
plus any additional statistics requested.

Compare these against the plain correlations from a Correlation Matrix on the
same variables. The comparison is the point of the analysis: a coefficient that
drops sharply once controls are added was substantially explained by them, and
one that barely moves was not.

Degrees of freedom are reduced by the number of control variables, so p-values
are slightly less generous than the raw correlation's.

## Example

Using the `bugs` dataset, we ask whether ratings of low-disgust and high-disgust
bugs are related beyond a general tendency to rate everything highly.

Put `LDLF` and `HDLF` into **Variables**, and `LDHF` into **Control
Variables**.

Leave **Partial** selected and tick **N**. Then run a **Correlation Matrix** on
`LDLF` and `HDLF` without the control and compare: the drop between the two
coefficients is how much of the original association was shared with the
control variable.

## References

- Fisher, R. A. (1924). The distribution of the partial correlation
  coefficient. *Metron, 3*, 329–332.
- Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). *Applied Multiple
  Regression/Correlation Analysis for the Behavioral Sciences* (3rd ed.).
  Erlbaum.

The partial and semipartial correlations are computed within jmv itself, not
by an external package.
