# MANCOVA

## Overview

Tests several continuous outcomes at once against one or more categorical
factors, optionally adjusting for covariates. Rather than running a separate
ANOVA per outcome, it asks whether the factors affect the outcomes *taken
together*.

This matters for two reasons. It controls the error rate that running many
separate tests would inflate, and it can detect effects that no single outcome
shows on its own, because it takes the correlations between the outcomes into
account.

## When to use it

Use this analysis when you have several related outcome measures — subscales of
one instrument, several symptoms, several performance measures — and it makes
sense to treat them as facets of one thing.

- With a single outcome variable, use **ANCOVA**, or **ANOVA** if there is no
  covariate.
- If the same people were measured repeatedly on *one* outcome, that is
  **Repeated Measures ANOVA**, not this.

The outcomes should be conceptually related. Bundling unrelated measures into a
MANCOVA to avoid multiple comparisons produces a test whose significance is
hard to interpret, because there is no coherent thing the factors are
affecting.

## Assumptions

- Observations are independent of one another.
- The outcomes are **multivariate normal** — a stronger requirement than each
  one being normal separately.
- The covariance matrices of the outcomes are equal across groups. This is the
  multivariate counterpart of homogeneity of variance, and is what Box's M
  tests.
- Covariates are linearly related to the outcomes, with the same slopes across
  groups.

Box's M is very sensitive, particularly to non-normality, and will often be
significant in samples where the analysis is perfectly usable. With roughly
equal group sizes the multivariate tests are fairly robust to violations;
Pillai's Trace is the most robust of the four.

## Options

### Dependent Variables

The continuous outcomes to test together. Two or more.

### Factors

The categorical predictors.

### Covariates

Continuous variables to adjust for. Adding them is what makes this MANCOVA
rather than MANOVA.

### Multivariate Statistics

Four tests of the same hypothesis, differing in how they combine the outcomes.
All four are reported by default.

| Statistic | Notes |
|---|---|
| Pillai's Trace | The most robust to violated assumptions; the safe default. |
| Wilks' Lambda | The most commonly reported. Smaller values mean stronger effects. |
| Hotelling's Trace | Similar power to Wilks in most situations. |
| Roy's Largest Root | The most powerful when the effect lies along a single dimension, the least robust otherwise. |

Roy's uses only the strongest dimension and discards the rest, so it is the one
most likely to disagree with the others — and when it alone is significant,
that is a reason for caution rather than celebration.

They usually agree. When they disagree, prefer Pillai's Trace, and note the
disagreement rather than picking the one that suits.

### Assumption Checks

- **Box's M test** — tests equality of the covariance matrices across groups.
  Read it with the caution above.
- **Shapiro-Wilk test** — tests multivariate normality.
- **Q-Q plot** — a multivariate Q-Q plot; points close to the diagonal indicate
  multivariate normality.

See [checking assumptions](topics/assumption-checks.md) for why these tests
mislead in very small and very large samples, and what to do when one fails.

## Results

### Multivariate Tests

One block per model term, with the requested multivariate statistics, their
approximate F, degrees of freedom and p-values.

This is the test to read first. A significant term means the outcomes, taken
together, differ across the levels of that factor.

### Univariate Tests

A separate ANOVA for each outcome variable, in the conventional order: consult
these only after the multivariate test is significant, to see which outcomes
are driving it.

These p-values are not corrected for the number of outcomes. The multivariate
test acts as the gatekeeper, but if you intend to make claims about individual
outcomes, correct them or say plainly that you have not.

### Assumption Checks

Box's M and Shapiro-Wilk. A *large* p-value is the reassuring one. Box's M in
particular should not be treated as a pass/fail gate.

## Example

Using the `iris` dataset, we ask whether the three species differ across flower
dimensions considered together.

Put `Sepal.Length`, `Sepal.Width`, `Petal.Length` and `Petal.Width` into
**Dependent Variables**, and `Species` into **Factors**.

Leave all four multivariate statistics ticked and check they agree. Under
Assumption Checks tick **Box's M test** — it will almost certainly be
significant here, which is a good illustration of why Pillai's Trace is the one
to lean on. Then read the Univariate Tests to see which dimensions separate the
species most sharply.

## References

- Pillai, K. C. S. (1955). Some new test criteria in multivariate analysis.
  *The Annals of Mathematical Statistics, 26*(1), 117–121.
- Olson, C. L. (1976). On choosing a test statistic in multivariate analysis of
  variance. *Psychological Bulletin, 83*(4), 579–586.

The multivariate Shapiro-Wilk test is computed with mvnormtest.

- Jarek, S. (2024). *mvnormtest: Normality Test for Multivariate Variables* [R
  package].
