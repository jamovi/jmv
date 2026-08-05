# Ordinal Logistic Regression

## Overview

Predicts an outcome whose categories have a natural order — low/medium/high,
disagree/neutral/agree, the stages of a condition — from one or more
predictors.

The model uses that ordering. Instead of comparing each category against a
reference, it models the odds of falling at or below each point on the scale,
and assumes a predictor shifts those odds by the same amount at every point.
The result is **one coefficient per predictor** rather than one per category,
which makes the output far smaller and easier to read than a multinomial
model's.

## When to use it

Use this when the outcome is categorical, ordered, and you are unwilling to
treat the spacing between categories as equal.

- If the categories have no order, use **Multinomial Logistic Regression**.
- With exactly two categories, use **Binomial Logistic Regression** — ordering
  is meaningless with two.
- If you are willing to treat an ordered scale as continuous — a common and
  often defensible choice for a long rating scale — **Linear Regression** is
  simpler and more familiar to readers.

The main reason to choose this over linear regression is that it makes no claim
that the gap between "disagree" and "neutral" equals the gap between "neutral"
and "agree".

## Assumptions

- Observations are independent of one another.
- Each continuous predictor is linearly related to the log odds.
- No severe multicollinearity among predictors.
- **Proportional odds**: each predictor's effect is the same at every threshold
  of the outcome scale.

Proportional odds is the assumption that buys the compact output, and the one
to think hardest about. If a predictor strongly separates the bottom category
from the rest but does nothing higher up the scale, the assumption fails and
the single coefficient reported here averages over genuinely different effects.
jamovi does not provide a test for it; comparing the fit against a multinomial
model on the same data is one practical check.

The outcome must be an ordinal variable, with its levels in the correct order.
Check that in the Data tab first — the ordering drives the whole model.

## Options

### Dependent Variable

The ordered categorical outcome, as an ordinal variable.

### Covariates and Factors

Continuous predictors go into **Covariates**, categorical ones into
**Factors**.

### Model Builder

Assigns predictors to **Blocks**, fitting nested models compared against one
another. Controls in the first block, predictors of interest in a later one.
Interaction terms are built here.

### Reference Levels

Sets the baseline level for each factor. Note there is no reference level for
the outcome here — the model uses its ordering instead, which is what
distinguishes it from the multinomial case.

### Model Fit

**Fit Measures** — **Deviance**, **AIC**, **BIC**, and an **Overall model
test** against a model with no predictors.

**Pseudo R²** offers McFadden's, Cox & Snell's, Nagelkerke's and Tjur's. None
is a proportion of variance explained; McFadden's is the default, and its
values run much lower than a linear R².

### Model Coefficients

- **Omnibus Tests** — likelihood ratio tests, one per predictor rather than one
  per dummy variable.
- **Model thresholds** — the cut-points between adjacent outcome categories on
  the underlying scale. These are part of the model rather than results to
  interpret; they position the categories, and are rarely reported.
- **Estimate (Log Odds Ratio)** — raw coefficients, optionally with a
  **Confidence interval**.
- **Odds Ratio** — the exponentiated coefficients, optionally with a confidence
  interval. Usually what you report.

An odds ratio here is the multiplicative change in the odds of being in a
*higher* outcome category per unit of the predictor — and the proportional odds
assumption is what lets that single number stand for every threshold.

## Results

### Model Fit Measures

One row per model with the requested fit measures and pseudo R².

### Model Comparisons

Present with more than one block; each row tests whether adding that block
improved the model, via a likelihood ratio test.

### Model Specific Results

Per model: the omnibus likelihood ratio tests, the thresholds if requested, and
the coefficients table.

Each coefficient applies across the whole outcome scale. A positive coefficient
means higher values of the predictor go with higher outcome categories. Because
there is only one coefficient per predictor, this table reads much like a
linear regression's — which is the practical advantage of the model, provided
proportional odds holds.

## Example

Using the `bugs` dataset, we ask whether reactions to bugs predict a
respondent's level of education.

Put `Education` into **Dependent Variable** — check in the Data tab that it is
an ordinal variable and that its levels run in the right order, since the whole
model depends on that. Put `LDLF`, `LDHF`, `HDLF` and `HDHF` into
**Covariates**.

Tick **Odds ratio** with its **Confidence interval**, and **Likelihood ratio
tests** under Omnibus Tests. Leave **Model thresholds** off unless you need
them; they are part of the machinery rather than a result.

## References

- McCullagh, P. (1980). Regression models for ordinal data. *Journal of the
  Royal Statistical Society: Series B, 42*(2), 109–142.
- Agresti, A. (2010). *Analysis of Ordinal Categorical Data* (2nd ed.). Wiley.
