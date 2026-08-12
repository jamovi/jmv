# Multinomial Logistic Regression

## Overview

Predicts an outcome with three or more **unordered** categories — a choice of
brand, a diagnosis, a preferred option — from one or more predictors.

The model works by picking one outcome category as the reference and fitting a
separate binary comparison of every other category against it. With four
outcome categories you get three sets of coefficients, and this is why the
output is larger than that of any other regression in jamovi.

## When to use it

Use this when the outcome is categorical with more than two levels and those
levels have no natural order.

- With exactly two outcome levels, use **Binomial Logistic Regression**.
- If the categories *are* ordered — low/medium/high, disagree/neutral/agree —
  use **Ordinal Logistic Regression**. It is more powerful because it uses the
  ordering, and gives one coefficient per predictor instead of several.
- If the outcome is continuous, use **Linear Regression**.

Treating ordered categories as unordered is a common and costly mistake: you
lose power and get an output several times larger for no benefit. Only use this
analysis when the categories genuinely have no sequence.

## Assumptions

- Observations are independent of one another.
- Each continuous predictor is linearly related to the log odds of each
  outcome comparison.
- No severe multicollinearity among predictors.
- **Independence of irrelevant alternatives**: the relative odds of two outcome
  categories do not depend on which other categories are available. This is the
  assumption specific to multinomial models, and it is often questionable when
  categories are close substitutes.

The model needs a reasonable number of cases in *every* outcome category, not
just overall. Because a separate comparison is fitted for each category, a
sparse category will produce unstable estimates for its whole set of
coefficients.

## Options

### Dependent Variable

The categorical outcome, as a nominal variable with three or more levels.

### Covariates and Factors

Continuous predictors go into **Covariates**, categorical ones into
**Factors**.

### Model Builder

Assigns predictors to **Blocks**, fitting nested models that are compared
against one another. Put controls in the first block and the predictors of
interest in a later one. Interaction terms are built here.

### Reference Levels

Sets the baseline level for the outcome and for each factor. The outcome's
reference level is the category everything else is compared against, so it
shapes the entire output — choose the one that makes the comparisons meaningful,
usually the most common category or a natural "none of the above".

### Model Fit

**Fit Measures** — **Deviance**, **AIC**, **BIC**, and an **Overall model
test** against a model with no predictors.

**Pseudo R²** offers McFadden's, Cox & Snell's, Nagelkerke's and Tjur's. None
is a proportion of variance explained; McFadden's is the default, and its
values run far lower than a linear R² would — McFadden put .2–.4 as excellent
fit, not the .7 a linear model might invite you to expect.

### Model Coefficients

- **Omnibus Tests** — likelihood ratio tests giving **one test per predictor**,
  across all outcome comparisons at once. In this analysis these matter more
  than elsewhere: they are the only place a predictor gets a single verdict
  rather than one per outcome category.
- **Estimate (Log Odds Ratio)** — raw coefficients, optionally with a
  **Confidence interval**.
- **Odds Ratio** — the exponentiated coefficients, optionally with a confidence
  interval. Usually what you report.

### Estimated Marginal Means

Model-predicted probabilities for each outcome category across the levels of a
term, as **tables** or **plots**.

For this analysis the plots are worth more than usual. A table of coefficients
across several outcome comparisons is hard to hold in the head; a plot of
predicted probabilities per category shows the same thing at a glance.

## Results

### Model Fit Measures

One row per model with the requested fit measures and pseudo R².

### Model Comparisons

Present with more than one block; each row tests whether adding that block
improved the model via a likelihood ratio test.

### Model Specific Results

Per model: the omnibus likelihood ratio tests, and the coefficients table.

The coefficients table is grouped by outcome category. Each block of rows
compares one category against the reference — so a coefficient is not "the
effect of this predictor" but "the effect of this predictor on the odds of
*this* category rather than the reference".

Read the omnibus tests first to see which predictors matter at all, then go to
the coefficients to see where the effect lies. Reading the coefficient table
first tends to produce a story assembled from whichever cells happened to be
significant.

## Example

Using the `bugs` dataset, we ask whether reactions to bugs differ by region of
residence.

Put `Region` into **Dependent Variable** and `LDLF`, `LDHF`, `HDLF` and `HDHF`
into **Covariates**.

Set **Reference Levels** for the outcome to the largest region, so the others
are compared against a well-estimated baseline. Tick **Likelihood ratio tests**
under Omnibus Tests — with several outcome categories this is the table that
tells you which predictors matter. Then tick **Odds ratio** and read the
coefficients to see which regions differ and how.

## References

- McFadden, D. (1979). Quantitative methods for analysing travel behaviour of
  individuals. In D. A. Hensher & P. R. Stopher (Eds.), *Behavioural Travel
  Modelling* (pp. 279–318). Croom Helm.
- Hosmer, D. W., Lemeshow, S., & Sturdivant, R. X. (2013). *Applied Logistic
  Regression* (3rd ed.). Wiley.
- Agresti, A. (2013). *Categorical Data Analysis* (3rd ed.). Wiley.

The model is fitted with nnet, likelihood ratio tests computed with car, and
estimated marginal means with emmeans.

- Ripley, B., Venables, W. (2025). *nnet: Feed-Forward Neural Networks and
  Multinomial Log-Linear Models* [R package].
- Fox, J., Weisberg, S., & Price, B. (2026). *car: Companion to Applied
  Regression* [R package].
- Lenth, R., & Piaskowski, J. (2026). *emmeans: Estimated Marginal Means, aka
  Least-Squares Means* [R package].
