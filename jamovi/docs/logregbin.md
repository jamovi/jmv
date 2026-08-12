# Binomial Logistic Regression

## Overview

Predicts a binary outcome — yes/no, passed/failed, survived/died — from one or
more predictors, which may be continuous, categorical, or both.

Rather than predicting the outcome directly, the model predicts the
**probability** of one of the two levels, and does so on a scale that keeps the
prediction between 0 and 1. The cost of that is interpretation: coefficients
come out as log odds, which is why the odds ratio option exists. Underneath it
is the [same linear model](topics/linear-models.md) as ordinary regression,
predicting a transformation of the outcome rather than the outcome itself.

## When to use it

Use this when your outcome has exactly two possible values.

- If the outcome is continuous, use **Linear Regression**.
- If it has three or more unordered categories, use **Multinomial Logistic
  Regression**; if they are ordered, **Ordinal Logistic Regression**.
- If both outcome and predictors are categorical and you only want to test
  their association, **Contingency Tables** is simpler.

Which level counts as "success" is set by the reference level, and it flips the
direction of every coefficient. Check it before interpreting anything.

## Assumptions

Logistic regression drops several of linear regression's assumptions — the
outcome is not normal, residuals are not normal, and variance is not constant
by design. What remains:

- observations are independent of one another;
- each continuous predictor is linearly related to the **log odds** of the
  outcome, which is what the Box-Tidwell test checks;
- no severe multicollinearity among predictors.

Two practical requirements matter as much as the formal assumptions. Logistic
regression needs a reasonable number of cases in *both* outcome categories —
rare outcomes produce unstable estimates. And **complete separation**, where a
predictor perfectly predicts the outcome, causes coefficients to run off toward
infinity; huge estimates with huge standard errors are the symptom.

## Options

### Dependent Variable

The binary outcome, as a nominal variable with two levels.

### Covariates and Factors

Continuous predictors go into **Covariates**, categorical ones into
**Factors**.

### Model Builder

Assigns predictors to **Blocks**, fitting a sequence of nested models compared
against one another. Put control variables in the first block and the
predictors of interest in a later one to test whether they add anything beyond
the controls. Interaction terms are built here.

### Reference Levels

Sets the baseline level for the outcome and for each factor. For the outcome
this determines which level the model predicts the probability *of* — get it
wrong and every odds ratio inverts.

### Model Fit

**Fit Measures** — **Deviance** (−2 log likelihood, lower is better), **AIC**
and **BIC** for comparing models, and an **Overall model test** against a model
with no predictors.

**Pseudo R²** offers four measures. None is the proportion of variance
explained — that quantity does not exist here — so treat them as relative
rather than absolute:

| Measure | Notes |
|---|---|
| McFadden's R² | The default. Values run far lower than a linear R²; McFadden put .2–.4 as excellent fit. |
| Cox & Snell's R² | Cannot reach 1, even for a perfect model. |
| Nagelkerke's R² | Cox & Snell rescaled so 1 is attainable. |
| Tjur's R² | The difference in mean predicted probability between the two outcome groups. The most intuitive of the four. |

### Model Coefficients

- **Omnibus Tests** — likelihood ratio tests, one per predictor rather than one
  per dummy variable. This is what you want for factors with several levels.
- **Estimate (Log Odds Ratio)** — the raw coefficients, optionally with a
  **Confidence interval**.
- **Odds Ratio** — the same coefficients exponentiated, optionally with a
  confidence interval. Almost always the ones to report.

An odds ratio of 1 means no effect; above 1 means the odds of the outcome rise
with the predictor, below 1 that they fall. Note that the null value is 1, not
0, so a confidence interval indicates significance by excluding 1.

### Estimated Marginal Means

Model-predicted probabilities for the levels of a term, averaging over the
other predictors, as **tables** or **plots**. These are on the probability
scale rather than the log-odds scale, which makes them the most readable output
the analysis produces. They are model predictions rather than observed
proportions; see
[marginal means and post hoc tests](topics/marginal-means-and-post-hoc.md).

### Prediction

- **Cut-off value** — the predicted probability above which a case is
  classified as the outcome. The default of 0.5 is not always right; with a
  rare outcome it may classify nothing at all.
- **Classification table** — predicted against observed outcomes.
- **Accuracy**, **Specificity**, **Sensitivity** — the proportions correct
  overall, among true negatives, and among true positives. Report sensitivity
  and specificity rather than accuracy alone: with a rare outcome, a model that
  never predicts it can still be 95% accurate.
- **AUC** and **ROC curve** — the area under the ROC curve summarises
  performance across *all* cut-offs. 0.5 is chance, 1 is perfect.
- **Cut-off plot** — how the predictive measures trade off as the cut-off
  moves.

### Assumption Checks

- **Collinearity statistics** — VIF and tolerance per predictor.
- **Box-Tidwell test** — tests linearity of the continuous predictors against
  the log odds.
- **Cook's distance** — identifies cases that disproportionately influence the
  fit.

Logistic regression drops the normality and constant-variance assumptions, so
the checks here differ from the linear model's; see
[checking assumptions](topics/assumption-checks.md) for the ones that remain.

### Save

Writes **Predicted values**, **Residuals** and **Cook's distance** back to the
spreadsheet as new columns.

## Results

### Model Fit Measures

One row per model with the requested fit measures and pseudo R². With several
blocks, one row per successive model.

### Model Comparisons

Present with more than one block. Each row tests whether adding that block
improved the model, via a likelihood ratio test.

### Model Specific Results

Per model: the omnibus likelihood ratio tests, and the coefficients table with
log odds and odds ratios.

Read the odds ratios, and read them relative to 1. For a continuous predictor,
an odds ratio of 1.5 means the odds multiply by 1.5 for each one-unit increase
— which depends entirely on the predictor's units, so a "small" odds ratio on a
wide-ranging variable can still be a large effect.

Prediction output and marginal means appear here too.

### Predicted values, Residuals, Cook's distance

The columns written back to the spreadsheet, if requested.

## Example

Using the `bugs` dataset, we ask whether reactions to bugs predict a
respondent's gender.

Put `Gender` into **Dependent Variable**, and `LDLF`, `LDHF`, `HDLF` and `HDHF`
into **Covariates**.

Check **Reference Levels** first so you know which gender the model is
predicting. Tick **Odds ratio** with its **Confidence interval** — these, not
the log odds, are what you would report. Then tick **Classification table** and
**AUC** under Prediction to see whether the model does better than guessing.

## References

- McFadden, D. (1979). Quantitative methods for analysing travel behaviour of
  individuals. In D. A. Hensher & P. R. Stopher (Eds.), *Behavioural Travel
  Modelling* (pp. 279–318). Croom Helm.
- Hosmer, D. W., Lemeshow, S., & Sturdivant, R. X. (2013). *Applied Logistic
  Regression* (3rd ed.). Wiley.
- Tjur, T. (2009). Coefficients of determination in logistic regression models.
  *The American Statistician, 63*(4), 366–372.

Likelihood ratio tests and collinearity statistics are computed with car, ROC
analysis with ROCR, and estimated marginal means with emmeans.

- Fox, J., Weisberg, S., & Price, B. (2026). *car: Companion to Applied
  Regression* [R package].
- Sing, T., Sander, O., Beerenwinkel, N., Lengauer, T., Unterthiner, T., &
  Ernst, F. G. M. (2026). *ROCR: Visualizing the Performance of Scoring
  Classifiers* [R package].
- Lenth, R., & Piaskowski, J. (2026). *emmeans: Estimated Marginal Means, aka
  Least-Squares Means* [R package].
