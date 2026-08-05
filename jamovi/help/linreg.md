# Linear Regression

## Overview

Predicts a continuous outcome from one or more predictors, which may be
continuous, categorical, or a mixture of both. Each coefficient describes how
the outcome changes per unit of its predictor, **holding the other predictors
constant** — that last part is what makes regression more than a set of
separate correlations.

Predictors can be entered in blocks, so that several nested models are fitted
and compared. This is how you ask whether a set of predictors adds anything
beyond what you already had.

## When to use it

Use this when one variable is the outcome and the rest are predictors.

- If no variable is singled out as the outcome, use **Correlation Matrix**.
- If you only want the relationship between two variables with others held
  constant, **Partial Correlation** is more direct.
- If the outcome is binary, use **Binomial Logistic Regression**; if it is
  categorical with more levels, **Multinomial** or **Ordinal Logistic
  Regression**.
- If all your predictors are categorical, **ANOVA** fits the same model and
  presents it in the form usually expected for experiments.

ANOVA and regression are the same underlying model. Choose on the basis of
which output your readers expect, not on which is correct.

## Assumptions

- **Linearity** — each predictor's relationship with the outcome is a straight
  line.
- **Independence** — observations are independent of one another.
- **Normality of residuals** — the residuals, not the raw outcome, should be
  normally distributed.
- **Homoscedasticity** — residual variance is constant across the range of
  predicted values.

Two further conditions matter in practice. **Multicollinearity**, where
predictors are strongly correlated with each other, does not bias the model's
predictions but makes individual coefficients unstable and hard to interpret —
check it with the collinearity statistics. And **influential cases**: a single
extreme observation can shift the coefficients substantially, which is what
Cook's distance is for.

The residual plots test the first and fourth assumptions far better than any
significance test, and should be your first stop.

## Options

### Dependent Variable

The continuous outcome to be predicted.

### Covariates and Factors

Continuous predictors go into **Covariates**, categorical ones into
**Factors**. Factors are converted to dummy variables automatically, one fewer
than the number of levels.

**Weights (optional)** takes a variable of case weights, for weighted least
squares — useful when observations vary in precision.

### Model Builder

Assigns predictors to **Blocks**. With one block you get a single model. With
several, jamovi fits a sequence of nested models — block 1, then blocks 1 and 2
together, and so on — and compares them.

This is hierarchical regression, and it is the tool for asking whether a set of
predictors explains anything *beyond* the ones already entered. Put the
variables you are controlling for in the first block and the ones you care
about in the second; the model comparison then tests exactly that question.

Interaction terms are built here too, by selecting several predictors and
adding them as an interaction.

### Reference Levels

Sets which level of each factor serves as the baseline. Every coefficient for
that factor is a comparison against this level, so choosing a meaningful one —
a control group, an absence of treatment — makes the output far easier to read.

**Intercept** chooses what the intercept represents: **Reference level (dummy
coding)** makes it the predicted value when every factor is at its reference
level, while **Grand mean (simple coding)** makes it the overall mean.

### Model Fit

**Fit Measures** report how well the model does overall:

| Measure | What it says |
|---|---|
| R | The correlation between predicted and observed values. |
| R² | The proportion of outcome variance the model explains. |
| Adjusted R² | R² penalised for the number of predictors; the one to compare models on. |
| AIC, BIC | Information criteria for comparing models — lower is better. BIC penalises complexity more heavily. |
| RMSE | Average prediction error, in the units of the outcome. |

R² always rises when a predictor is added, even a useless one. Use adjusted R²,
or the model comparison, when judging whether a predictor earned its place.

**Overall Model Test** adds an F test of the whole model against one with no
predictors.

### Model Coefficients

- **Omnibus Test** — an ANOVA table giving one test per predictor rather than
  one per dummy variable. This is what you want for a factor with more than two
  levels, where the individual coefficients each test only one contrast.
- **Estimate** — the coefficients themselves, optionally with a **Confidence
  interval**.
- **Standardized Estimate** — coefficients from a model refitted on
  standardized data, so predictors measured on different scales can be
  compared, optionally with a confidence interval.

  Only continuous columns are standardized; factors are left as they are, and
  the outcome is standardized along with the predictors. So a continuous
  predictor's standardized estimate is the familiar β — the change in outcome
  SDs per predictor SD — while a factor's is the group difference expressed in
  outcome SDs only. The two are not on the same footing, despite sharing a
  column. The intercept is left blank.

### Assumption Checks

- **Normality test** — Shapiro-Wilk on the residuals.
- **Q-Q plot of residuals** — the better check of the same assumption.
- **Residual plots** — residuals against predicted values and against each
  predictor. A funnel shape indicates heteroscedasticity; curvature indicates a
  non-linear relationship the model has missed.
- **Autocorrelation test** — the Durbin-Watson statistic, testing independence
  of consecutive residuals. Relevant for data collected in sequence; values
  near 2 indicate no autocorrelation.
- **Collinearity statistics** — VIF and tolerance per predictor. A common rule
  of thumb treats VIF above 5 (or 10) as a concern, though there is no sharp
  threshold.

**Data Summary** flags unusual cases: **Cook's distance** measures how much the
model would change if a case were dropped, and **Mahalanobis distance**
identifies cases far from the centre of the predictor space, with a p-value
threshold you can set.

### Estimated Marginal Means

Model-predicted means for a term, averaging over the other predictors and
holding covariates at their means. Available as **Marginal means tables** and
**Marginal means plots**, with a **Confidence interval** and an **Equal cell
weights** option.

This is how to make a model containing factors interpretable, and how to plot
an interaction.

### Save

Writes results back to the spreadsheet as new columns: **Predicted values**,
**Residuals**, **Cook's distance** and **Mahalanobis distance**. Useful for
plotting or inspecting individual cases yourself.

## Results

### Model Fit Measures

One row per model, with the fit measures requested. With a single block there
is one row; with several, one per successive model.

### Model Comparisons

Present when there is more than one block. Each row tests whether adding that
block improved the model significantly, reporting the change in R² alongside an
F test.

This is the table hierarchical regression exists for. A significant change
means that block of predictors explains variance the earlier blocks did not.

### Model Specific Results

For each model: the **Omnibus ANOVA Test** if requested, and the **Model
Coefficients** table.

Each coefficient is the change in the outcome per one-unit increase in that
predictor, with the others held constant. For a factor, it is the difference
between that level and the reference level. The intercept is the predicted
outcome when all predictors are zero or at their reference level — which is
often not a meaningful case, so do not over-interpret it.

Assumption check output and marginal means appear here too, per model.

### Predicted values, Residuals, Cook's Distance, Mahalanobis Distance

The columns written back to the spreadsheet, if requested.

## Example

Using the `Big5` dataset, we ask whether agreeableness predicts
conscientiousness beyond what the other personality dimensions already
explain.

Put `Conscientiousness` into **Dependent Variable**, and the remaining
personality scales into **Covariates**.

In **Model Builder**, put every predictor except `Agreeableness` into block 1,
and `Agreeableness` alone into block 2. Tick **Adjusted R²** under Fit
Measures. The **Model Comparisons** table now answers the question directly:
does adding agreeableness improve the model beyond the others?

Under Assumption Checks, tick **Residual plots** and **Collinearity
statistics** — personality scales tend to correlate with one another, so
collinearity is a live concern here rather than a formality.

## References

- Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). *Applied Multiple
  Regression/Correlation Analysis for the Behavioral Sciences* (3rd ed.).
  Erlbaum.
- Fox, J. (2015). *Applied Regression Analysis and Generalized Linear Models*
  (3rd ed.). Sage.

Sums of squares, collinearity statistics and the Durbin-Watson test are
computed with car; estimated marginal means with emmeans.

- Fox, J., Weisberg, S., & Price, B. (2026). *car: Companion to Applied
  Regression* [R package].
- Lenth, R., & Piaskowski, J. (2026). *emmeans: Estimated Marginal Means, aka
  Least-Squares Means* [R package].
