# Linear models

Several analyses with different names — t-tests, ANOVA, ANCOVA, linear
regression — are the same model underneath. They were developed in separate
traditions, by different people, for different kinds of experiment, and they
kept their separate names and separate output conventions. The arithmetic is
common to all of them.

Knowing this is not a technicality. It explains why the assumptions are the
same wherever you meet them, why an analysis sometimes gives an answer
identical to one with an entirely different name, and why the choice between
them is often a choice about which table you want to read rather than about
which model to fit.

## The common form

Every analysis in this family predicts an outcome as a weighted sum of
predictors, plus what is left over:

$$ y = b_0 + b_1 x_1 + b_2 x_2 + \cdots + \varepsilon $$

The weights are chosen to make the leftovers — the **residuals** — as small as
possible overall. Everything else is presentation: which weights get shown,
whether they are reported individually or gathered into a test of a whole
predictor at once, and what the effect sizes are called.

The residuals are the thread running through the family. They are what the
model failed to explain, they are what the assumptions are about, and they are
what the diagnostic plots plot.

## Factors and covariates are the same thing

The apparent divide in this family is between analyses that take grouping
variables and analyses that take continuous ones. It is not a real divide.

A factor enters the model as a set of numeric columns — one fewer than it has
levels — each contrasting a level against a reference. Once coded that way, a
factor is just more predictors, and the model neither knows nor cares that they
came from a grouping variable. This is why ANCOVA can take factors and
covariates side by side without needing a different kind of machinery for each.

The consequences are concrete:

- An independent samples t-test and a one-way ANOVA on two groups test the same
  hypothesis. The *F* is the *t* squared.
- A one-way ANOVA and a linear regression with a single factor as predictor fit
  the same model.
- ANCOVA is an ANOVA with a continuous predictor added, or equally a regression
  with a factor added.

## Which analysis should you run, then?

If the models coincide, pick the analysis whose output answers your question
with the least translation.

| Analysis | Its output is built around |
|---|---|
| **ANOVA** | a test per factor, and group means |
| **ANCOVA** | the same, with group means adjusted for a covariate |
| **MANCOVA** | several outcomes tested jointly |
| **Linear Regression** | a coefficient per predictor, and prediction |

ANOVA-family output is built around testing whole factors and estimating group
means; regression output is built around individual coefficients and the
model's predictive performance. Running the analysis whose output you actually
want to report saves you converting one into the other.

## When the outcome is not continuous

The same structure extends to outcomes that are not continuous, by predicting a
transformation of the outcome rather than the outcome itself. The weighted sum
stays exactly as it is; only what it predicts changes.

| Outcome | Analysis | Predicts |
|---|---|---|
| Continuous | **Linear Regression** | the outcome directly |
| Two categories | **Binomial Logistic Regression** | the log odds |
| Several categories | **Multinomial Logistic Regression** | log odds against a reference |
| Ordered categories | **Ordinal Logistic Regression** | log odds of each threshold |
| Counts | **Log-Linear Regression** | the log of the expected count |

This is why coefficients in these analyses are not read the way regression
coefficients are: they describe the transformed scale, which is what the odds
ratios in the output are for.

It also explains which assumptions survive. Normality and constant variance
were never assumptions about the data — they were assumptions about the
residuals of a model predicting a continuous outcome. Change what is being
predicted and they no longer apply, while independence and correct functional
form still do.

## What this means in practice

- **Assumptions** — shared across the family, so read them once rather than as
  a new list per analysis. See [checking assumptions](assumption-checks.md).
- **Normality** — an assumption about the residuals, not the raw outcome. A
  skewed outcome is not by itself a violation.
- **Effect sizes** — not shared. η², partial η², ω² and R² are all proportions
  of variance, but they are proportions of *different* totals and are not
  comparable across analyses.
- **Cell sizes** — categorical predictors need enough data per cell, however
  few predictors the model appears to contain.

## References

- Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). *Applied Multiple
  Regression/Correlation Analysis for the Behavioral Sciences* (3rd ed.).
  Lawrence Erlbaum.
- Nelder, J. A., & Wedderburn, R. W. M. (1972). Generalized linear models.
  *Journal of the Royal Statistical Society: Series A, 135*(3), 370–384.
