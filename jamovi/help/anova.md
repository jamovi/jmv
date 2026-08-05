# ANOVA

## Overview

Tests how one continuous outcome depends on one or more categorical factors,
including the interactions between them. With a single factor it is the
classical one-way ANOVA; with two or more it also tests whether the effect of
one factor changes across the levels of another.

An interaction is often the point of the analysis. It says that the factors
cannot be understood separately — the effect of one depends on the other.

## When to use it

Use this analysis when your outcome is continuous and your predictors are
categorical.

- With a single factor and no interest in the extras here, **One-Way ANOVA** is
  simpler, and offers Welch's version for unequal variances.
- If the same people were measured under several conditions, use **Repeated
  Measures ANOVA**.
- If you want to adjust for a continuous variable, use **ANCOVA**.
- If you have several outcome variables to test together, use **MANCOVA**.
- If your predictors are continuous, use **Linear Regression** — which is the
  same underlying model, presented differently.

## Assumptions

- Observations are independent of one another.
- The residuals are normally distributed.
- The residual variance is equal across all cells of the design.

Normality is an assumption about the *residuals*, not about the raw outcome, so
a skewed dependent variable is not by itself a problem. ANOVA is fairly robust
to non-normality with reasonable sample sizes, and much less robust to unequal
variances when cell sizes are also unequal.

## Options

### Dependent Variable

The continuous outcome. This analysis takes exactly one; for several outcomes
at once, use MANCOVA.

### Fixed Factors

The categorical predictors. By default every factor and every interaction
between them enters the model.

### Model Fit

**Overall model test** adds an F test for the model as a whole, against a model
with no predictors at all.

### Effect Size

Three measures, differing only in what they divide by:

    η²         = SS_effect / SS_total

    partial η² = SS_effect / (SS_effect + SS_error)

    ω²         = (SS_effect − df_effect × MS_error) / (SS_total + MS_error)

η² is the share of *total* variance a term explains, so shares across terms sum
to at most 1. Partial η² sets the other terms aside by excluding them from the
denominator; terms therefore do not sum to 1, and values run higher — often
much higher when the design has several strong effects.

ω² subtracts the error expected by chance, which removes most of the upward
bias the other two carry in small samples. It is the most accurate of the three
and the least reported.

Report which one you used. η² and partial η² are frequently confused, and
partial η² is always the larger.

### Model

**Model Terms** controls which main effects and interactions are fitted. Drop
an interaction here if theory says it does not belong, or to fit a
main-effects-only model.

**Sum of squares** sets how variance is attributed when the design is
unbalanced. Type 3 is the default and the usual choice; with equal cell sizes
all three types agree, so this only matters when cells are unequal.

### Assumption Checks

- **Homogeneity test** — Levene's test for equal variances across cells.
- **Normality test** — the Shapiro-Wilk test on the residuals.
- **Q-Q Plot** — residual quantiles against a normal distribution.

### Contrasts

Contrasts split a factor's effect into specific planned comparisons, defined
before seeing the data. Each factor can take one type:

| Type | Compares each level against |
|---|---|
| Deviation | The overall mean |
| Simple (vs. First) | The first level |
| Simple (vs. Last) | The last level |
| Difference | The mean of the preceding levels |
| Helmert | The mean of the subsequent levels |
| Repeated | The next level |
| Polynomial | Linear, quadratic and higher-order trends |

Polynomial is the one to reach for when the factor's levels are ordered and
evenly spaced — doses, time points, age bands — because it tests for a trend
rather than for scattered differences.

### Post Hoc Tests

Comparisons between every pair of levels, made after seeing the results, with a
correction for the number of comparisons. **Tukey** is the default and the
standard choice for all-pairs comparisons; **Bonferroni** is conservative;
**Holm** is uniformly more powerful than Bonferroni at the same guarantee;
**Scheffe** is for unplanned comparisons more complex than pairs; **No
correction** should be reported as such.

**Effect size** adds Cohen's d for each comparison, optionally with a
**Confidence interval**.

Contrasts and post-hoc tests answer different questions. Use contrasts when you
knew what you wanted to compare in advance, post-hoc tests when you are looking
across all pairs.

### Estimated Marginal Means

Model-predicted means for the levels of a term, averaging over the other
factors. In an unbalanced design these differ from the raw group means, and are
generally the ones to report because they are not distorted by unequal cell
sizes.

Each term you add gets its own set of output. **Marginal means plots** draw
them, optionally with **Observed scores** overlaid and a choice of **Error
bars**; **Marginal means tables** give the numbers. **Equal cell weights**
averages the cells equally rather than in proportion to their sizes.

An interaction plot here is usually the clearest way to show what an
interaction means.

### Save

**Residuals** writes the model residuals back to the spreadsheet as a new
column, so you can inspect or plot them yourself.

## Results

### ANOVA

One row per model term, with its sums of squares, degrees of freedom, F and
p-value, plus any effect sizes requested.

Read the interactions first. If an interaction is significant, the main effects
below it are averages across conditions that behave differently, and reporting
them on their own can be misleading.

### Assumption Checks

The homogeneity and normality tests. Here a *large* p-value is the reassuring
one. Both tests grow sensitive in large samples, so read them alongside the Q-Q
plot rather than as pass/fail gates.

### Contrasts

One row per planned comparison, with its estimate, standard error, t and
p-value. These are not corrected for multiple comparisons — planned contrasts
conventionally are not, but say so when reporting.

### Post Hoc Tests

Every pair of levels, with mean differences and corrected p-values. The
p-values already include the correction you chose; do not adjust them again.

### Estimated Marginal Means

The model-predicted means with confidence intervals, one table or plot per
term requested.

### Residuals

The saved residual column, if requested.

## Example

Using the `ToothGrowth` dataset, we ask whether tooth growth depends on the
supplement given, the dose, or the combination of the two.

Put `len` into **Dependent Variable**, and `supp` and `dose` into **Fixed
Factors**. (`dose` must be a nominal variable for this; change its measure type
in the Data tab if it is currently continuous.)

Tick **partial η²** under Effect Size. Under Estimated Marginal Means add a
term containing both factors and tick **Marginal means plots** — the resulting
interaction plot shows at a glance whether the two supplements behave
differently at different doses, which is what the interaction term is testing.

## References

- Fisher, R. A. (1925). *Statistical Methods for Research Workers*. Oliver and
  Boyd.
- Tukey, J. W. (1949). Comparing individual means in the analysis of variance.
  *Biometrics, 5*(2), 99–114.
