# ANCOVA

## Overview

Tests how a continuous outcome depends on one or more categorical factors while
adjusting for one or more continuous covariates. It is ANOVA with covariates
added.

The covariate does two things. It removes variance the factors were never going
to explain, which makes the tests of the factors more sensitive. And it reports
the factor effects as they would be if every group had the same average value
on the covariate — which is what makes it useful when groups differ on
something you did not manipulate.

## When to use it

Use this analysis when you have categorical predictors, a continuous outcome,
and a continuous variable you want to control for — a pre-test score, an age, a
baseline measurement.

- With no covariate to adjust for, use **ANOVA**.
- With a single factor and nothing else, **One-Way ANOVA** is simpler.
- If the same people were measured repeatedly, use **Repeated Measures ANOVA**,
  which accepts covariates too.
- With several outcome variables at once, use **MANCOVA**.
- If everything is continuous, use **Linear Regression**.

A warning about the common case. When groups were formed by random assignment,
adjusting for a baseline covariate is straightforward and helpful. When groups
differ for reasons you did not control, ANCOVA does *not* turn them into
comparable groups — it adjusts for the covariate you measured and nothing else,
and the adjusted means describe a situation that may not exist in your data.

## Assumptions

Everything ANOVA assumes, plus two more:

- Observations are independent of one another.
- The residuals are normally distributed.
- The residual variance is equal across cells.
- The covariate is linearly related to the outcome.
- **Homogeneity of regression slopes**: that relationship is the same in every
  group.

The last is the assumption specific to ANCOVA. Test it by adding the
factor × covariate interaction to **Model Terms** — if it is significant, the
covariate works differently across groups, and a single adjusted comparison is
misleading. Remove the interaction again once you have checked it.

The covariate should also be measured before the manipulation, or at least be
unaffected by it. Adjusting for something the treatment itself changed removes
part of the effect you are trying to measure.

## Options

### Dependent Variable

The continuous outcome. Exactly one; for several at once, use MANCOVA.

### Fixed Factors

The categorical predictors, with all their interactions included by default.

### Covariates

The continuous variables to adjust for.

### Model Fit

**Overall model test** adds an F test for the whole model against a model with
no predictors.

### Effect Size

The same three measures as ANOVA:

$$\begin{aligned}
\eta^2 &= \frac{SS_{effect}}{SS_{total}} \\[4pt]
\eta_p^2 &= \frac{SS_{effect}}{SS_{effect} + SS_{error}} \\[4pt]
\omega^2 &= \frac{SS_{effect} - df_{effect} \cdot MS_{error}}{SS_{total} + MS_{error}}
\end{aligned}$$

Partial η² is always the largest and the most commonly reported — say which you
used. Note that in ANCOVA these are computed on the model *including* the
covariate, so a factor's η² is its share of variance after the covariate has
taken its own, and will differ from the same factor's η² in an ANOVA without
it.

### Model

**Model Terms** controls which terms are fitted. This is where you add the
factor × covariate interaction to test homogeneity of slopes, and where you
remove interactions that theory says do not belong.

**Sum of squares** sets how variance is attributed in unbalanced designs; Type
3 is the default and the usual choice.

### Assumption Checks

- **Homogeneity test** — Levene's test for equal variances across cells.
- **Normality test** — the Shapiro-Wilk test on the residuals.
- **Q-Q Plot** — residual quantiles against a normal distribution.

Note that none of these test homogeneity of regression slopes; that is done
through Model Terms as described above.

### Contrasts

Planned comparisons within a factor, defined in advance. The same types are
available as in ANOVA — Deviation, Simple, Difference, Helmert, Repeated and
Polynomial — with Polynomial the natural choice for ordered levels.

### Post Hoc Tests

All-pairs comparisons with a **Correction**: Tukey (the default), Scheffe,
Bonferroni, Holm, or none. **Effect size** adds Cohen's d per comparison, with
an optional **Confidence interval**.

In ANCOVA these comparisons are made on the adjusted means, not the raw group
means.

### Estimated Marginal Means

The adjusted means — the model's prediction for each group with the covariate
held at its overall average. **These are the means to report from an ANCOVA**,
because the raw group means do not reflect the adjustment the analysis just
made.

**Marginal means plots** draw them, optionally with **Observed scores** and a
choice of **Error bars**; **Marginal means tables** give the numbers. **Equal
cell weights** averages cells equally rather than by size.

### Save

**Residuals** writes the model residuals to the spreadsheet as a new column.

## Results

### ANCOVA

One row per model term, with sums of squares, degrees of freedom, F, p-value
and any effect sizes requested. The covariate appears as a term of its own; a
significant covariate simply confirms it was worth adjusting for.

Read interactions between factors first — if one is significant, the main
effects are averages over conditions that behave differently.

### Assumption Checks

The homogeneity and normality tests. A *large* p-value is the reassuring one.
Both grow sensitive in large samples, so read them with the Q-Q plot rather
than as pass/fail gates.

### Contrasts

One row per planned comparison with its estimate, standard error, t and
p-value. Not corrected for multiple comparisons.

### Post Hoc Tests

Every pair of levels, compared on the adjusted means, with corrected p-values.
The correction is already applied.

### Estimated Marginal Means

The adjusted means with confidence intervals, as tables or plots. Report these
rather than raw group means.

### Residuals

The saved residual column, if requested.

## Example

Using the `ToothGrowth` dataset, we ask whether the two supplements differ in
their effect on tooth growth, once dose is taken into account.

Put `len` into **Dependent Variable**, `supp` into **Fixed Factors**, and
`dose` into **Covariates** — treating dose as continuous here rather than as a
factor.

First check the slopes assumption: under **Model Terms**, add the `supp` ✻
`dose` interaction and see whether it is significant. If it is not, remove it
and read the main analysis.

Tick **partial η²**, then under Estimated Marginal Means add `supp` and tick
**Marginal means tables** — those adjusted means, not the raw ones, are what
this analysis is about.

## References

- Fisher, R. A. (1932). *Statistical Methods for Research Workers* (4th ed.).
  Oliver and Boyd.
- Miller, G. A., & Chapman, J. P. (2001). Misunderstanding analysis of
  covariance. *Journal of Abnormal Psychology, 110*(1), 40–48.
