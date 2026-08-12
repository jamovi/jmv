# Analyses bundled with jamovi

These are the analyses that come with jamovi rather than being installed from
the library — the ones under Exploration, T-Tests, ANOVA, Regression,
Frequencies and Factor in the Analyses ribbon. They cover the methods most
research degrees teach and most papers report.

The same analyses are available outside jamovi as the **jmv** package for R,
which runs the identical code and produces the identical output. Nothing in
these documents is specific to one or the other.

## How the analyses are organised

The menu groups follow the kind of data you have rather than the mathematics
behind the methods, which is why closely related analyses can sit in different
groups. See [linear models](topics/linear-models.md) for how much of this menu
is one method in several guises.

### Exploration

**Descriptives**, for summarising and plotting variables before testing
anything. Worth running first whatever you intend to do next.

### T-Tests

**Independent Samples T-Test**, **Paired Samples T-Test** and **One Sample
T-Test**, comparing one or two means. Each also offers its rank-based
alternative for when the normality assumption is not tenable.

### ANOVA

Comparing means across more groups and more factors: **ANOVA**, **ANCOVA** with
covariates controlled, **Repeated Measures ANOVA** when the same people are
measured more than once, **MANCOVA** for several outcomes at once, and
**One-Way ANOVA**. Non-parametric equivalents sit in their own subgroup.

### Regression

Relationships between continuous variables: **Correlation Matrix** and
**Partial Correlation** for the strength of association, **Linear Regression**
for predicting an outcome, and the logistic family — **Binomial Logistic
Regression**, **Multinomial Logistic Regression** and **Ordinal Logistic
Regression** — for outcomes that are categories rather than quantities.

### Frequencies

Counts and proportions: **Contingency Tables** and **Paired Samples Contingency
Tables** for association between categorical variables, **Proportion Test (2
Outcomes)** and **Proportion Test (N Outcomes)** for a single variable against
expected proportions, and **Log-Linear Regression** for modelling the counts
themselves.

### Factor

Structure underlying a set of variables: **Principal Component Analysis**,
**Exploratory Factor Analysis** and **Confirmatory Factor Analysis**, plus
**Reliability Analysis** for the consistency of a scale.

## Ideas that run through the module

Some concepts recur across many analyses and are documented once:

- [Linear models](topics/linear-models.md) — why t-tests, ANOVA, ANCOVA and
  regression are the same model with different output.
- [Checking assumptions](topics/assumption-checks.md) — how to read a normality
  or equal-variance test, and what to do when one fails.
- [Marginal means and post hoc tests](topics/marginal-means-and-post-hoc.md) —
  what to do after a significant omnibus test, and why the marginal means do
  not match the descriptives.

## Example datasets

Four datasets are included, and the examples in these documents use them:

| Dataset | Contents |
|---|---|
| `bugs` | 93 people rating how much they would want to kill an insect, for insects varying in how disgusting and how frightening they are, with gender, region and education |
| `Big5` | 500 people scored on the five personality dimensions |
| `iris` | 150 iris flowers, four measurements each, across three species |
| `ToothGrowth` | 60 guinea pigs, tooth length by vitamin C supplement and dose |

## Citing an analysis

Each document ends with References covering the method it implements and the R
packages that compute it. Cite the method you used from there; jamovi's own
website explains how to cite jamovi itself.

## Reporting a problem

Errors in an analysis, and errors in these documents, go to the issue tracker
at <https://github.com/jamovi/jmv/issues>.
