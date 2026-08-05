# Log-Linear Regression

## Overview

Models the **counts** in a contingency table as an outcome in their own right,
predicted from the categorical variables that define it. It is a Poisson
regression on cell frequencies.

Where a χ² test asks whether two variables are associated, this asks how the
counts are generated — and because it is a regression, it extends to any number
of variables and to specific interactions between them. Association between two
variables appears here as an interaction term.

## When to use it

Use this when you have three or more categorical variables, or when you want to
test particular interactions rather than overall association.

- With exactly two categorical variables and a simple question, **Contingency
  Tables** is far more direct and gives effect sizes and residuals.
- With one categorical variable, use **Proportion Test (N Outcomes)**.
- If one variable is naturally an outcome to be predicted from the others, a
  logistic regression usually answers the question more directly — **Binomial**,
  **Multinomial** or **Ordinal Logistic Regression** depending on its levels.

The distinguishing feature of this analysis is that no variable is the outcome.
The count is. All the categorical variables enter as predictors on equal
footing, which is what makes it suited to exploring the structure of a
multi-way table.

## Assumptions

- Observations are independent, and each case contributes to exactly one cell.
- The counts follow a Poisson distribution.
- **Equidispersion** — the variance of the counts equals their mean.

Overdispersion, where variance exceeds the mean, is the common failure. It
leaves the coefficients roughly right but makes the standard errors too small,
so effects look more significant than they are. A deviance much larger than its
degrees of freedom is the warning sign.

Sparse tables are the other practical problem: with many variables the cells
divide quickly, and empty or near-empty cells destabilise the estimates.

## Options

### Factors and Counts

**Factors** takes the categorical variables defining the table. If your data
are already summarised as one row per cell with a frequency, put the frequency
variable into **Counts (optional)**; otherwise leave it empty.

### Model Builder

Assigns terms to **Blocks**, fitting a sequence of nested models compared
against one another.

This is the heart of the analysis. Interactions are built here, and an
interaction between two factors is precisely the claim that those two variables
are associated. Entering main effects in block 1 and an interaction in block 2
tests that association directly, as an improvement in model fit.

### Reference Levels

Sets the baseline level for each factor. Every coefficient is expressed
relative to it.

### Model Fit

**Fit Measures** — **Deviance**, **AIC** and **BIC**, plus an **Overall model
test**.

Deviance doubles as a diagnostic here: compare it against its degrees of
freedom. Much larger means the model fits poorly or the counts are
overdispersed.

**Pseudo R²** offers McFadden's, Cox & Snell's, Nagelkerke's and Tjur's. None
is a proportion of variance explained.

### Model Coefficients

- **Omnibus Tests** — likelihood ratio tests, one per term rather than one per
  dummy variable. For a factor with several levels, this is the test to read.
- **Estimate (Log Rate Ratio)** — the raw coefficients, on the log scale, with
  an optional **Confidence interval**.
- **Rate Ratio** — the exponentiated coefficients, with an optional confidence
  interval. Usually what you report.

A rate ratio is the multiplicative change in the expected count. The null value
is 1, not 0, so an interval excluding 1 indicates significance.

### Estimated Marginal Means

Model-predicted counts for the levels of a term, averaging over the rest, as
**Marginal means tables** and **Marginal means plots**, with a **Confidence
interval** and an **Equal cell weights** option.

For a multi-way table these plots are usually the only readable summary — a
three-way interaction is close to impossible to follow in a coefficients table.

## Results

### Model Fit Measures

One row per model with the requested fit measures and pseudo R². Check the
deviance against its degrees of freedom for overdispersion.

### Model Comparisons

Present with more than one block. Each row tests whether adding that block
improved the model, via a likelihood ratio test.

This is where an association is tested: add an interaction as its own block,
and a significant improvement means the variables are associated.

### Model Specific Results

Per model: the omnibus likelihood ratio tests and the coefficients table with
log rate ratios and rate ratios.

Read the interactions first. In this model, main effects describe how common
each level is overall — often uninteresting, since it may just reflect how the
sample was gathered. The interactions are what carry the relationships between
variables.

## Example

Using the `bugs` dataset, we ask whether the association between gender and
region holds once education is taken into account.

Put `Gender`, `Region` and `Education` into **Factors**, leaving **Counts**
empty since the data are one row per person.

In **Model Builder**, put the three main effects into block 1 and the
`Gender` ✻ `Region` interaction into block 2. The **Model Comparisons** table
then tests that association directly. Tick **Rate ratio** with its
**Confidence interval** for the coefficients, and check the deviance against
its degrees of freedom before trusting any of it.

## References

- Agresti, A. (2013). *Categorical Data Analysis* (3rd ed.). Wiley.
- Bishop, Y. M. M., Fienberg, S. E., & Holland, P. W. (1975). *Discrete
  Multivariate Analysis: Theory and Practice*. MIT Press.
