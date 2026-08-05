# Confirmatory Factor Analysis

## Overview

Tests a factor structure you specify in advance: you state which items load on
which factors, and the analysis reports how well that model reproduces the
observed covariances.

This is the confirmatory counterpart of exploratory factor analysis. Nothing is
discovered — every loading you did not specify is fixed at zero, and the
question is whether a model so constrained still fits the data.

The model implies a covariance matrix of its own, and fit is judged by how far
that implied matrix departs from the observed one.

## When to use it

Use this when you have a hypothesised structure — from theory, from a
published instrument, or from an EFA on different data.

- If you do not know how many factors there are or which items belong where,
  use **Exploratory Factor Analysis**.
- For data reduction with no measurement model, use **Principal Component
  Analysis**.
- To check the internal consistency of a single scale, use **Reliability
  Analysis**.

Running an EFA and a CFA on the same data does not confirm anything: the second
analysis tests a structure derived from the very data it is tested against.

## Assumptions

- The variables are continuous and multivariate normal — required by the
  maximum likelihood estimation used here.
- Relationships between items and factors are linear.
- The sample is large. CFA is demanding; several hundred cases is a common
  minimum, and small samples produce unstable estimates and unreliable fit
  statistics.
- The model is identified — enough constraints to estimate every parameter,
  which is what the Constraints option provides.

## Options

### Factors

Define each factor and assign its items. This is the model, and everything else
follows from it. Every item-factor combination you do not specify is fixed at
zero.

### Residual Covariances

Allows the residuals of two items to correlate, for items sharing something
beyond their factor — near-identical wording, a shared method. Adding these
improves fit, so add them only where justified rather than to rescue a failing
model.

### Options

**Missing Values Method** — **Full information maximum likelihood** uses all
available data and is generally preferable; **Exclude cases listwise** drops
any case with a missing value.

**Constraints** fix the scale of each latent factor, which is otherwise
arbitrary. **Factor variances = 1** standardizes the factors; **Scale factor =
scale first indicator** fixes the first item's loading at 1. The two give
equivalent fit and differ only in how the estimates are expressed.

### Estimates

**Test statistics** and **Confidence interval** for the loadings.
**Standardized estimate** rescales loadings so they are comparable across
items measured on different scales — usually the ones to report.

**Factor covariances**, **Factor intercepts**, **Residual covariances** and
**Residual intercepts** add those parameter estimates to the output.

### Model Fit

**Test for Exact Fit** reports the χ² test of the null hypothesis that the
model reproduces the covariance matrix exactly. Note the reversal: here a
*significant* result means the model does **not** fit. This test is so
sensitive to sample size that in a large sample almost any model is rejected,
which is precisely why the approximate fit measures below exist.

**Fit Measures**:

| Measure | Conventional guide |
|---|---|
| CFI | ≥ .95 good |
| TLI | ≥ .95 good |
| RMSEA | ≤ .06 good, ≤ .08 acceptable |
| SRMR | ≤ .08 good |
| AIC, BIC | Lower is better; for comparing models only |

CFI and TLI compare the model against a baseline in which all variables are
uncorrelated. RMSEA is reported with a 90% confidence interval — read the
interval rather than the point estimate, since in a small sample it is often
alarmingly wide.

These thresholds are conventions from simulation studies, not decision rules.
Report several measures and describe fit honestly rather than hunting for the
one that clears a cut-off.

### Additional Output

**Post-Hoc Model Performance** gives **Modification indices** — the improvement
in χ² expected from freeing each fixed parameter, with a **Highlight values
above** threshold. **Residuals observed correlation matrix** shows which item
pairs the model reproduces badly, also with a highlight threshold.

Modification indices are exploratory. Following them turns a confirmatory
analysis into an exploratory one, and a model reached that way needs
replication before it means anything.

**Path diagram** draws the model.

## Results

### Factor Loadings

Each item's loading on its factor, with standard errors, test statistics and
any intervals requested. Standardized loadings below about .5 mark items
contributing little to their factor.

### Factor Estimates

Factor variances, covariances and intercepts. A very high correlation between
two factors — above about .85 — suggests they are not distinct, and a model
merging them is worth testing.

### Residual Estimates

Residual variances and any residual covariances specified. A residual variance
near zero or negative — a Heywood case — signals a misspecified model, not a
good one.

### Model Fit

The χ² exact fit test and the requested fit measures. Read them together: a
model can clear the approximate thresholds while failing χ², which is common
and usually acceptable in large samples.

### Post-Hoc Model Performance

Modification indices and correlation residuals, if requested. Use them to
understand *where* a model fails, not as a list of changes to apply.

### Path Diagram

The model drawn with its estimates.

## Example

Using the `Big5` dataset, we test whether the personality items conform to the
five-factor structure they were written for.

Under **Factors**, create five factors and assign each item to the dimension it
was designed to measure. This is the hypothesis, and it must be specified
before looking at the data.

Leave the default fit measures, and tick **Standardized estimate** under
Estimates so loadings are comparable. Read the fit measures together with the
χ² test — with a large sample expect χ² to be significant even when CFI, TLI
and RMSEA are acceptable.

If fit is poor, tick **Residuals observed correlation matrix** to see which
item pairs the model reproduces badly, and treat anything you change in
response as exploratory.

## References

- Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in
  covariance structure analysis. *Structural Equation Modeling, 6*(1), 1–55.
- Rosseel, Y. (2012). lavaan: An R package for structural equation modeling.
  *Journal of Statistical Software, 48*(2), 1–36.
- Brown, T. A. (2015). *Confirmatory Factor Analysis for Applied Research*
  (2nd ed.). Guilford Press.

The model is fitted with lavaan, and the path diagram drawn with semPlot.

- Rosseel, Y., Jorgensen, T. D., De Wilde, L., Oberski, D., Byrnes,  J.,
  Vanbrabant, L., Savalei, V., Merkle, E., Hallquist, M., Rhemtulla, M.,
  Katsikatsou, M., Barendse, M., Rockwood,  N., Scharf, F., Du,  H., Jamil,
  H., Classe,  F. (2025). *lavaan: Latent Variable Analysis* [R package].
- Epskamp, S. (2026). *semPlot: Path Diagrams and Visual Analysis of Various
  SEM Packages' Output* [R package].
