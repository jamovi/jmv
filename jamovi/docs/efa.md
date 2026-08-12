# Exploratory Factor Analysis

## Overview

Identifies a small number of latent factors that account for the correlations
among a set of observed variables. Unlike PCA, it starts from a measurement
model: the factors are treated as unobserved causes of the variables, and each
variable's variance is split into a part shared with the factors and a part
unique to itself.

Each variable is modelled as a weighted sum of the factors plus a part unique
to itself. Only the shared variance is analysed, which is the formal difference
from PCA.

## When to use it

Use this when you want to discover how many constructs underlie a set of
measures, and which measures belong to which.

- If you only want fewer variables and make no claim about latent causes, use
  **Principal Component Analysis**.
- If you already have a factor structure and want to test it, use
  **Confirmatory Factor Analysis**. EFA lets the data determine the structure;
  CFA imposes one.
- To check the consistency of a set of items already known to form one scale,
  use **Reliability Analysis**.

Running EFA and then reporting it as though it confirmed a prior hypothesis is
a common error. If you had the structure in advance, test it with CFA — ideally
on different data.

## Assumptions

- The variables are continuous and linearly related.
- The correlations are substantial enough to factor — checked by KMO and
  Bartlett's test.
- The sample is adequate; 5 to 10 cases per variable is a common minimum, and
  more matters when loadings are modest.
- The factors account for the correlations among variables, leaving residuals
  that are uncorrelated.

## Options

### Variables

The variables to factor. All should be continuous.

### Number of Factors

- **Based on parallel analysis** — the default, and the best-supported method:
  observed eigenvalues are compared against those from random data of the same
  size.
- **Based on eigenvalue** — retains factors above **Minimum value**. Known to
  over-retain.
- **Fixed number** — exactly the number you specify, which is the right choice
  when theory dictates it.

### Extraction

How the loadings are estimated:

| Method | Notes |
|---|---|
| Minimum residuals | The default; robust and rarely fails to converge. |
| Maximum likelihood | Enables fit measures and significance tests, but assumes multivariate normality. |
| Principal axis | A traditional choice, undemanding about distributions. |

Choose maximum likelihood if you want the model fit measures; otherwise the
default is fine.

### Rotation

Redistributes variance across factors to make them interpretable, without
changing total variance explained. **Varimax** and **Quartimax** are
orthogonal, keeping factors uncorrelated; **Promax**, **Oblimin** (the default)
and **Simplimax** are oblique, allowing them to correlate.

Oblique is usually the better default in the social sciences, where constructs
generally do correlate. Forcing orthogonality when factors are genuinely
related distorts the loadings.

### Factor Loadings

**Hide loadings below** suppresses small loadings for readability, commonly at
0.3; **Sort loadings by size** groups variables by their dominant factor. Both
are presentational and do not change the solution.

### Additional Output

- **Initial eigenvalues** — eigenvalues before extraction.
- **Factor correlations** — meaningful after an oblique rotation. Strong
  correlations here suggest a higher-order factor.
- **Factor summary** — variance explained per factor.
- **Model fit measures** — available with maximum likelihood extraction.
- **Scree plot** — eigenvalues in descending order with the parallel analysis
  comparison line.

### Assumption Checks

- **KMO measure of sampling adequacy** — compares the correlations against the
  partial correlations. Above .8 is good; below .6 means the data are poorly
  suited to factoring.

- **Bartlett's test of sphericity** — tests the correlation matrix against an
  identity matrix. It should be significant, though with a reasonable sample it
  nearly always is.

### Save

**Factor scores** writes each case's estimated score on each factor back to the
spreadsheet, using the chosen **Estimation method**. Factor scores are
estimates rather than observations, and different methods give different
values — treat them as approximate.

## Results

### Exploratory Factor Analysis

The factor loadings, with values below the threshold hidden, plus the
additional output requested.

Interpret each factor from the variables that load on it. A clean solution has
each variable loading strongly on one factor; **cross-loadings**, where a
variable loads moderately on two, are the usual obstacle to naming factors and
often signal that the variable is measuring more than one thing.

**Uniqueness** is the share of a variable's variance not explained by the
factors — one minus its communality. A uniqueness above about .7 means the
variable has little in common with the rest and may not belong.

Where model fit measures are available, they indicate whether the retained
number of factors reproduces the correlations adequately. Retaining too few
leaves systematic residuals.

## Example

Using the `Big5` dataset, we ask whether the five personality dimensions
themselves share structure — whether they reduce to a smaller number of
higher-order factors.

Put all five dimensions into **Variables**.

Leave the number of factors on **parallel analysis** and the rotation on
**Oblimin**, since higher-order personality factors are expected to correlate.
Tick **Scree plot**, **Factor correlations** and, under Assumption Checks,
**KMO**.

Read the loadings with **Hide loadings below** at 0.3, then look for variables
that appear under no factor or under two — those are the ones that will make
the solution hard to name.

Note how thin the basis is: five variables is little to factor, and the KMO
will say so. An EFA is normally run on individual items, where the same reading
applies but the solution is far better determined.

## References

- Kaiser, H. F. (1974). An index of factorial simplicity. *Psychometrika,
  39*(1), 31–36.
- Fabrigar, L. R., Wegener, D. T., MacCallum, R. C., & Strahan, E. J. (1999).
  Evaluating the use of exploratory factor analysis in psychological research.
  *Psychological Methods, 4*(3), 272–299.
- Horn, J. L. (1965). A rationale and test for the number of factors in factor
  analysis. *Psychometrika, 30*(2), 179–185.

Factors, the KMO measure and Bartlett's test are computed with psych.

- Revelle, W. (2026). *psych: Procedures for Psychological, Psychometric, and
  Personality Research* [R package].
