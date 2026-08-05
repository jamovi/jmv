# Principal Component Analysis

## Overview

Reduces many correlated variables to a smaller number of components that
capture most of their variance. Each component is a weighted combination of the
original variables, constructed so that the first captures as much variance as
possible, the second as much of what remains, and so on.

PCA is a data reduction technique, not a measurement model. It makes no claim
that the components correspond to anything real — it simply re-expresses the
data in fewer dimensions.

## When to use it

Use this when you have many correlated variables and want fewer, without
committing to a theory about why they correlate.

- If you want to identify *latent constructs* thought to cause the observed
  correlations, use **Exploratory Factor Analysis**. The distinction is real:
  PCA explains total variance, EFA explains shared variance and separates out
  what is unique to each variable.
- To test a factor structure you already have in mind, use **Confirmatory
  Factor Analysis**.
- To check whether a set of items forms one consistent scale, use **Reliability
  Analysis**.

In practice PCA and EFA often give similar loadings, particularly with many
variables and strong correlations. The choice matters most for interpretation
and for what you claim in writing.

## Assumptions

- The variables are continuous and linearly related to one another.
- The correlations are substantial enough to be worth reducing — the KMO
  measure and Bartlett's test check this.
- The sample is large enough. Rules of thumb range from 5 to 10 cases per
  variable, and more is better.

PCA is sensitive to the scale of the variables, so jamovi works from the
correlation matrix, which standardizes them.

## Options

### Variables

The variables to reduce. All should be continuous.

### Number of Components

- **Based on parallel analysis** — the default and the best-supported method.
  It compares each observed eigenvalue against the eigenvalues from random data
  of the same size, retaining components that beat chance.
- **Based on eigenvalue** — retains components with eigenvalues above
  **Minimum value** (1 by default). This is the Kaiser criterion, and it is
  known to over-retain; parallel analysis is preferable.
- **Fixed number** — retains exactly the number you specify.

An eigenvalue λⱼ is the variance captured by component j, and the proportion of
total variance it explains is

    proportion = λⱼ / Σλ = λⱼ / p

where p is the number of variables, since each standardized variable
contributes a variance of 1.

### Rotation

Rotation redistributes variance across the retained components to make them
easier to interpret, without changing how much variance they explain in total.

| Rotation | Type |
|---|---|
| Varimax | Orthogonal — components stay uncorrelated |
| Quartimax | Orthogonal |
| Promax | Oblique — components may correlate |
| Oblimin | Oblique (the default) |
| Simplimax | Oblique |
| None | No rotation |

Oblique rotations allow the components to correlate, which is usually more
realistic. If they turn out nearly uncorrelated, an oblique rotation gives
almost the same answer as an orthogonal one — so the oblique default costs
little.

### Factor Loadings

**Hide loadings below** suppresses small loadings to make the pattern
readable; 0.3 is a common threshold. **Sort loadings by size** groups the
variables by the component they load on most strongly.

Suppressing loadings is presentational. Raise the threshold too far and a
variable loading moderately on two components will look like it loads cleanly
on one.

### Additional Output

- **Initial eigenvalues** — the eigenvalues before rotation, with the variance
  each explains.
- **Component correlations** — meaningful only after an oblique rotation.
- **Component summary** — variance explained per component.
- **Scree plot** — eigenvalues in descending order, with the parallel analysis
  line for comparison. The point where the curve levels off is a visual guide
  to how many components to keep.

### Assumption Checks

- **KMO measure of sampling adequacy** — the ratio of correlation to partial
  correlation:

      KMO = ΣΣr²ᵢⱼ / (ΣΣr²ᵢⱼ + ΣΣa²ᵢⱼ)

  where rᵢⱼ are correlations and aᵢⱼ partial correlations. Values above .8 are
  good, below .6 mean the data are poorly suited to PCA.

- **Bartlett's test of sphericity** — tests whether the correlation matrix
  differs from an identity matrix:

      χ² = −[(n − 1) − (2p + 5)/6] × ln|R|

  where |R| is the determinant of the correlation matrix. It should be
  significant; with any reasonable sample it almost always is, so it is a weak
  check compared with KMO.

### Save

**Component scores** writes each case's score on each component back to the
spreadsheet.

## Results

### Component Loadings

The loading of each variable on each retained component, with loadings below
the threshold hidden.

Interpret a component by what loads on it. A clean solution has each variable
loading strongly on one component and weakly on the rest; variables loading
moderately on several are the ones that make a solution hard to name.

**Uniqueness** is the proportion of a variable's variance not captured by the
retained components, so

    uniqueness = 1 − communality

A high uniqueness means the variable has little in common with the others.

### Component Statistics

Variance explained per component, individually and cumulatively, and the
component correlations after an oblique rotation.

### Model Fit

Fit information for the retained solution.

### Assumption Checks

KMO and Bartlett's test. KMO is reported per variable as well as overall — a
variable with a low individual KMO is a candidate for removal.

### Eigenvalues

Observed eigenvalues alongside those from parallel analysis. Components whose
observed eigenvalue exceeds the simulated one are the ones worth keeping.

### Factor scores

The component scores written back to the spreadsheet, if requested.

## Example

Using the `iris` dataset, we ask whether the four flower measurements can be
summarised in fewer dimensions.

Put `Sepal.Length`, `Sepal.Width`, `Petal.Length` and `Petal.Width` into
**Variables**.

Leave the number of components on **parallel analysis**, and tick **Scree
plot** and **Initial eigenvalues** to see the decision it made. Tick **KMO** and
**Bartlett's test** under Assumption Checks. With only four variables this is a
small example — the petal measurements will load together strongly, and sepal
width will stand apart.

## References

- Horn, J. L. (1965). A rationale and test for the number of factors in factor
  analysis. *Psychometrika, 30*(2), 179–185.
- Kaiser, H. F. (1974). An index of factorial simplicity. *Psychometrika,
  39*(1), 31–36.
