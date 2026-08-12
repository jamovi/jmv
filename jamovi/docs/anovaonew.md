# One-Way ANOVA

## Overview

Compares the means of three or more groups defined by a single grouping
variable, testing whether they are all equal. Each dependent variable is tested
separately against the same grouping variable.

Two versions are available. **Welch's** does not assume the groups have equal
variances and is the default here. **Fisher's** is the classical ANOVA, which
does assume equal variances.

Note that this differs from other software, which typically runs Fisher's by
default. Welch's is the safer choice: it costs very little when variances are
in fact equal, and protects you when they are not.

## When to use it

Use this analysis when one categorical variable splits your cases into groups,
and you want to know whether the groups differ on some measurement.

- If there are only two groups, use **Independent Samples T-Test** — the two
  analyses agree, but the t-test reports the difference more directly.
- If you have more than one grouping variable, or want to test their
  interaction, use **ANOVA**.
- If the same people were measured under every condition, use **Repeated
  Measures ANOVA**.
- If you want to adjust for a continuous variable, use **ANCOVA**.
- If the data are badly non-normal, use **One-Way ANOVA (Non-parametric)**.

A significant result tells you the groups are not all equal — not *which* ones
differ. Use the post-hoc tests for that.

## Assumptions

Both versions assume that:

- observations are independent of one another;
- the dependent variable is normally distributed within each group.

**Fisher's** additionally assumes the groups have equal variances. **Welch's**
does not, which is why it is the default. If the homogeneity test is
significant, that is an argument for Welch's rather than a reason to abandon
the analysis.

With reasonably large and similar-sized groups, ANOVA is fairly robust to
non-normality.

## Options

### Dependent Variables

The variables to compare across the groups. A separate one-way ANOVA is run for
each, and each appears as a row in the results table.

### Grouping Variable

The nominal or ordinal variable that defines the groups. With only two levels
this reduces to a t-test.

### Variances

Choose **Don't assume equal (Welch's)**, **Assume equal (Fisher's)**, or both.
Running both is a reasonable way to check whether the conclusion depends on the
assumption — if the two disagree, trust Welch's.

### Missing Values

**Exclude cases analysis by analysis** uses every row where that particular
dependent variable is present. **Exclude cases listwise** drops any row with a
missing value on any variable in the analysis, so every test uses the same
cases.

### Additional Statistics

- **Descriptives table** — N, mean, standard deviation and standard error for
  each group.
- **Descriptives plots** — the group means with confidence intervals. Usually
  the fastest way to see *where* a significant result comes from.

### Assumption Checks

- **Normality test** — the Shapiro-Wilk test on the residuals.
- **Q-Q plot** — residual quantiles against a normal distribution; points close
  to the diagonal indicate normality.
- **Homogeneity test** — Levene's test for equal variances. A significant
  result points you to Welch's.

See [checking assumptions](topics/assumption-checks.md) for why these tests
mislead in very small and very large samples, and what to do when one fails.

### Post-Hoc Tests

Having found that the groups are not all equal, post-hoc tests compare them
pair by pair, correcting for the number of comparisons made. This analysis
offers a different pair of methods from the rest of the ANOVA family; see
[marginal means and post hoc tests](topics/marginal-means-and-post-hoc.md).

- **Games-Howell (unequal variances)** — pairs with Welch's; does not assume
  equal variances.
- **Tukey (equal variances)** — pairs with Fisher's.

Match the post-hoc test to the ANOVA you ran. The **Statistics** options
control what each comparison reports: the **Mean difference**, its
**significance**, the underlying **test results (t and df)**, and whether to
**flag significant comparisons** with an asterisk.

## Results

### One-Way ANOVA

One row per dependent variable per selected version, giving F, the two degrees
of freedom and a p-value. Welch's degrees of freedom are usually fractional —
that is expected, not an error.

A small p-value means at least one group differs from the others somewhere. It
does not say which, or how many.

### Group Descriptives

N, mean, standard deviation and standard error for each group of each dependent
variable. Read this alongside the ANOVA — it is what tells you the direction
and size of any difference.

### Assumption Checks

The normality and homogeneity tests. Here a *large* p-value is the reassuring
one. With large samples both tests flag departures too small to matter, so read
them alongside the Q-Q plot.

### Plots

The descriptives plots and Q-Q plots requested above.

### Post Hoc Tests

One row per pair of groups, with the requested statistics and corrected
p-values. These p-values are already adjusted for multiple comparisons — do not
correct them again.

## Example

Using the `iris` dataset, we ask whether the three iris species differ in sepal
length.

Put `Sepal.Length` into **Dependent Variables** and `Species` into **Grouping
Variable**.

Leave **Welch's** ticked, and add **Descriptives plots** so you can see the
three group means. Under Assumption Checks tick **Homogeneity test** to see
whether the equal-variance assumption would have held. Then set **Post-Hoc
Test** to Games-Howell to find out which species actually differ from which.

## References

- Welch, B. L. (1951). On the comparison of several mean values: An alternative
  approach. *Biometrika, 38*(3/4), 330–336.
- Games, P. A., & Howell, J. F. (1976). Pairwise multiple comparison procedures
  with unequal n's and/or variances. *Journal of Educational Statistics, 1*(2),
  113–125.

Levene's test is computed with car.

- Fox, J., Weisberg, S., & Price, B. (2026). *car: Companion to Applied
  Regression* [R package].
