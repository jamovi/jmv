# One-Way ANOVA (Non-parametric)

## Overview

The Kruskal-Wallis test compares three or more independent groups without
assuming the data are normally distributed. It works on the ranks of the values
rather than the values themselves.

It is the non-parametric counterpart of the one-way ANOVA, and the extension of
the Mann-Whitney U test to more than two groups. Its limitation is that it
handles a single grouping variable only — there is no non-parametric
counterpart here for factorial designs.

## When to use it

Use this when you would have run a one-way ANOVA, but the data make you
uncomfortable doing so: strongly skewed distributions, obvious outliers, small
samples, or an ordinal outcome such as a rating scale.

- If the normality assumption is tenable, use **One-Way ANOVA** — it is more
  powerful, and Welch's version already handles unequal variances.
- With only two groups, use **Independent Samples T-Test**, which offers the
  Mann-Whitney U test.
- If the same people were measured under every condition, use **Repeated
  Measures ANOVA (Non-parametric)**.

Non-parametric does not mean assumption-free. Read the assumptions below before
treating this as the safe fallback.

## Assumptions

- Observations are independent, both within and between groups.
- The dependent variable is at least ordinal.

Strictly, the test asks whether one group tends to produce larger values than
another. It is commonly described as a test of medians, but that reading is
only valid when the groups have similarly shaped distributions. When the shapes
differ markedly — one skewed left, another right — a significant result tells
you the distributions differ without telling you the medians do.

There is no normality assumption, and no requirement that group variances be
equal, though grossly different spreads complicate interpretation for the
reason just given.

## Options

### Dependent Variables

The variables to compare across groups. A separate Kruskal-Wallis test is run
for each, and each appears as a row of the results table.

### Grouping Variable

The nominal or ordinal variable defining the groups.

### Effect Size

**ε²** (epsilon squared) is the proportion of variability in the ranks
explained by the grouping variable — the rank-based analogue of η². jmv
computes it from the test statistic:

    ε² = H(n + 1) / (n² − 1)

It runs from 0 to 1. Note that some sources define ε² as H/(n − 1) instead,
which gives a different number; if you are comparing against another package,
check which it reports.

### Post Hoc Tests

Having found that the groups differ, these identify which pairs differ.

- **DSCF pairwise comparisons** — the Dwass-Steel-Critchlow-Fligner procedure,
  which controls the family-wise error rate across all pairs. The usual choice
  after a Kruskal-Wallis test.
- **Dunn's pairwise comparisons** — reported with no correction, and with
  Bonferroni and Holm corrections alongside, so you can see the effect of each.

Use one or the other rather than reporting whichever gives the answer you
prefer.

## Results

### Kruskal-Wallis

One row per dependent variable, giving χ², its degrees of freedom and a
p-value, plus ε² if requested.

A small p-value means at least one group tends to produce larger values than
another. It does not say which, or by how much — the post-hoc comparisons and
the group medians answer that.

### Dwass-Steel-Critchlow-Fligner pairwise comparisons

One row per pair of groups, with the test statistic and a p-value already
adjusted for the number of comparisons. Do not correct these again.

### Dunn's pairwise comparisons

One row per pair, with the uncorrected p-value alongside Bonferroni- and
Holm-corrected versions. Decide which correction you are using before looking,
and report that one.

## Example

Using the `ToothGrowth` dataset, we ask whether tooth growth differs across the
three dose levels, without assuming normality.

Put `len` into **Dependent Variables** and `dose` into **Grouping Variable**.
(`dose` needs to be a nominal or ordinal variable; change its measure type in
the Data tab if it is currently continuous.)

Tick **ε²** for an effect size, and **DSCF pairwise comparisons** to see which
doses differ from which. To see the direction of any difference, run
**Descriptives** with `dose` as the split-by variable — this analysis reports
the test, not the group medians.

## References

- Kruskal, W. H., & Wallis, W. A. (1952). Use of ranks in one-criterion
  variance analysis. *Journal of the American Statistical Association, 47*(260),
  583–621.
- Dunn, O. J. (1964). Multiple comparisons using rank sums. *Technometrics,
  6*(3), 241–252.
