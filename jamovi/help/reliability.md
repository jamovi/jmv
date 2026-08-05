# Reliability Analysis

## Overview

Assesses whether a set of items measures a single thing consistently — whether
the items in a questionnaire scale hang together well enough to be summed or
averaged into one score.

Two coefficients are offered. Cronbach's α is the long-standing convention.
McDonald's ω rests on weaker assumptions and is generally the better estimate,
though α remains what most reviewers expect to see.

## When to use it

Use this before combining several items into a scale score, to check that
combining them is defensible.

- To discover how many underlying dimensions a set of items has, rather than
  assuming one, use **Exploratory Factor Analysis**.
- To test a hypothesised factor structure, use **Confirmatory Factor
  Analysis**.
- To reduce many variables to a few components without a measurement model,
  use **Principal Component Analysis**.

Reliability is not validity. A scale can be highly reliable and measure
something other than what you intended.

## Assumptions

- The items measure a single underlying construct. If they measure two, α is
  not interpretable, whatever its value.
- Items are scored in the same direction. jamovi does **not** reverse them
  automatically — a negatively worded item will drag the coefficient down until
  you list it under Reverse Scaled Items.
- For α specifically: all items contribute equally to the construct
  (tau-equivalence). This is rarely true, and when it fails α underestimates
  reliability. ω does not require it.

## Options

### Items

The items making up the scale. All should be measuring the same construct.

### Scale Statistics

**Cronbach's α** rises with the number of items regardless of their quality, so
a long scale of mediocre items can still post a high α.

**McDonald's ω** is computed from a one-factor model. jamovi reports omega
**total** — not omega hierarchical, which some sources mean by "ω", so check
which figure you are comparing against.

Because ω weights items by their loadings rather than treating them as
interchangeable, it does not require tau-equivalence and is usually the more
accurate figure.

**Mean** and **Standard deviation** give the scale score's descriptives.

### Additional Options

**Correlation heatmap** plots the inter-item correlation matrix. Worth a look:
a block of items correlating strongly among themselves but weakly with the rest
means the scale is measuring more than one thing, which no single coefficient
will reveal.

### Item Statistics

- **Cronbach's α (if item is dropped)** and **McDonald's ω (if item is
  dropped)** — the coefficient recomputed with each item removed. A value
  noticeably *higher* than the overall figure marks an item that is hurting the
  scale.
- **Mean** and **Standard deviation** per item.
- **Item-rest correlation** — the correlation between an item and the sum of
  the *other* items. The item is excluded from the total it is correlated
  against, which avoids the inflation of correlating an item with a sum
  containing itself. Values below about .3 suggest an item that does not
  belong; a negative value almost always means the item needs reverse scoring.

### Reverse Scaled Items

Move any negatively worded items here. Because jamovi does not detect them
automatically, this step is easy to forget — and forgetting it is the single
most common cause of an implausibly low α.

## Results

### Scale Reliability Statistics

The overall α and ω with the scale mean and standard deviation.

Conventional thresholds put .7 as acceptable and .8 as good, but they are
conventions rather than facts. A very high value — above about .95 — is not
better; it usually means several items are near-duplicates and the scale is
longer than it needs to be.

### Item Reliability Statistics

One row per item with its descriptives, item-rest correlation, and the
if-dropped coefficients.

Read the item-rest correlations first. They identify problem items directly,
whereas the if-dropped columns only show the consequence of removing them.

### Correlation Heatmap

The inter-item correlation matrix, if requested.

## Example

Using the `Big5` dataset, we check whether a set of items forms a consistent
scale.

Put the relevant items into **Items**.

Tick **McDonald's ω** alongside the default α, and under Item Statistics tick
**Item-rest correlation** and **Cronbach's α (if item is dropped)**. Any item
with a negative item-rest correlation is reverse-keyed and belongs under
**Reverse Scaled Items** — fix that before reading the coefficients at all,
since everything else will be misleading until you do.

## References

- Cronbach, L. J. (1951). Coefficient alpha and the internal structure of
  tests. *Psychometrika, 16*(3), 297–334.
- McDonald, R. P. (1999). *Test Theory: A Unified Treatment*. Erlbaum.
- Revelle, W. (2025). *psych: Procedures for Psychological, Psychometric, and
  Personality Research* [R package].
