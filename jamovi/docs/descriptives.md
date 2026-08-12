# Descriptives

## Overview

Summarises variables — their central tendency, spread and shape — and plots
their distributions. Usually the first analysis you run, to understand your
data and check it looks the way you expect before testing anything.

## When to use it

Use Descriptives to get a feel for your variables: how they are distributed,
whether values are missing, and whether anything looks wrong.

- To describe the *relationship* between two variables rather than each one
  alone, use **Correlation Matrix** or **Contingency Tables**.
- To compare a summary statistic *between* groups formally, use a t-test or
  ANOVA. Descriptives will show you the group means via **Split by**, but it
  does not test whether they differ.

## Options

### Variables

The variables to summarise. Which statistics are meaningful depends on each
variable's measure type — a mean is reported for continuous variables but not
for nominal ones.

### Split by

Splits the summary by the levels of one or more nominal variables, giving a
separate column per group. Useful for comparing groups before testing them.

### Frequency tables

A count and percentage for each level. Available for nominal and ordinal
variables only; a frequency table of a continuous variable would have one row
per distinct value.

### Statistics

Tick the statistics you want; each becomes a row of the Descriptives table.
Most are self-explanatory, but a few are worth a note:

| Statistic | Note |
|---|---|
| Mode | If several values tie, only the first is reported — a footnote appears when this happens. |
| Std. deviation, Variance | Computed with the sample (n − 1) denominator. |
| Skewness | 0 means symmetric; positive means a long right tail. |
| Kurtosis | Excess kurtosis — 0 means normal-like, positive means heavy tails. |
| Shapiro-Wilk | Tests normality. A small p-value means non-normal. |
| Percentile values | Enter the percentiles you want, separated by commas. |

Skewness and kurtosis are reported with their standard errors. A rough guide
is that a value more than about twice its standard error is worth a second
look — but always look at the histogram too.

### Outliers

**Extreme values** lists the highest and lowest values of each variable, with
their row numbers, so you can go and inspect them in the spreadsheet.

### Plots

Choose the plots suited to the variable type: **Histogram** (with an optional
**Density** overlay) and **Q-Q plot** for continuous variables; **Bar plot**
for nominal ones; **Box plot**, **Violin** and **Data** for comparing
distributions, especially when combined with **Split by**.

The **Box plot** shows the median, quartiles and whiskers, and can be annotated
with the **Mean** and with **Label outliers**.

## Results

### Descriptives

One column per variable (or per group, when splitting) and one row per
requested statistic. **N** and **Missing** are on by default, so unless you
untick them you can always see how much data each summary rests on.

### Frequencies

One table per nominal or ordinal variable, listing each level with its count,
its percentage of all cases, and its cumulative percentage.

### Extreme Values

The highest and lowest values of each variable with their row numbers. Use it
to find data-entry errors — an implausible extreme is often a typo rather than
a real observation.

### Plots

The plots requested above, one set per variable.

## Example

Using the `bugs` dataset, we want to understand the variables before analysing
them.

Put `LDLF` and `LDHF` into **Variables** and `Gender` into **Split by**.

Tick **Histogram** to see the shape of each distribution, and **Box plot** with
**Data** to compare the two genders. Under Statistics, add **Skewness** to
quantify any asymmetry you see in the histograms.

## References

Weighted statistics are computed with matrixStats and Hmisc; the plots are
drawn with ggplot2 and ggridges.

- Bengtsson, H. (2025). *matrixStats: Functions that Apply to Rows and Columns
  of Matrices* [R package].
- Harrell, F. E., Jr. (2026). *Hmisc: Harrell Miscellaneous* [R package].
- Wickham, H., Chang, W., Henry, L., Pedersen, T. L., Takahashi, K., Wilke,
  C., Woo, K., Yutani, H., Dunnington, D., van den Brand, T., & Posit, PBC
  (2026). *ggplot2: Create Elegant Data Visualisations Using the Grammar of
  Graphics* [R package].
- Wilke, C. (2025). *ggridges: Ridgeline Plots in ggplot2* [R package].
