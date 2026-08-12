# Checking assumptions

Most analyses come with assumption checks — a normality test, a test of equal
variances, a Q-Q plot. They are easy to read backwards, and the convention that
has grown up around them gets the logic of the situation almost exactly
inverted.

## The reversed logic

An assumption check tests the null hypothesis that the assumption *holds*. A
small p-value is evidence the assumption is violated; a large one is the
reassuring result.

This is the opposite of every other p-value in the output, and it is worth
saying out loud when reading a table that mixes the two — a MANCOVA reporting
Box's M beside its multivariate tests wants opposite readings of the two
p-values in the same glance.

A large p-value is not proof the assumption holds. It means the data gave no
evidence against it, which in a small sample is not saying much.

## The sample size trap

Assumption tests have a property that makes them nearly backwards as
decision rules:

- **In small samples** they have little power, so violations pass unnoticed —
  and a small sample is exactly where a violation does the most damage.
- **In large samples** they detect departures far too trivial to affect
  anything — and a large sample is where the analysis is most robust to them.

So the test is least sensitive when it matters most, and most sensitive when it
matters least. Using it as a gate means being warned mainly in the cases where
the warning is least warranted.

The recommendation that follows is not to skip the checks but to stop treating
them as pass/fail. Look at the *size* of the departure, not only the p-value,
and let the plots carry most of the weight.

## Prefer the plots

The plots answer a question the tests do not: not *is there any departure* but
*how bad is it, and where*.

- **Q-Q plot** — sorted residuals against what a normal distribution would
  give. Points close to the diagonal indicate normality. Departures at the ends
  are heavy tails or outliers; a curve through the middle is skew. Mild wobble
  at the extreme ends is normal in any real sample.
- **Residual plots** — residuals against predicted values. A shapeless
  horizontal band is what you want. A funnel widening to one side is unequal
  variance; a curve is a relationship the model has fitted as a straight line
  and should not have.

A Q-Q plot that looks fine alongside a significant Shapiro-Wilk in a large
sample is the ordinary case, not a contradiction.

## What the individual checks do

| Check | Tests | Notes |
|---|---|---|
| Shapiro-Wilk | Normality | Applied to the residuals, or to the differences in a paired design |
| Levene's test | Equal variances across groups | The check behind the Student/Welch choice |
| Box's M | Equal covariance matrices | Very sensitive; not a pass/fail gate |
| Durbin-Watson | Independence of residuals | Meaningful when cases have an order, such as time |
| VIF, tolerance | Multicollinearity | Predictors overlapping, not a model assumption as such |
| Box-Tidwell | Linearity of the log odds | For logistic models |

## What to do when one fails

There is almost always a remedy that costs less than abandoning the analysis.

| Violated | Try |
|---|---|
| Equal variances | Welch's test, which does not assume it and costs almost nothing when variances are in fact equal |
| Normality | The rank-based alternative — Mann-Whitney, Wilcoxon, Kruskal-Wallis, Friedman |
| Linearity | A transformed predictor, or a term for the curve |
| Multicollinearity | Drop or combine the overlapping predictors |
| Independence | Nothing here. It needs a model that accounts for the structure |

Sample size matters to this decision. Tests in the linear model family are
fairly robust to non-normality once samples are reasonable, so a significant
normality test in a large sample rarely calls for switching to a rank-based
test — and switching costs you the means, the effect sizes and the interval
estimates that the original analysis reported.

## The assumption you cannot test

Independence is the one assumption with no check in the output, and it is the
one whose violation does the most damage. It comes from how the data were
collected, not from anything in the data: pupils within classes, repeated
measurements on the same person, patients within clinics.

No test will flag it, and the analyses here cannot repair it. Where the
measurements are grouped or repeated, the design has to be matched by the
analysis — a repeated measures analysis, or a model with the grouping in
it — rather than checked for after the fact.

## References

- Shapiro, S. S., & Wilk, M. B. (1965). An analysis of variance test for
  normality (complete samples). *Biometrika, 52*(3–4), 591–611.
- Zimmerman, D. W. (2004). A note on preliminary tests of equality of
  variances. *British Journal of Mathematical and Statistical Psychology,
  57*(1), 173–181.
- Wilkinson, L., & Task Force on Statistical Inference. (1999). Statistical
  methods in psychology journals: Guidelines and explanations. *American
  Psychologist, 54*(8), 594–604.
