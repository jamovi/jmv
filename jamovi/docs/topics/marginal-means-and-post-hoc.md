# Marginal means and post hoc tests

Both answer the question that follows a significant ANOVA — *which groups
actually differ, and by how much?* — and they are routinely confused with each
other and with the descriptive statistics. Marginal means describe; post hoc
tests compare. The numbers behind them are the same, and neither is the raw
group means.

## Estimated marginal means are not group means

An estimated marginal mean is the **model's prediction** for a group, not the
average of the observations in it. The two coincide in a balanced design with
no covariates, and part company as soon as either condition fails.

They differ for two reasons:

- **Covariates** — held constant. Each group's mean is predicted with every
  covariate set to its overall average, which is what makes the groups
  comparable. This is the whole point of an ANCOVA, and why the marginal means
  are the ones to report from one.
- **Cell weighting** — equal by default. Where an interaction's cells hold
  different numbers of cases, averaging over a factor can weight each cell
  equally or in proportion to how many cases it holds. jamovi weights them
  equally unless you untick **Equal cell weights**, so a marginal mean is the
  average of the cell predictions rather than of the people.

Equal weighting is usually what you want, because it describes the design
rather than the accident of who happened to be recruited into which cell. It
does mean that a marginal mean can sit outside the range of the raw group
means it summarises, which is correct rather than a fault.

Marginal means that do not match the descriptives are **the expected result,
not an error**. The mismatch tells you the design is unbalanced, a covariate is
doing work, or both.

## Post hoc tests compare marginal means

In **ANOVA**, **ANCOVA** and **Repeated Measures ANOVA**, the post hoc
comparisons are comparisons of the estimated marginal means — the same adjusted
quantities described above, differenced pair by pair. They are not computed
from the raw group means.

This is why a post hoc mean difference need not equal the difference between
two group means in the Descriptives table, and why hand-checking one against
the descriptives will not reproduce it whenever there is a covariate or an
unbalanced cell.

The **Cohen's d** available alongside each comparison is likewise built from
the model: the difference is divided by the model's residual standard
deviation, using its residual degrees of freedom. It is not the two-group d
that a t-test on the same pair would report.

## Choosing a correction

Comparing every pair of groups means running many tests, and the chance of at
least one false positive climbs with each. A correction holds that chance down.

| Correction | Use it when |
|---|---|
| **Tukey** | Comparing all pairs. The default, and the right choice for the usual case |
| **Holm** | Comparing a chosen subset. Uniformly more powerful than Bonferroni |
| **Bonferroni** | A few planned comparisons. Simple, widely recognised, conservative |
| **Scheffe** | Arbitrary comparisons decided after seeing the data. Very conservative |
| **No correction** | Rarely defensible for pairwise comparisons |

Two practical notes. **Holm dominates Bonferroni** — it rejects everything
Bonferroni rejects and sometimes more, at the same error rate — so
Bonferroni's only real advantage is that readers recognise it.

And jamovi computes **all five at once**, showing whichever you tick. Switching
between them costs nothing and refits nothing, which makes it tempting to pick
the one that gives the answer you wanted. Decide before you look.

## When the omnibus test and the post hoc tests disagree

Both directions happen, and neither is a bug.

**A significant *F*, no significant pair.** The *F* tests whether the means
differ *anywhere*, pooling all the evidence into one test; the pairwise
comparisons each use a fraction of it and then pay a correction on top. A
pattern spread thinly across several groups can be detectable in aggregate and
in no single pair.

**A significant pair, no significant *F*.** Rarer, and mostly a sign that the
correction is doing less work than the *F*'s single test did. Scheffe is the
one correction guaranteed to agree with the *F*: nothing is significant under
it unless the omnibus test was.

Requiring a significant *F* before looking at pairwise tests is a common house
rule. It is not a statistical requirement, and with Tukey the pairwise tests
already control the error rate on their own.

## Post hoc tests or contrasts?

The distinction is *when you decided*, not what you computed.

- **Contrasts** are comparisons specified before seeing the data, from the
  hypothesis. Fewer tests, more power, no correction needed for a small planned
  set.
- **Post hoc tests** are every pair, decided after. More tests, correction
  required, less power per test.

If you have a specific prediction, contrasts test it more sensitively than
scanning all pairs will. Deciding which comparisons matter after seeing which
ones are large, and then reporting them as planned, is the error the
corrections exist to prevent.

## Elsewhere in jamovi

The ANOVA family shares the machinery described above. Other analyses do not.

| Follow-up | Where it lives |
|---|---|
| Games-Howell, or Tukey | **One-Way ANOVA**, where both are off by default |
| DSCF, or Dunn's | **One-Way ANOVA (Non-parametric)** |
| Durbin-Conover | **Repeated Measures ANOVA (Non-parametric)** |

Games-Howell is the safer choice in a one-way design, for the same reason
Welch's test is: it costs little when variances are equal and protects you when
they are not.

**Linear Regression**, the logistic family and **Log-Linear Regression** offer
marginal means for their factor terms but no post hoc comparisons. Everything
in the first half of this document applies to those means; nothing in the
second half does.

## References

- Searle, S. R., Speed, F. M., & Milliken, G. A. (1980). Population marginal
  means in the linear model: An alternative to least squares means. *The
  American Statistician, 34*(4), 216–221.
- Holm, S. (1979). A simple sequentially rejective multiple test procedure.
  *Scandinavian Journal of Statistics, 6*(2), 65–70.
- Games, P. A., & Howell, J. F. (1976). Pairwise multiple comparison procedures
  with unequal n's and/or variances. *Journal of Educational Statistics, 1*(2),
  113–125.

Marginal means and post hoc comparisons are computed with emmeans.

- Lenth, R., & Piaskowski, J. (2026). *emmeans: Estimated Marginal Means, aka
  Least-Squares Means* [R package].
