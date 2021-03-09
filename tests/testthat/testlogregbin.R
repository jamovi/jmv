context('logregbin')

test_that('logregbin works general', {
    set.seed(1337)

    N <- 100
    x1 <- rnorm(N)
    x2 <- rnorm(N)
    z <- 1 + 2*x1 + 3*x2
    pr <- 1 / ( 1 + exp(-z))
    y <- factor(rbinom(N, 1, pr))

    df <- data.frame(y=y, x1=x1, x2=x2)

    logReg <- jmv::logRegBin(data = df, dep = "y", covs = c("x1", "x2"),
                             blocks = list("x1", "x2"),
                             refLevels = list(list(var="y", ref="0")),
                             modelTest = TRUE, bic = TRUE,
                             pseudoR2 = c("r2mf", "r2cs", "r2n"),
                             omni = TRUE, ci = TRUE, OR = TRUE, ciOR = TRUE,
                             class = TRUE, acc = TRUE, spec = TRUE, sens = TRUE,
                             auc = TRUE, collin = TRUE,
                             emMeans = ~x1, emmPlots = FALSE, emmTables = TRUE)

    # Test model fit table
    modelFit <- logReg$modelFit$asDF
    expect_equal(39.039, modelFit$dev[2], tolerance = 1e-4)
    expect_equal(45.039, modelFit$aic[2], tolerance = 1e-5)
    expect_equal(52.854, modelFit$bic[2], tolerance = 1e-5)
    expect_equal(0.701, modelFit$r2mf[2], tolerance = 1e-3)
    expect_equal(0.600, modelFit$r2cs[2], tolerance = 1e-3)
    expect_equal(0.823, modelFit$r2n[2], tolerance = 1e-3)
    expect_equal(91.645, modelFit$chi[2], tolerance = 1e-5)
    expect_equal(2, modelFit$df[2])
    expect_equal(0, modelFit$p[2], tolerance = 1e-3)

    # Test model comparison table
    modelComp <- logReg$modelComp$asDF
    expect_equal(72.433, modelComp$chi, tolerance = 1e-3)
    expect_equal(1, modelComp$df)
    expect_equal(0, modelComp$p, tolerance = 1e-3)

    #  Test omnibus likelihood ratio tests table
    lrt <- logReg$models[[2]]$lrt$asDF
    expect_equal(43.325, lrt$chi[1], tolerance = 1e-5)
    expect_equal(1, lrt$df[1])
    expect_equal(1.728e-17, lrt$p[2], tolerance = 1e-20)

    # Test coefficients table
    coef <- logReg$models[[2]]$coef$asDF
    expect_equal(0.926, coef$est[1], tolerance = 1e-3)
    expect_equal(1.166, coef$se[3], tolerance = 1e-3)
    expect_equal(0.000189, coef$p[2], tolerance = 1e-6)
    expect_equal(1.445, coef$lower[2], tolerance = 1e-4)
    expect_equal(7.215, coef$upper[3], tolerance = 1e-4)
    expect_equal(2.524, coef$odds[1], tolerance = 1e-4)
    expect_equal(4.242, coef$oddsLower[2], tolerance = 1e-4)
    expect_equal(1360.308, coef$oddsUpper[3], tolerance = 1e-6)

    # Test collinearity table
    col <- logReg$models[[2]]$assump$collin$asDF
    expect_equal(2.331, col$vif[1], tolerance = 1e-4)
    expect_equal(0.429, col$tol[1], tolerance = 1e-4)
    expect_equal(2.331, col$vif[2], tolerance = 1e-4)
    expect_equal(0.429, col$tol[2], tolerance = 1e-4)

    # Test emmeans table
    emmeans <- logReg$models[[2]]$emm[[1]]$emmTable$asDF
    expect_equal(-0.828, emmeans$x1[1], tolerance = 1e-3)
    expect_equal(0.199, emmeans$prob[1], tolerance = 1e-3)
    expect_equal(0.126, emmeans$se[1], tolerance = 1e-3)
    expect_equal(0.0502, emmeans$lower[1], tolerance = 1e-4)
    expect_equal(0.539, emmeans$upper[1], tolerance = 1e-4)
    expect_equal(0.864, emmeans$prob[2], tolerance = 1e-4)
    expect_equal(0.994, emmeans$prob[3], tolerance = 1e-3)

    # Test classification table
    class <- logReg$models[[2]]$pred$class$asDF
    expect_equal(28, class$`neg[0]`)
    expect_equal(8, class$`pos[0]`)
    expect_equal(5, class$`neg[1]`)
    expect_equal(59, class$`pos[1]`)
    expect_equal(77.778, class$`perc[0]`, tolerance = 1e-5)
    expect_equal(92.188, class$`perc[1]`, tolerance = 1e-5)

    # Test predictive measures table
    pred <- logReg$models[[2]]$pred$measures$asDF
    expect_equal(0.87, pred$accuracy, tolerance = 1e-3)
    expect_equal(0.778, pred$spec, tolerance = 1e-3)
    expect_equal(0.922, pred$sens, tolerance = 1e-3)
    expect_equal(0.976, pred$auc, tolerance = 1e-3)
})

test_that('logregbin works with factors', {
    set.seed(1337)

    N <- 100
    x <- sample(LETTERS[1:3], N, replace=TRUE)
    y <- sample(0:1, N, replace=TRUE)
    df <- data.frame(y=y, x=x)

    refLevels <- list(list(var="y", ref="0"),
                      list(var="x", ref="A"))

    logReg <- jmv::logRegBin(data = df, dep = "y", factors = "x",
                             blocks = list("x"), refLevels = refLevels)

    # Test coefficients table
    coef <- logReg$models[[1]]$coef$asDF
    expect_equal("x:", coef$term[2])
    expect_equal("B – A", coef$term[3])
    expect_equal("C – A", coef$term[4])
    expect_equal(-0.0606, coef$est[1], tolerance = 1e-3)
    expect_equal(-0.0824, coef$est[3], tolerance = 1e-3)
    expect_equal(0.112, coef$est[4], tolerance = 1e-3)
    expect_equal(0.348, coef$se[1], tolerance = 1e-3)
    expect_equal(0.515, coef$se[3], tolerance = 1e-3)
    expect_equal(0.473, coef$se[4], tolerance = 1e-3)
    expect_equal(-0.174, coef$z[1], tolerance = 1e-3)
    expect_equal(-0.160, coef$z[3], tolerance = 1e-3)
    expect_equal(0.236, coef$z[4], tolerance = 1e-3)
    expect_equal(0.862, coef$p[1], tolerance = 1e-3)
    expect_equal(0.873, coef$p[3], tolerance = 1e-3)
    expect_equal(0.813, coef$p[4], tolerance = 1e-3)
})
