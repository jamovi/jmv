testthat::context('logregbin')

testthat::test_that('All options in the logRegBin work (sunny)', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    N <- 100
    cov1 <- rnorm(N)
    cov2 <- rnorm(N)
    z <- 1 + 2*cov1 + 3*cov2
    pr <- 1 / ( 1 + exp(-z))
    dep <- factor(rbinom(N, 1, pr))

    df <- data.frame(
        `dep 1`=dep, `cov 1`=cov1, `cov 2`=cov2, check.names = FALSE
    )

    r <- jmv::logRegBin(
        data = df,
        dep = "dep 1",
        covs = c("cov 1", "cov 2"),
        blocks = list(list("cov 1", "cov 2")),
        refLevels = list(list(var="dep 1", ref="0")),
        modelTest = TRUE,
        bic = TRUE,
        pseudoR2 = c("r2mf", "r2cs", "r2n"),
        omni = TRUE,
        ci = TRUE,
        OR = TRUE,
        ciOR = TRUE,
        class = TRUE,
        acc = TRUE,
        spec = TRUE,
        sens = TRUE,
        auc = TRUE,
        collin = TRUE,
        emMeans = ~`cov 1` + `cov 2`,
        emmPlots = FALSE,
        emmTables = TRUE
    )

    # Test model fit table
    modelFitTable <- r$modelFit$asDF
    testthat::expect_equal(1, modelFitTable[['model']])
    testthat::expect_equal(39.039, modelFitTable[['dev']], tolerance = 1e-3)
    testthat::expect_equal(45.039, modelFitTable[['aic']], tolerance = 1e-3)
    testthat::expect_equal(52.854, modelFitTable[['bic']], tolerance = 1e-3)
    testthat::expect_equal(0.701, modelFitTable[['r2mf']], tolerance = 1e-3)
    testthat::expect_equal(0.6, modelFitTable[['r2cs']], tolerance = 1e-3)
    testthat::expect_equal(0.823, modelFitTable[['r2n']], tolerance = 1e-3)
    testthat::expect_equal(91.645, modelFitTable[['chi']], tolerance = 1e-3)
    testthat::expect_equal(2, modelFitTable[['df']])
    testthat::expect_equal(0, modelFitTable[['p']])

    #  Test omnibus likelihood ratio tests table
    lrtTable <- r$models[[1]]$lrt$asDF
    testthat::expect_equal(c('cov 1', 'cov 2'), lrtTable[['term']])
    testthat::expect_equal(c(43.325, 72.433), lrtTable[['chi']], tolerance = 1e-3)
    testthat::expect_equal(c(1, 1), lrtTable[['df']])
    testthat::expect_equal(c(0, 0), lrtTable[['p']], tolerance = 1e-3)

    # Test model coefficients table
    coefTable <- r$models[[1]]$coef$asDF
    testthat::expect_equal(c('Intercept', 'cov 1', 'cov 2'), coefTable[['term']])
    testthat::expect_equal(c(0.926, 3.042, 4.929), coefTable[['est']], tolerance = 1e-3)
    testthat::expect_equal(c(0.022, 1.445, 2.643), coefTable[['lower']], tolerance = 1e-3)
    testthat::expect_equal(c(1.829, 4.639, 7.215), coefTable[['upper']], tolerance = 1e-3)
    testthat::expect_equal(c(0.461, 0.815, 1.166), coefTable[['se']], tolerance = 1e-3)
    testthat::expect_equal(c(2.008, 3.734, 4.226), coefTable[['z']], tolerance = 1e-3)
    testthat::expect_equal(c(0.045, 0, 0), coefTable[['p']], tolerance = 1e-3)
    testthat::expect_equal(c(2.524, 20.944, 138.297), coefTable[['odds']], tolerance = 1e-3)
    testthat::expect_equal(c(1.023, 4.242, 14.06), coefTable[['oddsLower']], tolerance = 1e-3)
    testthat::expect_equal(c(6.229, 103.41, 1360.308), coefTable[['oddsUpper']], tolerance = 1e-3)

    # Test collinearity table
    collinTable <- r$models[[1]]$assump$collin$asDF
    testthat::expect_equal(c('cov 1', 'cov 2'), collinTable[['term']])
    testthat::expect_equal(c(2.331, 2.331), collinTable[['vif']], tolerance = 1e-3)
    testthat::expect_equal(c(0.429, 0.429), collinTable[['tol']], tolerance = 1e-3)

    # Test estimated marginal means table
    emmeansTable1 <- r$models[[1]]$emm[[1]]$emmTable$asDF
    testthat::expect_equal(c(-0.828, 0.237, 1.302), emmeansTable1[['cov 1']], tolerance = 1e-3)
    testthat::expect_equal(c(0.199, 0.864, 0.994), emmeansTable1[['prob']], tolerance = 1e-3)
    testthat::expect_equal(c(0.126, 0.062, 0.007), emmeansTable1[['se']], tolerance = 1e-3)
    testthat::expect_equal(c(0.05, 0.691, 0.939), emmeansTable1[['lower']], tolerance = 1e-3)
    testthat::expect_equal(c(0.539, 0.947, 0.999), emmeansTable1[['upper']], tolerance = 1e-3)

    emmeansTable2 <- r$models[[1]]$emm[[2]]$emmTable$asDF
    testthat::expect_equal(c(-0.965, 0.041, 1.048), emmeansTable2[['cov 2']], tolerance = 1e-3)
    testthat::expect_equal(c(0.043, 0.864, 0.999), emmeansTable2[['prob']], tolerance = 1e-3)
    testthat::expect_equal(c(0.038, 0.062, 0.002), emmeansTable2[['se']], tolerance = 1e-3)
    testthat::expect_equal(c(0.007, 0.691, 0.977), emmeansTable2[['lower']], tolerance = 1e-3)
    testthat::expect_equal(c(0.218, 0.947, 1), emmeansTable2[['upper']], tolerance = 1e-3)

    # Test classification table
    classTable <- r$models[[1]]$pred$class$asDF
    testthat::expect_equal('0', classTable[['name[0]']])
    testthat::expect_equal(28, classTable[['neg[0]']])
    testthat::expect_equal(8, classTable[['pos[0]']])
    testthat::expect_equal(77.778, classTable[['perc[0]']], tolerance = 1e-3)
    testthat::expect_equal('1', classTable[['name[1]']])
    testthat::expect_equal(5, classTable[['neg[1]']])
    testthat::expect_equal(59, classTable[['pos[1]']])
    testthat::expect_equal(92.188, classTable[['perc[1]']], tolerance = 1e-3)

    # Test predictive measures table
    measuresTable <- r$models[[1]]$pred$measures$asDF
    testthat::expect_equal(0.87, measuresTable[['accuracy']], tolerance = 1e-3)
    testthat::expect_equal(0.778, measuresTable[['spec']], tolerance = 1e-3)
    testthat::expect_equal(0.922, measuresTable[['sens']], tolerance = 1e-3)
    testthat::expect_equal(0.976, measuresTable[['auc']], tolerance = 1e-3)
})

testthat::test_that('logregbin works with factors', {
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
    testthat::expect_equal("x:", coef$term[2])
    testthat::expect_equal("B – A", coef$term[3])
    testthat::expect_equal("C – A", coef$term[4])
    testthat::expect_equal(-0.0606, coef$est[1], tolerance = 1e-3)
    testthat::expect_equal(-0.0824, coef$est[3], tolerance = 1e-3)
    testthat::expect_equal(0.112, coef$est[4], tolerance = 1e-3)
    testthat::expect_equal(0.348, coef$se[1], tolerance = 1e-3)
    testthat::expect_equal(0.515, coef$se[3], tolerance = 1e-3)
    testthat::expect_equal(0.473, coef$se[4], tolerance = 1e-3)
    testthat::expect_equal(-0.174, coef$z[1], tolerance = 1e-3)
    testthat::expect_equal(-0.160, coef$z[3], tolerance = 1e-3)
    testthat::expect_equal(0.236, coef$z[4], tolerance = 1e-3)
    testthat::expect_equal(0.862, coef$p[1], tolerance = 1e-3)
    testthat::expect_equal(0.873, coef$p[3], tolerance = 1e-3)
    testthat::expect_equal(0.813, coef$p[4], tolerance = 1e-3)
})

testthat::test_that('logregbin works with ordered factors', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    N <- 100
    x <- factor(sample(LETTERS[1:3], N, replace=TRUE), ordered = TRUE)
    y <- factor(sample(0:1, N, replace=TRUE), ordered = TRUE)
    df <- data.frame(y=y, x=x)

    refLevels <- list(list(var="y", ref="0"),
                      list(var="x", ref="A"))

    logReg <- jmv::logRegBin(data = df, dep = "y", factors = "x",
                             blocks = list("x"), refLevels = refLevels)

    # Test coefficients table
    coef <- logReg$models[[1]]$coef$asDF
    testthat::expect_equal("x:", coef$term[2])
    testthat::expect_equal("B – A", coef$term[3])
    testthat::expect_equal("C – A", coef$term[4])
    testthat::expect_equal(-0.0606, coef$est[1], tolerance = 1e-3)
    testthat::expect_equal(-0.0824, coef$est[3], tolerance = 1e-3)
    testthat::expect_equal(0.112, coef$est[4], tolerance = 1e-3)
    testthat::expect_equal(0.348, coef$se[1], tolerance = 1e-3)
    testthat::expect_equal(0.515, coef$se[3], tolerance = 1e-3)
    testthat::expect_equal(0.473, coef$se[4], tolerance = 1e-3)
    testthat::expect_equal(-0.174, coef$z[1], tolerance = 1e-3)
    testthat::expect_equal(-0.160, coef$z[3], tolerance = 1e-3)
    testthat::expect_equal(0.236, coef$z[4], tolerance = 1e-3)
    testthat::expect_equal(0.862, coef$p[1], tolerance = 1e-3)
    testthat::expect_equal(0.873, coef$p[3], tolerance = 1e-3)
    testthat::expect_equal(0.813, coef$p[4], tolerance = 1e-3)
})
