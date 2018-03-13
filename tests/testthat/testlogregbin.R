context('logregbin')

test_that('logregbin works', {

    set.seed(1337)
    x1 <- rnorm(100)
    x2 <- rnorm(100)
    z <- 1 + 2*x1 + 3*x2
    pr <- 1 / ( 1 + exp(-z))
    y <- factor(rbinom(100, 1, pr))

    df <- data.frame(y=y, x1=x1, x2=x2)

    logReg <- jmv::logRegBin(data = df, dep = "y", covs = c("x1", "x2"),
                             blocks = list(list("x1", "x2")),
                             refLevels = list(list(var="y", ref="0")))

    modelFit <- logReg$modelFit$asDF
    coef <- logReg$models[[1]]$coef$asDF

    # Test model fit table
    expect_equal(0.701, modelFit$r2mf[1], tolerance = 1e-3)
    expect_equal(39.039, modelFit$dev[1], tolerance = 1e-3)

    # Test coefficients table
    expect_equal(0.926, coef$est[1], tolerance = 1e-3)
    expect_equal(1.166, coef$se[3], tolerance = 1e-3)
    expect_equal(0.000189, coef$p[2], tolerance = 1e-6)
})
