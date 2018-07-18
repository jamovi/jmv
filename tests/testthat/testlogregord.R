context('logregord')

test_that('logregord works', {

    set.seed(1337)

    y <- factor(sample(1:3, 100, replace = TRUE))
    x1 <- rnorm(100)
    x2 <- rnorm(100)

    df <- data.frame(y=y, x1=x1, x2=x2)

    logReg <- jmv::logRegOrd(data = df, dep = "y",
                             covs = c("x1", "x2"),
                             blocks = list(list("x1", "x2")))

    modelFit <- logReg$modelFit$asDF
    coef <- logReg$models[[1]]$coef$asDF

    # Test model fit table
    expect_equal(0.000568, modelFit$r2mf[1], tolerance = 1e-6)
    expect_equal(217.779, modelFit$dev[1], tolerance = 1e-3)

    # Test coefficients table
    expect_equal(0.058, coef$est[1], tolerance = 1e-3)
    expect_equal(0.172, coef$se[2], tolerance = 1e-3)
    expect_equal(0.848, coef$p[2], tolerance = 1e-3)
})
