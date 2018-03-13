context('logregmulti')

test_that('logregmulti works', {

    set.seed(1337)

    y <- sample(c('A', 'B', 'C'), 100, replace = TRUE)
    x1 <- rnorm(100)
    x2 <- rnorm(100)

    df <- data.frame(y=y, x1=x1, x2=x2)

    logReg <- jmv::logRegMulti(data = df, dep = "y",
                               covs = c("x1", "x2"),
                               blocks = list(list("x1", "x2")),
                               refLevels = list(list(var="y", ref="A")))

    modelFit <- logReg$modelFit$asDF
    coef <- logReg$models[[1]]$coef$asDF

    # Test model fit table
    expect_equal(0.000870, modelFit$r2mf[1], tolerance = 1e-6)
    expect_equal(217.714, modelFit$dev[1], tolerance = 1e-3)

    # Test coefficients table
    expect_equal(-0.160, coef$est[1], tolerance = 1e-3)
    expect_equal(0.242, coef$se[3], tolerance = 1e-3)
    expect_equal(0.917, coef$p[2], tolerance = 1e-3)
})
