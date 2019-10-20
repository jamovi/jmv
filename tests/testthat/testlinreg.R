context('linreg')

test_that('linreg works', {

    set.seed(100)
    intercept <- rnorm(100) + 1
    a <- rnorm(100) * 2.5
    b <- rnorm(100) * .5
    c <- rnorm(100) * .1

    y <- intercept + a + b + c

    data <- list()
    data[["dep"]] <- y
    data[["var1"]] <- a
    data[["var 2"]] <- b
    data[["var3"]] <- c

    attr(data, 'row.names') <- seq_len(length(data[[1]]))
    attr(data, 'class') <- 'data.frame'

    dep <- "dep"
    covs <- c("var1", "var 2", "var3")
    blocks = list(list("var1", "var 2", "var3"))

    linreg <- jmv::linReg(data, dep=!!dep, covs=!!covs, blocks=blocks, stdEst = TRUE)

    modelFit <- linreg$modelFit$asDF
    coef <- linreg$models[[1]]$coef$asDF

    # Test model fit table
    expect_equal(0.875, modelFit$r[1], tolerance = 1e-3)
    expect_equal(0.766, modelFit$r2[1], tolerance = 1e-3)

    # Test coefficients table
    expect_equal(1.008, coef$est[1], tolerance = 1e-3)
    expect_equal(0.958, coef$se[4], tolerance = 1e-3)
    expect_true(is.na(coef$stdEst[1]))

    blocks2 = list(list("var1", "var 2", "var3", c("var1", "var 2")))
    linreg2 <- jmv::linReg(data, dep=!!dep, covs=!!covs, blocks=blocks2, stdEst = TRUE, ciStdEst = TRUE)

    coef2 <- linreg2$models[[1]]$coef$asDF

    # Test coefficients table
    expect_equal(0.903, coef2$stdEst[2], tolerance = 1e-3)
    expect_equal(0.189, coef2$stdEstLower[3], tolerance = 1e-3)
    expect_equal(0.394, coef2$stdEstUpper[3], tolerance = 1e-3)
})
