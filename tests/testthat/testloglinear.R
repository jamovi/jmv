context('loglinear')

test_that('loglinear works', {

    data('mtcars')

    tab <- table('gear'=mtcars$gear, 'cyl'=mtcars$cyl)
    dat <- as.data.frame(tab)

    logLin <- jmv::logLinear(data = dat, factors = c("gear", "cyl"),  counts = "Freq",
                        blocks = list(list("gear", "cyl", c("gear", "cyl"))),
                        refLevels = list(
                            list(var="gear", ref="3"),
                            list(var="cyl", ref="4")))

    modelFit <- logLin$modelFit$asDF
    coef <- logLin$models[[1]]$coef$asDF

    # Test model fit table
    expect_equal(1, modelFit$r2mf[1], tolerance = 1e-3)
    expect_equal(41.382, modelFit$aic[1], tolerance = 1e-3)

    # Test coefficients table
    expect_equal(2.079, coef$est[3], tolerance = 1e-3)
    expect_equal(1.225, coef$se[6], tolerance = 1e-3)
    expect_equal(0.571, coef$p[4], tolerance = 1e-3)
})
