context('loglinear')

testthat::test_that('loglinear works', {

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
    testthat::expect_equal(1, modelFit$r2mf[1], tolerance = 1e-3)
    testthat::expect_equal(41.382, modelFit$aic[1], tolerance = 1e-3)

    # Test coefficients table
    testthat::expect_equal(2.079, coef$est[3], tolerance = 1e-3)
    testthat::expect_equal(1.225, coef$se[6], tolerance = 1e-3)
    testthat::expect_equal(0.571, coef$p[4], tolerance = 1e-3)
})

testthat::test_that('Provide error message when factor contains fewer than two levels', {
    df <- data.frame(x = rep(1, 7))

    testthat::expect_error(
        jmv::logLinear(
            data=df,
            factors="x",
            blocks=list(list("x")),
            refLevels = list(list(var="x", ref="1"))
        ),
        "Factors must have at least two levels"
    )
})

# testthat::test_that('Sensible error message is provided var with only missing values', {
#     df <- data.frame(x = factor(rep(NA, 7)))
#
#     testthat::expect_error(
#         jmv::logLinear(
#             data=df,
#             factors="x",
#             blocks=list(list("x")),
#             refLevels = list(list(var="x", ref=NULL))
#         ),
#         "Some sensible error message"
#     )
# })
