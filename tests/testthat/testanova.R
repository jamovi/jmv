
context('anova')

data('ToothGrowth')

testthat::test_that('Correct error message is displayed with perfect fit of the model', {
    df <- data.frame(
        dep = c(90, 87, 75, 60, 35, 50, 65, 70),
        var = factor(1:8)
    )

    testthat::expect_error(
        jmv::ANOVA(df, dep='dep', factors='var', qq=TRUE),
        "perfect fit"
    )
})

testthat::test_that('anova works', {

    r <- jmv::ANOVA(ToothGrowth, dep='len', factors=c('dose', 'supp'))

    main <- as.data.frame(r$main)

    # Test anova table
    testthat::expect_equal(108.319, main$ss[3])
    testthat::expect_equal(54, main$df[4])
    testthat::expect_equal(4.046291e-18, main$p[1])

})

testthat::test_that('Provide error message when dep variable contains only one unique value', {
    df <- data.frame(
        dep = c(1, 1, 1, 1, 1, 1, 1),
        var = factor(c(1, 2, 1, 2, 1, 2, 1))
    )

    testthat::expect_error(
        jmv::ANOVA(formula=dep~var, data=df),
        "Dependent variable 'dep' contains only one unqiue value"
    )
})

testthat::test_that('Provide error message when a variable contains only missing values', {
    df <- data.frame(
        dep = 1:7,
        var = rep(NA, 7)
    )

    testthat::expect_error(
        jmv::ANOVA(formula=dep~var, data=df),
        "The dataset contains 0 rows"
    )
})

# test_that('Sensible error message is provided with dependent var with an infinte value', {
#     df <- data.frame(
#         dep = c(1:6, Inf),
#         var = rep(1:2, length.out = 7)
#     )
#
#     testthat::expect_error(
#         jmv::ANOVA(formula=dep~var, data=df),
#         "Some sensible error message"
#     )
# })
