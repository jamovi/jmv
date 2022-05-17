
context('anova')

data('ToothGrowth')

test_that('Correct error message is with perfect fit of the model', {
    df <- data.frame(
        dep = c(90, 87, 75, 60, 35, 50, 65, 70),
        var = factor(1:8)
    )

    testthat::expect_error(
        jmv::ANOVA(df, dep='dep', factors='var', qq=TRUE),
        "perfect fit"
    )
})

test_that('anova works', {

    r <- jmv::ANOVA(ToothGrowth, dep='len', factors=c('dose', 'supp'))

    main <- as.data.frame(r$main)

    # Test anova table
    expect_equal(108.319, main$ss[3])
    expect_equal(54, main$df[4])
    expect_equal(4.046291e-18, main$p[1])

})
