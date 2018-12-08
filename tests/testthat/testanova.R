
context('anova')

data('ToothGrowth')

test_that('anova works', {

    r <- jmv::ANOVA(ToothGrowth, dep='len', factors=c('dose', 'supp'))

    main <- as.data.frame(r$main)

    # Test anova table
    expect_equal(108.319, main$ss[3])
    expect_equal(54, main$df[4])
    expect_equal(4.046291e-18, main$p[1])

})
