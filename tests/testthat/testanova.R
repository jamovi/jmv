
context('anova')

data('ToothGrowth')

test_that('anova works', {

    r <- jmv::anova(ToothGrowth, dep='len', factors=c('dose', 'supp'), descStats = TRUE)

    main <- as.data.frame(r$main)
    desc <- as.data.frame(r$desc)

    # Test anova table
    expect_equal(108.319, main$ss[3])
    expect_equal(54, main$df[4])
    expect_equal(4.046291e-18, main$p[1])

    # Test desc table
    expect_equal(mean(ToothGrowth$len[ToothGrowth$dose == '1' & ToothGrowth$supp == 'OJ']), desc$mean[3])
    expect_equal(sd(ToothGrowth$len[ToothGrowth$dose == '0.5' & ToothGrowth$supp == 'VC']), desc$sd[2])
    expect_equal(length(ToothGrowth$len[ToothGrowth$dose == '2' & ToothGrowth$supp == 'VC']), desc$n[6])

})
