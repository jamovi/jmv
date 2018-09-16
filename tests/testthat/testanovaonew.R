
context('anovaonew')

data('ToothGrowth')

test_that('anovaonew works', {

    dat <- ToothGrowth
    dat$dose <- factor(dat$dose)

    r <- jmv::anovaOneW(dat, deps = "len", group = "dose", fishers = TRUE, desc = TRUE, eqv = TRUE,
                        phMethod = "gamesHowell", phTest = TRUE)

    main <- as.data.frame(r$anova)
    desc <- as.data.frame(r$desc)
    levene <- as.data.frame(r$assump$eqv)
    postHoc <- r$postHoc[[1]]

    # Test anova table
    expect_equal(67.416, main$`F[fisher]`, tolerance = 1e-5)
    expect_equal(68.401, main$`F[welch]`, tolerance = 1e-5)
    expect_equal(37.743, main$`df2[welch]`, tolerance = 1e-5)
    expect_equal(2.8124e-13, main$`p[welch]`, tolerance = 1e-5)

    # Test descriptives table
    means <- as.numeric(tapply(dat$len, dat$dose, mean))
    sds <- as.numeric(tapply(dat$len, dat$dose, sd))
    ns <- as.numeric(tapply(dat$len, dat$dose, length))

    expect_equal(means, desc$mean, tolerance = 1e-5)
    expect_equal(sds, desc$sd, tolerance = 1e-5)
    expect_equal(ns, desc$num, tolerance = 1e-5)

    # Test levene's table
    expect_equal(0.73276, levene$F, tolerance = 1e-5)
    expect_equal(2, levene$df1, tolerance = 1e-5)
    expect_equal(57, levene$df2, tolerance = 1e-5)
    expect_equal(0.48505, levene$p, tolerance = 1e-5)

    # Test post-hoc table
    expect_equal(-6.4766, postHoc$getCell(rowKey="0.5", "1[t]")$value, tolerance = 1e-5)
    expect_equal(37.101, postHoc$getCell(rowKey="1", "2[df]")$value, tolerance = 1e-5)
    expect_equal(5.5686e-05, postHoc$getCell(rowKey="1", "2[p]")$value, tolerance = 1e-5)
    expect_equal(-15.495, postHoc$getCell(rowKey="0.5", "2[md]")$value, tolerance = 1e-5)

})
