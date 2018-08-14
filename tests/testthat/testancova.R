context('ancova')

test_that('ancova works', {

    factor <- as.factor(c(rep(c("a", "b", "c"), each=6)))
    cov <- c(8,51,2,74,1,91,5,25,1,59,5,32,7,9,2,54,21,12)
    dep <-  c(0,4,19,5,9,15,1,4,19,10,13,7,5,12,2,23,6,13)

    data <- data.frame(f = factor, c =cov, dep = dep)

    r <- jmv::ancova(data, dep='dep', factors='f', covs='c')

    main <- as.data.frame(r$main)

    # Test anova table
    expect_equal(734.999, main$ss[3], tolerance = 1e-3)
    expect_equal(1, main$df[2], tolerance = 1e-3)
    expect_equal(0.871, main$p[1], tolerance = 1e-3)
})
