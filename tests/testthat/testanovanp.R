context('anovanp')

test_that('anovanp works', {

    factor <- as.factor(c(rep(c("a", "b", "c"), each=6)))
    dep <-  c(0,4,19,5,9,15,1,4,19,10,13,7,5,12,2,23,6,13)

    data <- data.frame(f = factor, dep = dep)

    r <- jmv::anovaNP(data, deps='dep', group='f')

    main <- as.data.frame(r$table)

    # Test anova table
    expect_equal(0.167, main$chiSq, tolerance = 1e-3)
    expect_equal(2, main$df, tolerance = 1e-3)
    expect_equal(0.920, main$p, tolerance = 1e-3)
})
