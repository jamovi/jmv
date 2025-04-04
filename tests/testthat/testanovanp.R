testthat::context('anovanp')

testthat::test_that('All options in the anovaNP work (sunny)', {
    factor <- as.factor(c(rep(c("a", "b", "c"), each=6)))
    dep <-  c(0,4,19,5,9,15,1,4,19,10,13,7,5,12,2,23,6,13)

    data <- data.frame(f = factor, dep = dep)

    r <- jmv::anovaNP(data, deps='dep', group='f', es=TRUE, pairs=TRUE)

    # Test anova table
    main <- r$table$asDF
    testthat::expect_equal(0.167, main$chiSq, tolerance = 1e-3)
    testthat::expect_equal(2, main$df, tolerance = 1e-3)
    testthat::expect_equal(0.920, main$p, tolerance = 1e-3)
    testthat::expect_equal(0.0098, main$es, tolerance = 1e-4)

    # Test comparisons table
    comp <- r$comparisons[[1]]$asDF
    testthat::expect_equal(c(0.227, 0.567, 0.340), comp$W, tolerance = 1e-3)
    testthat::expect_equal(c(0.986, 0.915, 0.969), comp$p, tolerance = 1e-3)
})

testthat::test_that('Dunn test in the anovaNP works (sunny)', {
    factor <- as.factor(c(rep(c("a", "b", "c"), each=6)))
    dep <-  c(0,4,19,5,9,15,1,4,19,10,13,7,5,12,2,23,6,13)

    data <- data.frame(f = factor, dep = dep)

    r <- jmv::anovaNP(data, deps='dep', group='f', pairsDunn=TRUE)

    # Dunn's Test comparisonsDunn table
    comp <- r$comparisonsDunn[[1]]$asDF
    testthat::expect_equal(c(-0.163, -0.406, -0.244), comp$z, tolerance = 1e-3)
    testthat::expect_equal(c(0.871, 0.684, 0.807), comp$p, tolerance = 1e-3)
    testthat::expect_equal(c(1.000, 1.000, 1.000), comp$padj, tolerance = 1e-3)
})

testthat::test_that('Dunn test in the anovaNP works (rainy)', {
    factor_rainy <- as.factor(c(rep(c("x", "y", "z"), each=6)))
    dep_rainy <- c(7, 12, NA, 8, 3, 15, 20, 18, 16, NA, 22, 19, 5, 9, 14, 11, 8, NA)

    data_rainy <- data.frame(f = factor_rainy, dep = dep_rainy)
    r <- jmv::anovaNP(data_rainy, deps='dep', group='f', es=TRUE, pairsDunn=TRUE)

    # Test anova table
    main <- r$table$asDF
    testthat::expect_equal(9.41, main$chiSq, tolerance = 1e-3)
    testthat::expect_equal(2, main$df, tolerance = 1e-3)
    testthat::expect_equal(0.009, main$p, tolerance = 1e-3)
    testthat::expect_equal(0.672, main$es, tolerance = 1e-4)
    
    # Dunn's Test comparisonsDunn table
    comp <- r$comparisonsDunn[[1]]$asDF
    testthat::expect_equal(c(-2.137, -0.216, 1.921), comp$z, tolerance = 1e-3)
    testthat::expect_equal(c(0.033, 0.829, 0.055), comp$p, tolerance = 1e-3)
    testthat::expect_equal(c(0.098, 1.000, 0.164), comp$padj, tolerance = 1e-3)
})
