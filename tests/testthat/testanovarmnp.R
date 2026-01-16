testthat::context('anovarmnp')

testthat::test_that('All options in the anovaRMNP work (sunny)', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    data <- data.frame(
        x1 = sample(1:10, 20, replace=TRUE),
        x2 = sample(1:10, 20, replace=TRUE),
        x3 = sample(1:10, 20, replace=TRUE),
        x4 = sample(1:10, 20, replace=TRUE)
    )

    r <- jmv::anovaRMNP(
        data,
        measures = c('x1', 'x2', 'x3', 'x4'),
        desc = TRUE,
        pairs = TRUE
    )

    # Test main table
    mainTable <- r$table$asDF
    testthat::expect_equal(2.198, mainTable[['stat']], tolerance = 1e-3)
    testthat::expect_equal(3, mainTable[['df']])
    testthat::expect_equal(0.532, mainTable[['p']], tolerance = 1e-3)

    # Test descriptives table
    descTable <-r$desc$asDF
    testthat::expect_equal(c('x1', 'x2', 'x3', 'x4'), descTable[['level']])
    testthat::expect_equal(as.vector(sapply(data, mean)), descTable[['mean']], tolerance = 1e-3)
    testthat::expect_equal(as.vector(sapply(data, median)), descTable[['median']], tolerance = 1e-3)

    # Test pairwise comparisons
    pairsTable <- r$comp$asDF
    testthat::expect_equal(c('x1', 'x1', 'x1', 'x2', 'x2', 'x3'), pairsTable[['i1']])
    testthat::expect_equal(c('x2', 'x3', 'x4', 'x3', 'x4', 'x4'), pairsTable[['i2']])
    testthat::expect_equal(
        c(0.88, 0.063, 0.566, 0.943, 1.446, 0.503), pairsTable[['stat']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.382, 0.95, 0.574, 0.349, 0.154, 0.617), pairsTable[['p']], tolerance = 1e-3
    )
})


testthat::test_that('calcDurbin works for a complete block design without p-value adjustment', {
    # example for a complete block design from
    # https://www.rdocumentation.org/packages/PMCMRplus/versions/4.0/topics/posthoc.durbin.test
    # results are equal to those from PMCMRplus::durbinAllPairsTest(y, p.adj="none"), diff < 1e-15

    # GIVEN a matrix `y` representing a complete block design
    y <- matrix(
        c(
            3.88,  5.64,  5.76,  4.25,  5.91,  4.33, 30.58, 30.14, 16.92, 23.19, 26.74, 10.91, 
            25.24, 33.52, 25.45, 18.85, 20.45, 26.67, 4.44, 7.94, 4.04, 4.4, 4.23, 4.36, 29.41, 
            30.72, 32.92, 28.23, 23.35, 12, 38.87, 33.12, 39.15, 28.06, 38.23, 26.65
        ), nrow = 6, ncol = 6, dimnames=list(1:6, LETTERS[1:6])
    )
    
    # WHEN calling `calcDurbin` with default arguments (no p-value adjustment)
    r <- calcDurbin(y)

    # THEN the returned pairwise comparisons are as expected
    expect_equal(
        r[, 1], 
        c("A", "A", "A", "A", "A", "B", "B", "B", "B", "C", "C", "C", "D", "D", "E")
    )
    expect_equal(
        r[, 2], 
        c("B", "C", "D", "E", "F", "C", "D", "E", "F", "D", "E", "F", "E", "F", "F")
    )
    # AND the test statistics are as expected
    expect_equal(
        r[, 3], 
        c(
            4.4821073, 5.0797216, 0.5976143, 5.6773359, 7.470179, 0.5976143, 3.8844930, 1.1952286,
            2.9880720, 4.4821073, 0.5976143, 2.390457, 5.0797216, 6.8725650, 1.792843
        ), tolerance = 1e-6
    )
    # AND the p-values are as expected
    expect_equal(
        r[, 4], 
        c(
            0.0001426546, 0.000030327980, 0.55547245630, 0.0000065360, 0.00000008006288, 
            0.55547245626, 0.0006661548, 0.243213110008, 0.00621361995, 0.0001426546, 
            0.555472456255, 0.02468018576662, 0.000030327983, 0.00000033329893, 0.08510737977997
        )
    )
})

testthat::test_that('calcDurbin works for a complete block design with Holm correction', {
    # using the same dataset with Holm correction for multiple comparisons
    # example for a complete block design from
    # https://www.rdocumentation.org/packages/PMCMRplus/versions/4.0/topics/posthoc.durbin.test
    # results are equal to those from PMCMRplus::durbinAllPairsTest(y, p.adj="holm"), diff < 1e-14

    # GIVEN a matrix `y` representing a complete block design
    y <- matrix(
        c(
            3.88,  5.64,  5.76,  4.25,  5.91,  4.33, 30.58, 30.14, 16.92, 23.19, 26.74, 10.91, 
            25.24, 33.52, 25.45, 18.85, 20.45, 26.67, 4.44, 7.94, 4.04, 4.4, 4.23, 4.36, 29.41, 
            30.72, 32.92, 28.23, 23.35, 12, 38.87, 33.12, 39.15, 28.06, 38.23, 26.65
        ), nrow = 6, ncol = 6, dimnames=list(1:6, LETTERS[1:6])
    )
    
    # WHEN calling `calcDurbin` with "holm" p-value adjustment
    r <- calcDurbin(y, "holm")

    # THEN the returned p-values are as expected
    expect_equal(
        r[, 4], 
        c(
            0.001426545736, 0.000363935798, 1, 0.000084968305, 0.000001200943, 1, 0.005329238412,
            0.972852440031, 0.043495339635, 0.001426545736, 1, 0.148081114600, 0.000363935798,
            0.000004666185, 0.425536898900
        )
    )
})

testthat::test_that('calcDurbin works with data from real-statistics.com (Conover test after KW)', {
    # example from https://real-statistics.com/one-way-analysis-of-variance-anova/kruskal-wallis-test/conover-test-after-kw/ 
    # results are equal to those from PMCMRplus::durbinAllPairsTest(y, p.adj="none"), diff < 1e-15

    # GIVEN a smaller matrix `y` representing data across three groups
    y <- matrix(
        c(
            18, 49, 33, 19, 24, 17, 48, 22, 35, 30,
            81, 32, 42, 62, 37, 44, 38, 47, 49, 31,
            48, 31, 25, 22, 30, 30, 32, 15, 64, 50
        ), ncol = 3, dimnames = list(seq(10), c("Ctl", "New", "Old"))
    )
    
    # WHEN calling `calcDurbin` on this data
    r <- calcDurbin(y)

    # THEN the returned pairwise comparisons are as expected
    expect_equal(
        r[, 1], 
        c("Ctl", "Ctl", "New")
    )
    expect_equal(
        r[, 2], 
        c("New", "Old", "Old")
    )
    # AND the test statistics are as expected
    expect_equal(
        r[, 3], 
        c(2.5, 0.5, 2.0)
    )
    # AND the p-values are as expected
    expect_equal(
        r[, 4], 
        c(0.02230802, 0.62313246, 0.06082147)
    )
})

testthat::test_that('calcDurbin works with randomly generated data', {
    # example from https://medium.com/@serurays/introduction-to-statistical-testing-in-r-part-3-non-parametric-tests-2cde06ae2893
    # results are equal to those from PMCMRplus::durbinAllPairsTest(y, p.adj="none"), diff <- 1e-16
    
    # GIVEN a matrix `y` generated with randomized probabilities using a fixed seed
    set.seed(9)
    y <- matrix(
        c(
            sample(1:10, 30, replace = TRUE, prob = c(0.20, 0.20, 0.20, 0.15, 0.15, 0.10, 0.05, 0.05, 0, 0)),
            sample(1:10, 30, replace = TRUE, prob = c(0.10, 0.10, 0.15, 0.20, 0.20, 0.10, 0.10, 0.10, 0, 0)),
            sample(1:10, 30, replace = TRUE, prob = c(0.05, 0.05, 0.10, 0.15, 0.25, 0.20, 0.10, 0.05, 0, 0))
        ),
        ncol = 3, dimnames = list(seq(30), c("A", "B", "C"))
    )
    
    # WHEN calling `calcDurbin` on the randomly generated matrix
    r <- calcDurbin(y)

    # THEN the returned pairwise comparisons are as expected
    expect_equal(
        r[, 1], 
        c("A", "A", "B")
    )
    expect_equal(
        r[, 2], 
        c("B", "C", "C")
    )
    # AND the test statistics are as expected
    expect_equal(
        r[, 3], 
        c(2.74883920, 2.67650132, 0.07233787)
    )
    # AND the p-values are as expected
    expect_equal(
        r[, 4], 
        c(0.007959595, 0.009656408, 0.942581904)
    )
})
