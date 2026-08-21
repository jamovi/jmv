testthat::context('reliability')

testthat::test_that('All options in the reliability work (sunny)', {
    data <- lavaan::HolzingerSwineford1939
    names(data)[7] <- "x 1"

    r <- jmv::reliability(
        data = data,
        vars = c("x 1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9"),
        omegaScale = TRUE,
        meanScale = TRUE,
        sdScale = TRUE,
        alphaItems = TRUE,
        omegaItems = TRUE,
        meanItems = TRUE,
        sdItems = TRUE,
        itemRestCor = TRUE
    )

    # Test scale statistics table
    scaleTable <- r$scale$asDF
    testthat::expect_equal(4.216, scaleTable[['mean']], tolerance = 1e-3)
    testthat::expect_equal(0.661, scaleTable[['sd']], tolerance = 1e-3)
    testthat::expect_equal(0.76, scaleTable[['alpha']], tolerance = 1e-3)
    testthat::expect_equal(0.771, scaleTable[['omega']], tolerance = 1e-3)

    # Test item statistics table
    itemsTable <- r$items$asDF
    testthat::expect_equal(
        c('x 1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9'), itemsTable[['name']]
    )
    testthat::expect_equal(
        c(4.936, 6.088, 2.25, 3.061, 4.341, 2.186, 4.186, 5.527, 5.374),
        itemsTable[['mean']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(1.167, 1.177, 1.131, 1.164, 1.29, 1.096, 1.09, 1.013, 1.009),
        itemsTable[['sd']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.522, 0.275, 0.37, 0.581, 0.525, 0.595, 0.246, 0.369, 0.495),
        itemsTable[['itemRestCor']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.725, 0.764, 0.749, 0.715, 0.724, 0.714, 0.766, 0.748, 0.731),
        itemsTable[['alpha']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.743, 0.774, 0.764, 0.724, 0.731, 0.722, 0.776, 0.764, 0.75),
        itemsTable[['omega']],
        tolerance = 1e-3
    )
})

testthat::test_that('Analysis does not run (yet) with fewer than two variables', {
    df <- data.frame(y1 = c(4,4,3,4,8,0,9,8,8,6,0,3))
    r <- jmv::reliability(df, c("y1"))

    testthat::expect_true(is.na(r$scale$asDF$alpha))
})

testthat::test_that('Error is thrown for infinite values', {
    df <- data.frame(
        y1 = c(4,4,3,4,8,0,9,8,8,6,0,3),
        inf = c(Inf, Inf, -Inf, 2, 4, 2, 1.2, 3, 4, 2.3, 5.3, 2.23)
    )

    testthat::expect_error(
        jmv::reliability(df, c("y1", "inf")),
        "Item 'inf' contains infinite values",
        fixed=TRUE
    )
})

testthat::test_that('Error names the item that contains only missing values', {
    # GIVEN an item whose values are all missing
    df <- data.frame(
        y1 = c(4,4,3,4,8,0,9,8,8,6,0,3),
        y2 = c(3,2,5,4,6,1,7,7,6,5,1,2),
        allNA = rep(NA_real_, 12)
    )

    # WHEN running reliability
    # THEN the analysis is rejected naming the all missing item, and only it
    error <- testthat::expect_error(
        jmv::reliability(df, c("y1", "y2", "allNA")),
        "Item 'allNA' contains only missing values",
        fixed=TRUE
    )
    testthat::expect_length(conditionMessage(error), 1)
})

testthat::test_that('Error is thrown when no rows are left after missing values', {
    # GIVEN two items that never have a value on the same row
    df <- data.frame(
        y1 = c(4,4,3,4,8,0,NA,NA,NA,NA,NA,NA),
        y2 = c(NA,NA,NA,NA,NA,NA,7,7,6,5,1,2)
    )

    # WHEN running reliability
    # THEN the analysis is rejected saying the dataset has no rows left
    testthat::expect_error(
        jmv::reliability(df, c("y1", "y2")),
        regexp="contains 0 rows",
        ignore.case=TRUE
    )
})

testthat::test_that('Error is thrown when a single row is left after missing values', {
    # GIVEN items that share a value on one row only
    df <- data.frame(
        y1 = c(4,4,3,4,8,0,9,NA,NA,NA,NA,NA),
        y2 = c(NA,NA,NA,NA,NA,NA,7,7,6,5,1,2)
    )

    # WHEN running reliability
    # THEN the analysis is rejected saying the items have no variance
    testthat::expect_error(
        jmv::reliability(df, c("y1", "y2")),
        "Item 'y1' has no variance",
        fixed=TRUE
    )
})
