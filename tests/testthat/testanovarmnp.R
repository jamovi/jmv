context('anovarmnp')

test_that('anovarmnp works', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    data <- data.frame(
        x1 = sample(1:10, 20, replace=TRUE),
        x2 = sample(1:10, 20, replace=TRUE),
        x3 = sample(1:10, 20, replace=TRUE),
        x4 = sample(1:10, 20, replace=TRUE)
    )

    r <- jmv::anovaRMNP(data, measures = c('x1', 'x2', 'x3', 'x4'), desc = TRUE)

    main <- as.data.frame(r$table)
    desc <- as.data.frame(r$desc)

    # Test anova table
    expect_equal(2.198, main$stat, tolerance = 1e-3)
    expect_equal(3, main$df)
    expect_equal(0.532, main$p, tolerance = 1e-3)

    # Test desc table
    expect_equal(mean(data$x1), desc$mean[1], tolerance = 1e-3)
    expect_equal(mean(data$x2), desc$mean[2], tolerance = 1e-3)
    expect_equal(mean(data$x3), desc$mean[3], tolerance = 1e-3)
    expect_equal(mean(data$x4), desc$mean[4], tolerance = 1e-3)
    expect_equal(median(data$x1), desc$median[1])
    expect_equal(median(data$x1), desc$median[1])
    expect_equal(median(data$x1), desc$median[1])
    expect_equal(median(data$x1), desc$median[1])
})
