context('anovarmnp')

data('bugs', package = 'jmv')

test_that('anovarmnp works', {

    r <- jmv::anovaRMNP(bugs, measures = c('LDLF', 'LDHF', 'HDLF', 'HDHF'), desc = TRUE)

    main <- as.data.frame(r$table)
    desc <- as.data.frame(r$desc)

    # Test anova table
    expect_equal(55.834, main$stat, tolerance = 1e-3)
    expect_equal(3, main$df, tolerance = 1e-3)
    expect_equal(4.558e-12, main$p, tolerance = 1e-3)

    # Test desc table
    expect_equal(5.659, desc$mean[1], tolerance = 1e-3)
    expect_equal(8, desc$median[2], tolerance = 1e-3)
    expect_equal(7.823864, desc$mean[4], tolerance = 1e-3)
})
