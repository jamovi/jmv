context('proptestn')

data('HairEyeColor')
dat <- as.data.frame(HairEyeColor)

test_that('proptestn works', {

    r <- jmv::propTestN(dat, var = 'Eye', counts = 'Freq', ratio = c(1,1,1,1))

    props <- as.data.frame(r$props)
    test <- as.data.frame(r$tests)

    # Test props table
    expect_equal(220, props$count[1], tolerance = 1e-3)
    expect_equal(215, props$count[2], tolerance = 1e-3)
    expect_equal(0.157, props$prop[3], tolerance = 1e-3)
    expect_equal(0.108, props$prop[4], tolerance = 1e-3)

    # Test test table
    expect_equal(133.473, test$chi, tolerance = 1e-3)
    expect_equal(3, test$df, tolerance = 1e-3)
    expect_equal(9.651e-29, test$p, tolerance = 1e-3)
})
