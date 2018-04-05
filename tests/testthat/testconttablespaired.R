context('conttablespaired')

test_that('conttablespaired works', {

    dat <- data.frame(
        `1st survey` = c('Approve', 'Approve', 'Disapprove', 'Disapprove'),
        `2nd survey` = c('Approve', 'Disapprove', 'Approve', 'Disapprove'),
        `Counts` = c(794, 150, 86, 570),
        check.names=FALSE)

    r <- jmv::contTablesPaired(dat, rows = '1st survey', cols = '2nd survey', counts = 'Counts')

    freq <- as.data.frame(r$freqs)
    test <- as.data.frame(r$test)

    # Test frequencies table
    expect_equal(794, freq$`1[count]`[1], tolerance = 1e-3)
    expect_equal(570, freq$`2[count]`[2], tolerance = 1e-3)
    expect_equal(1600, freq$`.total[count]`[3], tolerance = 1e-3)

    # Test test table
    expect_equal(17.356, test$`value[mcn]`, tolerance = 1e-3)
    expect_equal(3.099e-05, test$`p[mcn]`, tolerance = 1e-3)
    expect_equal(1600, test$`value[n]`, tolerance = 1e-3)
})
