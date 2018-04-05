context('proptest2')

test_that('proptest2 works', {

    dat <- data.frame(x=c(8, 15))

    r <- jmv::propTest2(dat, vars = 'x', areCounts = TRUE)

    table <- as.data.frame(r$table)

    # Test test table
    expect_equal(8, table$count[1], tolerance = 1e-3)
    expect_equal(23, table$total[2], tolerance = 1e-3)
    expect_equal(0.348, table$prop[1], tolerance = 1e-3)
    expect_equal(0.652, table$prop[2], tolerance = 1e-3)
    expect_equal(0.210, table$p[1], tolerance = 1e-3)
    expect_equal(0.210, table$p[2], tolerance = 1e-3)
})
