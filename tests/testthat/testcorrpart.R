context('corrpart')

test_that('corrpart works', {

    x <- c(8,51,2,74,1,91,5,25,1,59,5,32,7)
    y <- c(2,NA,NaN,3,-1,-2,1,1,-2,2,-2,-3,3)
    z <- c(0,4,19,5,9,15,1,4,19,10,13,7,5)

    data <- data.frame(x = x, y = y, z = z)

    corrmatrix <- jmv::corrPart(data, vars=vars(x, y), controls=vars(z))

    expect_equal(0.254, as.numeric(corrmatrix$matrix$getCell(rowKey="y", "x[r]")$value), tolerance = 1e-3)
    expect_equal(0.478, as.numeric(corrmatrix$matrix$getCell(rowKey="y", "x[rp]")$value), tolerance = 1e-3)

})
