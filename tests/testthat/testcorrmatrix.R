context('corrmatrix')

test_that('corrmatrix works', {

    x <- as.factor(c(NA,rep(c("a", "b"), 6)))
    y <- c(8,51,2,74,1,91,5,25,1,59,5,32,7) # breaks equality of variance
    z <- c(2,NA,NaN,3,-1,-2,1,1,-2,2,-2,-3,3)
    w <- c(0,4,19,5,9,15,1,4,19,10,13,7,5)

    data <- data.frame(x = x, y = y, z = z, w = w)

    corrmatrix <- jmv::corrMatrix(data, c("y","z","w"))

    expect_equal(0.0834, as.numeric(corrmatrix$matrix$getCell(rowKey="y", "z[r]")$value), tolerance = 1e-3)
    expect_equal(0.0315, as.numeric(corrmatrix$matrix$getCell(rowKey="z", "w[rp]")$value), tolerance = 1e-3)

})
