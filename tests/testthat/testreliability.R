context('reliability')

test_that('reliability works', {

    x <- c(4,4,3,4,8,0,9,8,8,6,0,3)
    y <- c(4,4,3,4,8,0,9,8,8,6,0,3)
    z <- c(NA,NaN,3,-1,-2,1,1,-2,2,-2,-3,3)
    inf <- c(Inf, Inf, -Inf, 2, 4, 2, 1.2, 3, 4, 2.3, 5.3, 2.23)

    data <- data.frame(x=x, y=y, z=z, inf=inf)
    rel <- jmv::reliability(data, c("x", "y", "z"), omegaScale = TRUE, meanScale = TRUE, alphaItems = TRUE, omegaItems = TRUE)

    expect_equal(0.636883, rel$scale$getCell(rowNo=1, "alpha")$value)
    expect_equal(6.59471190861493e-08, rel$items$getCell(rowNo=1, "omega")$value)
    expect_error(jmv::reliability(data, c("x", "inf")), "Item 'inf' contains infinite values", fixed=TRUE)
    expect_true(is.null(jmv::reliability(data, c("x"))$scale$getCell(rowNo=1, "alpha")$value))
})

