context('reliability')

test_that('reliability works', {

    data <- list()
    data[["y1"]] <- c(4,4,3,4,8,0,9,8,8,6,0,3)
    data[["y2"]] <- c(4,4,3,4,8,0,9,8,8,6,0,3)
    data[["y3 plus"]] <- c(NA,NaN,3,-1,-2,1,1,-2,2,-2,-3,3)
    data[["inf"]] <- c(Inf, Inf, -Inf, 2, 4, 2, 1.2, 3, 4, 2.3, 5.3, 2.23)

    attr(data, 'row.names') <- seq_len(length(data[[1]]))
    attr(data, 'class') <- 'data.frame'

    rel <- jmv::reliability(data, c("y1", "y2", "y3 plus"), omegaScale = TRUE, meanScale = TRUE, alphaItems = TRUE, omegaItems = TRUE)

    expect_equal(0.636883, rel$scale$getCell(rowNo=1, "alpha")$value)
    expect_equal(0, rel$items$getCell(rowNo=1, "omega")$value)
    expect_error(jmv::reliability(data, c("y1", "inf")), "Item 'inf' contains infinite values", fixed=TRUE)
    expect_true(is.null(jmv::reliability(data, c("y1"))$scale$getCell(rowNo=1, "alpha")$value))
})

