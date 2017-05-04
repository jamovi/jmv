context('efa')

test_that('efa works', {

    set.seed(100)

    y <- rnorm(100)
    z <- rnorm(100)

    data <- list()
    data[["y1"]] <- y + .2 * rnorm(100)
    data[["y2"]] <- y + .2 * rnorm(100)
    data[["y3 plus"]] <- y + .2 * rnorm(100)
    data[["z1"]] <- z + .2 * rnorm(100)
    data[["z2"]] <- z + .2 * rnorm(100)

    attr(data, 'row.names') <- seq_len(length(data[[1]]))
    attr(data, 'class') <- 'data.frame'

    efa <- jmv::efa(data = data, vars = c("y1","y2","y3 plus","z1","z2"),
                    nFactorMethod = 'fixed', nFactors = 2, hideLoadings = .3, rotation = 'varimax')

    # Test loadings table
    expect_equal("", efa$loadings$getCell(rowNo=2, "pc2")$value)
    expect_equal(0.969985600224343, efa$loadings$getCell(rowNo=2, "pc1")$value)
    expect_equal(0.00443466058941522, efa$loadings$getCell(rowNo=4, "uniq")$value)
    expect_equal("y3 plus", efa$loadings$getCell(rowNo=3, "name")$value)

    expect_error(jmv::efa(data = data, vars = c("y1","y2","y3 plus","z1","z2"), nFactorMethod = "fixed", nFactors = 6),
                 'Number of factors cannot be bigger than number of variables', fixed=TRUE)

    data('ToothGrowth')
    efa2 <- jmv::efa(data = ToothGrowth, vars = c("len","dose"), factorSummary = TRUE, rotation = 'varimax')

    # expect_error(jmv::pca(data = data, vars = c("y1","y2","y3 plus","z1","z2"), hideLoadings = .3, rotation = "cluster",
    #                       nFactorMethod = "fixed", nFactors = 4),
    #              'Number of components cannot be bigger than number of variables', fixed=TRUE)

})
