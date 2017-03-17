context('pca')

test_that('pca works', {

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

    pca <- jmv::pca(data = data, vars = c("y1","y2","y3 plus","z1","z2"), hideLoadings = .3)

    # Test loadings table
    expect_equal("", pca$loadings$getCell(rowNo=2, "pc2")$value)
    expect_equal(0.982044844526955, pca$loadings$getCell(rowNo=2, "pc1")$value)
    expect_equal(0.0282236641764453, pca$loadings$getCell(rowNo=4, "uniq")$value)
    expect_equal("y3 plus", pca$loadings$getCell(rowNo=3, "name")$value)

    pca2 <- jmv::pca(data = data, vars = c("y1","y2","y3 plus","z1","z2"),
                     nFactorMethod = "fixed",
                     nFactors = 1)

    expect_error(jmv::pca(data = data, vars = c("y1","y2","y3 plus","z1","z2"), nFactorMethod = "fixed", nFactors = 6),
                          'Number of components cannot be bigger than number of variables', fixed=TRUE)

    # expect_error(jmv::pca(data = data, vars = c("y1","y2","y3 plus","z1","z2"), hideLoadings = .3, rotation = "cluster",
    #                       nFactorMethod = "fixed", nFactors = 4),
    #              'Number of components cannot be bigger than number of variables', fixed=TRUE)

})
