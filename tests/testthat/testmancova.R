testthat::context('mancova')

testthat::test_that('mancova works', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(100)

    data <- list()
    data[["y1"]] <- rnorm(100)
    data[["y2"]] <- rnorm(100)
    data[["y3 plus"]] <- rnorm(100)
    data[["y4"]] <- rnorm(100)

    data[["age"]] <- sample(c("young", "medium", "old"), 100, replace = TRUE)
    data[["sex"]] <- sample(c("male", "female"), 100, replace = TRUE)
    data[["cov"]] <- rnorm(100)

    attr(data, 'row.names') <- seq_len(length(data[[1]]))
    attr(data, 'class') <- 'data.frame'

    r <- jmv::mancova(
        data = data,
        deps = c("y1","y2","y3 plus","y4"),
        factors = c("age","sex"),
        covs="cov",
        boxM = TRUE,
        shapiro = TRUE
    )

    # Test multivariate table
    testthat::expect_equal(1.74224110707516, r$multivar$getCell(rowNo=3, "f[pillai]")$value)

    # Test univariate table
    testthat::expect_equal(0.718326942610652, r$univar$getCell(rowKey="agesexy3 plus", "F")$value)
    testthat::expect_equal(0.195881684969791, r$univar$getCell(rowKey="covy1", "ss")$value)

    # Test Box's M test
    testthat::expect_equal(69.8733148653828, r$assump$boxM$getCell(rowNo=1, "chi")$value)
    testthat::expect_equal(50, r$assump$boxM$getCell(rowNo=1, "df")$value)
    testthat::expect_equal(0.0331165141642921, r$assump$boxM$getCell(rowNo=1, "p")$value)

    # Test Shapiro-Wilk test
    testthat::expect_equal(0.978275817603229, r$assump$shapiro$getCell(rowNo=1, "w")$value)
    testthat::expect_equal(0.0974751087311048, r$assump$shapiro$getCell(rowNo=1, "p")$value)
})


})
