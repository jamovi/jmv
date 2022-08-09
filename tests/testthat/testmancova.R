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

testthat::test_that('Provide error message when dependent variables are highly correlated', {
    df <- data.frame(
        dep1 = 1:10,
        dep2 = 1:10,
        factor = rep(letters[1:2], length.out=10),
        stringsAsFactors = TRUE
    )

    testthat::expect_error(
        jmv::mancova(data = df, deps = c("dep1","dep2"), factors = c("factor")),
        "Dependent variables are very highly correlated"
    )
})

testthat::test_that('Provide error message if residual degrees of freedom are equal to 0', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)
    df <- data.frame(
        dep1 = rnorm(22),
        dep2 = rnorm(22),
        factor1 = rep(letters[1:10], length.out=22),
        factor2 = rep(LETTERS[1:3], length.out=22),
        stringsAsFactors = TRUE
    )

    testthat::expect_error(
        jmv::mancova(data = df, deps = c("dep1","dep2"), factors = c("factor1", "factor2")),
        "Not enough degrees of freedom to estimate all the model effects"
    )
})
