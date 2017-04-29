context('mancova')

test_that('mancova works', {

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

    mancova <- jmv::mancova(data = data, deps = c("y1","y2","y3 plus","y4"), factors = c("age","sex"), cov="cov",
                            boxM = TRUE, shapiro = TRUE)

    # Test multivariate table
    expect_equal(1.74224110707516, mancova$multivar$getCell(rowNo=3, "f[pillai]")$value)

    # Test univariate table
    expect_equal(0.718326942610652, mancova$univar$getCell(rowKey="agesexy3 plus", "F")$value)
    expect_equal(0.195881684969791, mancova$univar$getCell(rowKey="covy1", "ss")$value)

    # Test Box's M test
    expect_equal(69.8733148653828, mancova$assump$boxM$getCell(rowNo=1, "chi")$value)
    expect_equal(50, mancova$assump$boxM$getCell(rowNo=1, "df")$value)
    expect_equal(0.0331165141642921, mancova$assump$boxM$getCell(rowNo=1, "p")$value)

    # Test Shapiro-Wilk test
    expect_equal(0.978275817603229, mancova$assump$shapiro$getCell(rowNo=1, "w")$value)
    expect_equal(0.0974751087311048, mancova$assump$shapiro$getCell(rowNo=1, "p")$value)

    # jmv::mancova(data = data, deps = "y1", factors = "age")

})
