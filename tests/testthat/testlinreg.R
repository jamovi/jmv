context('linreg')

test_that('linreg works', {

    set.seed(100)
    intercept <- rnorm(100) + 1
    a <- rnorm(100) * 2.5
    b <- rnorm(100) * .5
    c <- rnorm(100) * .1

    y <- intercept + a + b + c

    data <- list()
    data[["dep"]] <- y
    data[["var1"]] <- a
    data[["var 2"]] <- b
    data[["var3"]] <- c

    attr(data, 'row.names') <- seq_len(length(data[[1]]))
    attr(data, 'class') <- 'data.frame'

    dep <- "dep"
    blocks <- list(
        c("var1"),
        c("var 2"),
        c("var3")
    )

    linreg <- jmv::linReg(data, dep, blocks, stdEst = TRUE, durbin = TRUE, collin = TRUE,
                          desc = TRUE, cooks = TRUE)

    # Test model fit table
    expect_equal(153.189584266139, linreg$modelFit$getCell(rowNo=2, "f")$value)
    expect_equal(307.185787574925, linreg$modelFit$getCell(rowNo=3, "bic")$value)
    expect_equal(9.57393529009331e+22, linreg$modelFit$getCell(rowNo=1, "bf")$value)

    # Test model comparison table
    expect_equal(97, linreg$modelComp$getCell(rowNo=1, "df2")$value)
    expect_equal(29.8143924006029, linreg$modelComp$getCell(rowNo=1, "f")$value)
    expect_equal(3.65836089098396e-07, linreg$modelComp$getCell(rowNo=1, "p")$value)
    expect_equal(28712.736907087, linreg$modelComp$getCell(rowNo=1, "bf")$value)

    # Test coefficients table
    expect_false(is.numeric(linreg$coef$getCell(rowNo=3, "stdEst")$value))
    expect_equal(0.20609477986347, linreg$coef$getCell(rowNo=8, "se")$value)
    expect_equal("var1", linreg$coef$getCell(rowNo=4, "terms")$value)

    # Test descriptives table
    expect_equal(0.0278520916466284, linreg$dataSummary$desc$getCell(rowNo=2, "mean")$value)
    expect_equal(-0.182194478667355, linreg$dataSummary$desc$getCell(rowNo=2, "median")$value)
    expect_equal(0.0108086086897674, linreg$dataSummary$desc$getCell(rowNo=4, "se")$value)

    # Test cooks table
    expect_equal(0.0108766143946069, linreg$dataSummary$cooks$getCell(rowNo=1, "mean")$value)
    expect_equal(0.0187571747075716, linreg$dataSummary$cooks$getCell(rowNo=1, "sd")$value)
    expect_equal(1.58025802217294e-07, linreg$dataSummary$cooks$getCell(rowNo=1, "min")$value)

    # Test DW table
    expect_equal(-0.260593973186961, linreg$assump$durbin$getCell(rowNo=1, "autoCor")$value)
    expect_equal(2.51048535456813, linreg$assump$durbin$getCell(rowNo=1, "dw")$value)
    expect_equal(0.008, linreg$assump$durbin$getCell(rowNo=1, "p")$value)

    # Test collinearity table
    expect_equal(1.07906860128795, linreg$assump$collin$getCell(rowNo=1, 'vif')$value)
    expect_equal(0.920805815542556, linreg$assump$collin$getCell(rowNo=2, 'tol')$value)
})
