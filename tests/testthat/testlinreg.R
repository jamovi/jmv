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
    covs <- c("var1", "var 2", "var3")
    blocks = list(list("var1", "var 2", "var3"))

    linreg <- jmv::linReg(data, dep=!!dep, covs=!!covs, blocks=blocks, stdEst = TRUE)

    modelFit <- linreg$modelFit$asDF
    coef <- linreg$models[[1]]$coef$asDF

    # Test model fit table
    expect_equal(0.875, modelFit$r[1], tolerance = 1e-3)
    expect_equal(0.766, modelFit$r2[1], tolerance = 1e-3)

    # Test coefficients table
    expect_equal(1.008, coef$est[1], tolerance = 1e-3)
    expect_equal(0.958, coef$se[4], tolerance = 1e-3)
    expect_true(is.na(coef$stdEst[1]))

    blocks2 = list(list("var1", "var 2", "var3", c("var1", "var 2")))
    linreg2 <- jmv::linReg(data, dep=!!dep, covs=!!covs, blocks=blocks2, stdEst = TRUE, ciStdEst = TRUE)

    coef2 <- linreg2$models[[1]]$coef$asDF

    # Test coefficients table
    expect_equal(0.903, coef2$stdEst[2], tolerance = 1e-3)
    expect_equal(0.189, coef2$stdEstLower[3], tolerance = 1e-3)
    expect_equal(0.394, coef2$stdEstUpper[3], tolerance = 1e-3)

    # Test different intercept codings
    data <- ToothGrowth
    data$dose <- factor(data$dose)

    dep <- "len"
    factors <- c("dose", "supp")
    blocks = list(list("dose", "supp"))
    refLevels = list(list(var="supp", ref="OJ"),
                     list(var="dose", ref="0.5"))

    linreg3 <- jmv::linReg(data, dep=!!dep, factors=!!factors, blocks=blocks, refLevels=refLevels)
    coef3 <- linreg3$models[[1]]$coef$asDF

    expect_equal(12.455, coef3$est[1], tolerance = 1e-3)

    linreg4 <- jmv::linReg(data, dep=!!dep, factors=!!factors, blocks=blocks, refLevels=refLevels, intercept='grandMean')
    coef4 <- linreg4$models[[1]]$coef$asDF

    expect_equal(18.813, coef4$est[1], tolerance = 1e-3)

    linreg4 <- jmv::linReg(data, dep=!!dep, factors=!!factors, blocks=blocks, refLevels=refLevels,
                           emMeans=~ supp, emmTables=TRUE, emmPlots=FALSE)

    emmeans <- linreg4$models[[1]]$emm[[1]]$emmTable$asDF

    expect_equal(20.663, emmeans$emmean[1], tolerance = 1e-3)

})

test_that('cooks summary in linreg works', {

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
    covs <- c("var1", "var 2", "var3")
    blocks = list(list("var1", "var 2", "var3"))

    linreg <- jmv::linReg(data, dep=!!dep, covs=!!covs, blocks=blocks, cooks=TRUE)

    cooksTable <- linreg$models[[1]]$dataSummary$cooks$asDF

    expect_equal(0.0109, cooksTable$mean, tolerance = 1e-4)
    expect_equal(0.0031, cooksTable$median, tolerance = 1e-4)
    expect_equal(0.0188, cooksTable$sd, tolerance = 1e-4)
    expect_equal(0.0000, cooksTable$min, tolerance = 1e-4)
    expect_equal(0.0966, cooksTable$max, tolerance = 1e-4)
})
