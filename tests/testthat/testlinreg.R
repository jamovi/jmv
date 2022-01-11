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

    linreg <- jmv::linReg(
        data,
        dep=!!dep,
        covs=!!covs,
        blocks=blocks,
        cooks=TRUE
    )

    cooksTable <- linreg$models[[1]]$dataSummary$cooks$asDF

    expect_equal(0.0109, cooksTable$mean, tolerance = 1e-4)
    expect_equal(0.0031, cooksTable$median, tolerance = 1e-4)
    expect_equal(0.0188, cooksTable$sd, tolerance = 1e-4)
    expect_equal(0.0000, cooksTable$min, tolerance = 1e-4)
    expect_equal(0.0966, cooksTable$max, tolerance = 1e-4)
})

test_that('emmeans table in linreg works with covariate with only two unique values', {

    set.seed(100)
    data <- data.frame(
        dep = rnorm(100),
        var1 = rnorm(100),
        var2 = sample(0:1, 100, replace = TRUE)
    )

    dep <- "dep"
    covs <- c("var1", "var2")
    blocks = list(list("var1", "var2"))

    linreg <- jmv::linReg(
        data,
        dep=!!dep,
        covs=!!covs,
        blocks=blocks,
        emMeans = ~var1:var2,
        emmTables = TRUE
    )

    emmeansTable <- linreg$models[[1]]$emm[[1]]$emmTable$asDF

    expect_equal(0.0077, emmeansTable$emmean[1], tolerance = 1e-4)
    expect_equal(0.1441, emmeansTable$se[2], tolerance = 1e-4)
    expect_equal(-0.1564, emmeansTable$lower[4], tolerance = 1e-4)
    expect_equal(0.1623, emmeansTable$upper[6], tolerance = 1e-4)
    expect_equal(0.2515, emmeansTable$emmean[7], tolerance = 1e-4)
    expect_equal(0.1821, emmeansTable$se[9], tolerance = 1e-4)
})

test_that("analysis shows warning note on singular fit", {

    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    N <- 20
    data <- data.frame(
        x1 = sample(letters[1:4], N, replace = TRUE),
        x2 = sample(letters[1:4], N, replace = TRUE),
        y = rnorm(N)
    )

    dep <- "y"
    factors <- c("x1", "x2")
    blocks = list(list("x1", "x2", c("x1", "x2")))
    refLevels = list(list(var="x1", ref="a"),
                     list(var="x2", ref="a"))

    r <- jmv::linReg(
        data,
        dep=!!dep,
        factors=!!factors,
        blocks=blocks,
        refLevels=refLevels,
        anova = TRUE,
        collin = TRUE
    )

    noteAnova <- r$models[[1]]$anova$notes$alias$note
    noteCoef <- r$models[[1]]$coef$notes$alias$note
    noteVIF <- r$models[[1]]$assump$collin$notes$alias$note

    expect_equal(noteAnova, "Linear model contains aliased coefficients (singular fit)")
    expect_equal(noteCoef, "Linear model contains aliased coefficients (singular fit)")
    expect_equal(noteVIF, "Linear model contains aliased coefficients (singular fit)")
})

test_that("analysis works for covariate with one unique value", {

    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    df <- data.frame(
        x = rep(1, 100),
        y = rnorm(100)
    )

    r <- jmv::linReg(
        df,
        dep="y",
        covs="x",
        blocks=list(list("x")),
    )

    coef <- r$models[[1]]$coef$asDF
    expect_equal(0.237, coef[1, "est"], tolerance = 1e-4)
    expect_equal(NaN, coef[2, "est"])
})

test_that("analysis throws error for factor with one level", {

    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    df <- data.frame(
        x = factor(rep(1, 100)),
        y = rnorm(100)
    )

    testthat::expect_error(
        {
            jmv::linReg(
                df,
                dep="y",
                factors="x",
                blocks=list(list("x")),
                refLevels = list(list(var="x", ref="1"))
            )
        },
        regexp = "needs to have at least 2 levels"
    )
})

test_that("analysis works with weights", {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    df <- data.frame(
        weights = abs(rnorm(100)),
        dep = rnorm(100),
        cov = rnorm(100),
        factor = factor(sample(LETTERS[1:3], 100, replace=TRUE))
    )

    refLevels = list(list(var="factor", ref="A"))

    r <- jmv::linReg(
        df,
        dep="dep",
        covs="cov",
        factors="factor",
        weights="weights",
        blocks=list(list("cov", "factor")),
        refLevels=refLevels,
    )

    coef <- r$models[[1]]$coef
    coefDf <- coef$asDF

    testthat::expect_equal("Weighted by 'weights'", coef$notes$weights$note)
    testthat::expect_equal(coefDf$est[1], -0.100, tolerance = 1e-3)
    testthat::expect_equal(coefDf$se[2], 0.089, tolerance = 1e-3)
    testthat::expect_equal(coefDf$t[4], 1.004, tolerance = 1e-3)
    testthat::expect_equal(coefDf$p[5], 0.247, tolerance = 1e-3)
})

test_that("analysis throws error with negative weights", {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    df <- data.frame(
        weights = rnorm(100),
        dep = rnorm(100),
        cov = rnorm(100)
    )

    testthat::expect_error(
        {
            jmv::linReg(
                df,
                dep="dep",
                covs="cov",
                weights="weights",
                blocks=list(list("cov")),
            )
        },
        regexp = "Negative weights are not permitted"
    )
})

