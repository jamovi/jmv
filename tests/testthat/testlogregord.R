testthat::context('logregord')

testthat::test_that('All options in the logRegOrd work (sunny)', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    df <- data.frame(
        `dep 1` = sample(letters[1:3], 100, replace = TRUE),
        `cov 1` = rnorm(100),
        `cov 2` = rnorm(100),
        `factor 1` = sample(LETTERS[20:21], 100, replace = TRUE),
        check.names = FALSE,
        stringsAsFactors = TRUE
    )

    r <- jmv::logRegOrd(
        data = df,
        dep = "dep 1",
        covs = c("cov 1", "cov 2"),
        factors = c("factor 1"),
        blocks = list(list("cov 1", "cov 2", "factor 1")),
        refLevels = list(
            list(var="factor 1", ref=LETTERS[20])
        ),
        modelTest = TRUE,
        bic = TRUE,
        pseudoR2 = c("r2mf", "r2cs", "r2n"),
        omni = TRUE,
        thres = TRUE,
        ci = TRUE,
        OR = TRUE,
        ciOR = TRUE
    )

    # Test model fit table
    modelFitTable <- r$modelFit$asDF
    testthat::expect_equal(1, modelFitTable[['model']])
    testthat::expect_equal(216.412, modelFitTable[['dev']], tolerance = 1e-3)
    testthat::expect_equal(226.412, modelFitTable[['aic']], tolerance = 1e-3)
    testthat::expect_equal(239.437, modelFitTable[['bic']], tolerance = 1e-3)
    testthat::expect_equal(0.007, modelFitTable[['r2mf']], tolerance = 1e-3)
    testthat::expect_equal(0.005, modelFitTable[['r2cs']], tolerance = 1e-3)
    testthat::expect_equal(0.01, modelFitTable[['r2n']], tolerance = 1e-3)
    testthat::expect_equal(1.492, modelFitTable[['chi']], tolerance = 1e-3)
    testthat::expect_equal(3, modelFitTable[['df']])
    testthat::expect_equal(0.684, modelFitTable[['p']], tolerance = 1e-3)

    #  Test omnibus likelihood ratio tests table
    lrtTable <- r$models[[1]]$lrt$asDF
    testthat::expect_equal(c('cov 1', 'cov 2', 'factor 1'), lrtTable[['term']])
    testthat::expect_equal(c(0.151, 0.046, 1.368), lrtTable[['chi']], tolerance = 1e-3)
    testthat::expect_equal(c(1, 1, 1), lrtTable[['df']])
    testthat::expect_equal(c(0.697, 0.83, 0.242), lrtTable[['p']], tolerance = 1e-3)

    # Test model coefficients table
    coefTable <- r$models[[1]]$coef$asDF
    testthat::expect_equal(c('cov 1', 'cov 2', 'factor 1:', 'U – T'), coefTable[['term']])
    testthat::expect_equal(c(0.075, 0.037, NA, -0.435), coefTable[['est']], tolerance = 1e-3)
    testthat::expect_equal(c(-0.304, -0.301, NA, -1.172), coefTable[['lower']], tolerance = 1e-3)
    testthat::expect_equal(c(0.462, 0.377, NA, 0.293), coefTable[['upper']], tolerance = 1e-3)
    testthat::expect_equal(c(0.194, 0.172, NA, 0.373), coefTable[['se']], tolerance = 1e-3)
    testthat::expect_equal(c(0.389, 0.214, NA, -1.167), coefTable[['z']], tolerance = 1e-3)
    testthat::expect_equal(c(0.698, 0.83, NA, 0.243), coefTable[['p']], tolerance = 1e-3)
    testthat::expect_equal(c(1.078, 1.038, NA, 0.647), coefTable[['odds']], tolerance = 1e-3)
    testthat::expect_equal(c(0.738, 0.74, NA, 0.31), coefTable[['oddsLower']], tolerance = 1e-3)
    testthat::expect_equal(c(1.587, 1.458, NA, 1.341), coefTable[['oddsUpper']], tolerance = 1e-3)

    # Test model thresholds table
    thresTable <- r$models[[1]]$thres$asDF
    testthat::expect_equal(c('a | b', 'b | c'), thresTable[['term']])
    testthat::expect_equal(c(-0.915, 0.255), thresTable[['est']], tolerance = 1e-3)
    testthat::expect_equal(c(0.282, 0.267), thresTable[['se']], tolerance = 1e-3)
    testthat::expect_equal(c(-3.249, 0.956), thresTable[['z']], tolerance = 1e-3)
    testthat::expect_equal(c(0.001, 0.339), thresTable[['p']], tolerance = 1e-3)
    testthat::expect_equal(c(0.401, 1.291), thresTable[['odds']], tolerance = 1e-3)
})

testthat::test_that("Model comparison works", {
    # GIVEN a dataset with a dependent variable and two covariates
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)
    df <- data.frame(
        dep = sample(letters[1:3], 100, replace = TRUE),
        cov1 = rnorm(100),
        cov2 = rnorm(100),
        stringsAsFactors = TRUE
    )

    # WHEN an ordinal regression model is fitted in two blocks
    r <- jmv::logRegOrd(
        data = df,
        dep = "dep",
        covs = c("cov1", "cov2"),
        blocks = list(list("cov1"), list("cov2"))
    )

    # THEN the model comparison table contains the model fit statistics
    modelCompTable <- r$modelComp$asDF
    testthat::expect_equal(0.0369, modelCompTable$chi, tolerance = 1e-3)
    testthat::expect_equal(1, modelCompTable$df)
    testthat::expect_equal(0.848, modelCompTable$p, tolerance = 1e-3)
})

testthat::test_that("Analysis works with global weights", {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    weights <- sample(1:10, 100, replace=TRUE)

    df <- data.frame(
        dep = sample(letters[1:3], 100, replace = TRUE),
        cov = rnorm(100),
        factor = sample(LETTERS[20:21], 100, replace = TRUE),
        check.names = FALSE,
        stringsAsFactors = TRUE
    )
    attr(df, "jmv-weights") <- weights

    r <- jmv::logRegOrd(
        data = df,
        dep = "dep",
        covs = "cov",
        factors = "factor",
        blocks = list(list("cov", "factor")),
        refLevels = list(
            list(var="factor", ref=LETTERS[20])
        )
    )

    # Test model fit table
    modelFitTable <- r$modelFit$asDF
    testthat::expect_equal(1, modelFitTable[['model']])
    testthat::expect_equal(1246.297, modelFitTable[['dev']], tolerance = 1e-3)
    testthat::expect_equal(1254.297, modelFitTable[['aic']], tolerance = 1e-3)
    testthat::expect_equal(0.028, modelFitTable[['r2mf']], tolerance = 1e-3)

    # Test model coefficients table
    coefTable <- r$models[[1]]$coef$asDF
    testthat::expect_equal(c(-0.41, NA, 0.326), coefTable[['est']], tolerance = 1e-3)
    testthat::expect_equal(c(0.079, NA, 0.156), coefTable[['se']], tolerance = 1e-3)
    testthat::expect_equal(c(-5.204, NA, 2.092), coefTable[['z']], tolerance = 1e-3)
    testthat::expect_equal(c(0, NA, 0.036), coefTable[['p']], tolerance = 1e-3)
})

testthat::test_that('Model fit table contains sample size footnote', {
    df <- data.frame(
        y = sample(letters[1:3], 15, replace = TRUE),
        x = rnorm(15),
        check.names = FALSE,
        stringsAsFactors = TRUE
    )

    r <- jmv::logRegOrd(
        df,
        dep="y",
        covs="x",
        blocks=list(list("x"))
    )

    testthat::expect_match(r$modelFit$notes$n$note, "N=15")
})

params <- list(
    list(refLevels = list(list(var="factor", ref="X")), info = "Non-existing reference levels"),
    list(refLevels = NULL, info = "No reference levels"),
    list(refLevels = list(list(var="wrong_factor", ref="A")), info = "Wrong variable name")
)
testthat::test_that('Reference level defaults to first level for faulty reference levels', {
    for (param in params) {
        # GIVEN a dataset with a factor with two levels
        df <- data.frame(
            dep = rep(letters[1:3], length.out=10),
            factor = rep(LETTERS[1:2], length.out=10),
            stringsAsFactors = TRUE
        )

        # WHEN a ordinal logistic regression is fitted with reference level set to a non-existing level
        r <- jmv::logRegOrd(
            df,
            dep = "dep",
            factors = "factor",
            blocks = list(list("factor")),
            refLevels = param$refLevels
        )

        # THEN the reference level should default to the first level
        testthat::expect_match(r$models[[1]]$coef$asDF$term[2], "B – A", info=param$info)
        # AND a warning is added informing the user that the user defined reference level does not
        #   exist and therefore was changed to the first level
        testthat::expect_match(r[[1]]$content, "reference level was not found", info=param$info)
    }
})
