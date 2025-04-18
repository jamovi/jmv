testthat::context('logregmulti')

testthat::test_that('All options in the logRegMulti work (sunny)', {
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

    r <- jmv::logRegMulti(
        data = df,
        dep = "dep 1",
        covs = c("cov 1", "cov 2"),
        factors = "factor 1",
        blocks = list(list("cov 1", "cov 2", "factor 1")),
        refLevels = list(
            list(var="dep 1", ref=letters[1]),
            list(var="factor 1", ref=LETTERS[20])
        ),
        modelTest = TRUE,
        bic = TRUE,
        pseudoR2 = c("r2mf", "r2cs", "r2n"),
        omni = TRUE,
        ci = TRUE,
        OR = TRUE,
        ciOR = TRUE,
        emMeans = ~ `cov 1` + `cov 2` + `factor 1`,
        emmPlots = FALSE,
        emmTables = TRUE
    )

    # Test model fit table
    modelFitTable <- r$modelFit$asDF
    testthat::expect_equal(1, modelFitTable[['model']])
    testthat::expect_equal(212.869, modelFitTable[['dev']], tolerance = 1e-3)
    testthat::expect_equal(228.869, modelFitTable[['aic']], tolerance = 1e-3)
    testthat::expect_equal(249.71, modelFitTable[['bic']], tolerance = 1e-3)
    testthat::expect_equal(0.023, modelFitTable[['r2mf']], tolerance = 1e-3)
    testthat::expect_equal(0.017, modelFitTable[['r2cs']], tolerance = 1e-3)
    testthat::expect_equal(0.032, modelFitTable[['r2n']], tolerance = 1e-3)
    testthat::expect_equal(5.034, modelFitTable[['chi']], tolerance = 1e-3)
    testthat::expect_equal(6, modelFitTable[['df']])
    testthat::expect_equal(0.539, modelFitTable[['p']], tolerance = 1e-3)

    #  Test omnibus likelihood ratio tests table
    lrtTable <- r$models[[1]]$lrt$asDF
    testthat::expect_equal(c('cov 1', 'cov 2', 'factor 1'), lrtTable[['term']])
    testthat::expect_equal(c(0.148, 0.055, 4.845), lrtTable[['chi']], tolerance = 1e-3)
    testthat::expect_equal(c(2, 2, 2), lrtTable[['df']])
    testthat::expect_equal(c(0.929, 0.973, 0.089), lrtTable[['p']], tolerance = 1e-3)

    # Test model coefficients table
    coefTable <- r$models[[1]]$coef$asDF
    testthat::expect_equal(
        c('b - a', 'b - a', 'b - a', 'b - a', 'b - a', 'c - a', 'c - a', 'c - a', 'c - a', 'c - a'),
        coefTable[['dep']]
    )
    testthat::expect_equal(
        c(
            'Intercept',
            'cov 1',
            'cov 2',
            'factor 1:',
            'U – T',
            'Intercept',
            'cov 1',
            'cov 2',
            'factor 1:',
            'U – T'
        ),
        coefTable[['term']]
    )
    testthat::expect_equal(
        c(0.41, -0.003, 0.035, NA, -1.15, 0.511, 0.081, 0.052, NA, -0.622),
        coefTable[['est']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(-0.324, -0.542, -0.452, NA, -2.201, -0.209, -0.407, -0.386, NA, -1.572),
        coefTable[['lower']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(1.144, 0.536, 0.522, NA, -0.099, 1.23, 0.568, 0.49, NA, 0.327),
        coefTable[['upper']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.374, 0.275, 0.249, NA, 0.536, 0.367, 0.249, 0.223, NA, 0.485),
        coefTable[['se']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(1.096, -0.01, 0.141, NA, -2.144, 1.392, 0.325, 0.233, NA, -1.284),
        coefTable[['z']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.273, 0.992, 0.888, NA, 0.032, 0.164, 0.745, 0.816, NA, 0.199),
        coefTable[['p']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(1.507, 0.997, 1.036, NA, 0.317, 1.667, 1.084, 1.053, NA, 0.537),
        coefTable[['odds']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.724, 0.582, 0.636, NA, 0.111, 0.812, 0.666, 0.68, NA, 0.208),
        coefTable[['oddsLower']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(3.139, 1.709, 1.686, NA, 0.906, 3.423, 1.765, 1.632, NA, 1.387),
        coefTable[['oddsUpper']],
        tolerance = 1e-3
    )


    # Test estimated marginal means table
    emmeansTable1 <- r$models[[1]]$emm[[1]]$emmTable$asDF
    testthat::expect_equal(
        c(-0.852, -0.852, -0.852, 0.116, 0.116, 0.116, 1.083, 1.083, 1.083),
        emmeansTable1[['cov 1']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c('a', 'b', 'c', 'a', 'b', 'c', 'a', 'b', 'c'), emmeansTable1[['dep 1']]
    )
    testthat::expect_equal(
        c(0.34, 0.289, 0.371, 0.33, 0.28, 0.39, 0.32, 0.271, 0.409),
        emmeansTable1[['prob']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.066, 0.063, 0.069, 0.046, 0.044, 0.049, 0.065, 0.062, 0.07),
        emmeansTable1[['se']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.187, 0.144, 0.213, 0.224, 0.178, 0.277, 0.171, 0.127, 0.248),
        emmeansTable1[['lower']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.493, 0.435, 0.529, 0.437, 0.382, 0.502, 0.47, 0.415, 0.57),
        emmeansTable1[['upper']],
        tolerance = 1e-3
    )

    emmeansTable2 <- r$models[[1]]$emm[[2]]$emmTable$asDF
    testthat::expect_equal(
        c(-1.149, -1.149, -1.149, -0.08, -0.08, -0.08, 0.988, 0.988, 0.988),
        emmeansTable2[['cov 2']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c('a', 'b', 'c', 'a', 'b', 'c', 'a', 'b', 'c'), emmeansTable2[['dep 1']]
    )
    testthat::expect_equal(
        c(0.341, 0.279, 0.381, 0.33, 0.28, 0.39, 0.32, 0.281, 0.399),
        emmeansTable2[['prob']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.066, 0.063, 0.069, 0.046, 0.044, 0.049, 0.065, 0.063, 0.07),
        emmeansTable2[['se']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.188, 0.134, 0.222, 0.224, 0.178, 0.277, 0.171, 0.135, 0.238),
        emmeansTable2[['lower']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.493, 0.423, 0.54, 0.437, 0.382, 0.502, 0.469, 0.427, 0.559),
        emmeansTable2[['upper']],
        tolerance = 1e-3
    )

    emmeansTable3 <- r$models[[1]]$emm[[3]]$emmTable$asDF
    testthat::expect_equal(c('T', 'T', 'T', 'U', 'U', 'U'), emmeansTable3[['factor 1']])
    testthat::expect_equal(c('a', 'b', 'c', 'a', 'b', 'c'), emmeansTable3[['dep 1']])
    testthat::expect_equal(
        c(0.239, 0.36, 0.401, 0.421, 0.2, 0.379), emmeansTable3[['prob']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.06, 0.068, 0.069, 0.07, 0.057, 0.069), emmeansTable3[['se']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.1, 0.203, 0.241, 0.26, 0.07, 0.22), emmeansTable3[['lower']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.379, 0.516, 0.561, 0.582, 0.331, 0.537), emmeansTable3[['upper']], tolerance = 1e-3
    )
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

    # WHEN a multinomial regression model is fitted in two blocks
    r <- jmv::logRegMulti(
        data = df,
        dep = "dep",
        covs = c("cov1", "cov2"),
        blocks = list(list("cov1"), list("cov2"))
    )

    # THEN the model comparison table contains the model fit statistics
    modelCompTable <- r$modelComp$asDF
    testthat::expect_equal(0.0359, modelCompTable$chi, tolerance = 1e-3)
    testthat::expect_equal(2, modelCompTable$df)
    testthat::expect_equal(0.982, modelCompTable$p, tolerance = 1e-3)
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

    refLevels = list(list(var="dep", ref=letters[1]), list(var="factor", ref=LETTERS[20]))

    r <- jmv::logRegMulti(
        df,
        dep = "dep",
        covs = "cov",
        factors = "factor",
        blocks = list(list("cov", "factor")),
        refLevels = refLevels,
    )

    # Test model fit table
    modelFitTable <- r$modelFit$asDF
    testthat::expect_equal(1, modelFitTable[['model']])
    testthat::expect_equal(1241.675, modelFitTable[['dev']], tolerance = 1e-3)
    testthat::expect_equal(1253.675, modelFitTable[['aic']], tolerance = 1e-3)
    testthat::expect_equal(0.032, modelFitTable[['r2mf']], tolerance = 1e-3)

    # Test model coefficients table
    coefTable <- r$models[[1]]$coef$asDF
    testthat::expect_equal(
        c(0.164, -0.189, NA, -0.112, -0.186, -0.567, NA, 0.447),
        coefTable[['est']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.14, 0.1, NA, 0.207, 0.154, 0.111, NA, 0.212),
        coefTable[['se']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(1.172, -1.896, NA, -0.539, -1.203, -5.088, NA, 2.104),
        coefTable[['z']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.241, 0.058, NA, 0.59, 0.229, 0, NA, 0.035),
        coefTable[['p']], tolerance = 1e-3
    )
})

testthat::test_that('Model fit table contains sample size footnote', {
    df <- data.frame(
        y = sample(0:2, 16, replace = TRUE),
        x = rnorm(16)
    )

    r <- jmv::logRegMulti(
        df,
        dep="y",
        covs="x",
        blocks=list(list("x")),
        refLevels=list(list(var="y", ref="0"))
    )

    testthat::expect_match(r$modelFit$notes$n$note, "N=16")
})

params <- list(
    list(refLevels = list(list(var="dep", ref="x"), list(var="factor", ref="X")), info = "Non-existing reference levels"),
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

        # WHEN a multinomial logistic regression is fitted with reference level set to a non-existing level
        r <- jmv::logRegMulti(
            df,
            dep = "dep",
            factors = "factor",
            blocks = list(list("factor")),
            refLevels = param$refLevels
        )

        # THEN the reference level should default to the first level for the dep variable
        testthat::expect_match(r$models[[1]]$coef$asDF$dep[1], "b - a", info=param$info)
        # THEN the reference level should default to the first level for the factor
        testthat::expect_match(r$models[[1]]$coef$asDF$term[3], "B – A", info=param$info)
        # AND a warning is added informing the user that the user defined reference level does not
        #   exist and therefore was changed to the first level
        testthat::expect_match(r[[1]]$content, "reference level was not found", info=param$info)
    }
})


