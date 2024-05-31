testthat::context('ttestIS')

testthat::test_that('All options in the ttestIS work (sunny)', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)
    df <- data.frame(
        `dep 1` = rnorm(100),
        `dep 2` = rnorm(100),
        `group 1` = sample(letters[1:2], size = 100, replace = TRUE),
        check.names = FALSE
    )

    r <- jmv::ttestIS(
        df,
        vars = c("dep 1", "dep 2"),
        group = "group 1",
        bf = TRUE,
        welchs = TRUE,
        mann = TRUE,
        norm = TRUE,
        eqv = TRUE,
        meanDiff = TRUE,
        ci = TRUE,
        effectSize = TRUE,
        ciES = TRUE,
        desc = TRUE
    )

    # Test main t-test table
    ttestTable <- r$ttest$asDF
    testthat::expect_equal(c('dep 1', 'dep 2'), ttestTable[['var[stud]']])
    testthat::expect_equal(c(-1.207, 0.586), ttestTable[['stat[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(98, 98), ttestTable[['df[stud]']])
    testthat::expect_equal(c(0.23, 0.559), ttestTable[['p[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(-0.258, 0.119), ttestTable[['md[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.214, 0.203), ttestTable[['sed[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(-0.682, -0.284), ttestTable[['cil[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.166, 0.522), ttestTable[['ciu[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(-0.243, 0.118), ttestTable[['es[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(-0.638, -0.277), ttestTable[['ciles[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.153, 0.512), ttestTable[['ciues[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.404, 0.247), ttestTable[['stat[bf]']], tolerance = 1e-3)
    testthat::expect_equal(c(0, 0), ttestTable[['err[bf]']], tolerance = 1e-3)
    testthat::expect_equal(c(-1.217, 0.591), ttestTable[['stat[welc]']], tolerance = 1e-3)
    testthat::expect_equal(c(96.62, 96.547), ttestTable[['df[welc]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.226, 0.556), ttestTable[['p[welc]']], tolerance = 1e-3)
    testthat::expect_equal(c(-0.258, 0.119), ttestTable[['md[welc]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.212, 0.201), ttestTable[['sed[welc]']], tolerance = 1e-3)
    testthat::expect_equal(c(-0.678, -0.281), ttestTable[['cil[welc]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.163, 0.519), ttestTable[['ciu[welc]']], tolerance = 1e-3)
    testthat::expect_equal(c(-0.244, 0.118), ttestTable[['es[welc]']], tolerance = 1e-3)
    testthat::expect_equal(c(1022, 1222), ttestTable[['stat[mann]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.136, 0.917), ttestTable[['p[mann]']], tolerance = 1e-3)
    testthat::expect_equal(c(-0.328, 0.019), ttestTable[['md[mann]']], tolerance = 1e-3)
    testthat::expect_equal(c(-0.738, -0.37), ttestTable[['cil[mann]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.115, 0.422), ttestTable[['ciu[mann]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.174, -0.013), ttestTable[['es[mann]']], tolerance = 1e-3)

    # Test normality tests table
    normTable <- r$assum$norm$asDF
    testthat::expect_equal(c('dep 1', 'dep 2'), normTable[['name']])
    testthat::expect_equal(c(0.99, 0.973), normTable[['w']], tolerance = 1e-3)
    testthat::expect_equal(c(0.685, 0.039), normTable[['p']], tolerance = 1e-3)

    # Test Levene's tests table
    eqvTable <- r$assum$eqv$asDF
    testthat::expect_equal(c('dep 1', 'dep 2'), eqvTable[['name']])
    testthat::expect_equal(c(0.001, 0.004), eqvTable[['f']], tolerance = 1e-3)
    testthat::expect_equal(c(1, 1), eqvTable[['df']])
    testthat::expect_equal(c(98, 98), eqvTable[['df2']])
    testthat::expect_equal(c(0.97, 0.951), eqvTable[['p']], tolerance = 1e-3)

    # Test descriptives table
    descTable <- r$desc$asDF
    testthat::expect_equal(c('dep 1', 'dep 2'), descTable[['dep']])
    testthat::expect_equal(c('a', 'a'), descTable[['group[1]']])
    testthat::expect_equal(c(55, 55), descTable[['num[1]']])
    testthat::expect_equal(c(0.121, 0.095), descTable[['mean[1]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.051, -0.134), descTable[['med[1]']], tolerance = 1e-3)
    testthat::expect_equal(c(1.102, 1.045), descTable[['sd[1]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.149, 0.141), descTable[['se[1]']], tolerance = 1e-3)
    testthat::expect_equal(c('b', 'b'), descTable[['group[2]']])
    testthat::expect_equal(c(45, 45), descTable[['num[2]']])
    testthat::expect_equal(c(0.379, -0.024), descTable[['mean[2]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.45, -0.054), descTable[['med[2]']], tolerance = 1e-3)
    testthat::expect_equal(c(1.013, 0.965), descTable[['sd[2]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.151, 0.144), descTable[['se[2]']], tolerance = 1e-3)
})

testthat::test_that('Error is thrown if grouping var has more than 2 levels', {
    df <- data.frame(
        dep = c(1, 7, 4),
        group = c("a", "b", "c"),
        stringsAsFactors = TRUE
    )

    testthat::expect_error(
        jmv::ttestIS(df, vars = "dep", group = "group"),
        "Grouping variable 'group' must have exactly 2 levels",
        fixed=TRUE
    )
})

testthat::test_that('Error is thrown if grouping var has more than 2 levels', {
    df <- data.frame(
        dep = c(1, 7, 4),
        group = c("a", "b", "c"),
        stringsAsFactors = TRUE
    )

    testthat::expect_error(
        jmv::ttestIS(df, vars = "dep", group = "group"),
        "Grouping variable 'group' must have exactly 2 levels",
        fixed=TRUE
    )
})

testthat::test_that('Matched rank biserial correlation is correct', {
    df <- data.frame(
        score = c(220, 221, 223, 225, 226, 228, 229, 230, 232),
        group = c('spr', 'spr', 'spr', 'ctrl', 'spr', 'spr', 'ctrl', 'ctrl', 'ctrl'),
        stringsAsFactors = TRUE
    )

    r <- jmv::ttestIS(df, vars="score", group="group", mann=TRUE, students=FALSE, effectSize=TRUE)

    # Test rank biserial correlation
    ttestTable <- r$ttest$asDF
    testthat::expect_equal(2, ttestTable[['stat[mann]']])
    testthat::expect_equal(0.063, ttestTable[['p[mann]']], tolerance = 1e-3)
    testthat::expect_equal(-0.8, ttestTable[['es[mann]']])
})

testthat::test_that('Rank biserial correlation can be negative', {
    df <- data.frame(
        score = c(3, 4, 7, 8, 5, 9),
        group = c('spr', 'spr', 'spr', 'ctrl', 'ctrl', 'ctrl'),
        stringsAsFactors = TRUE
    )

    r <- jmv::ttestIS(df, vars="score", group="group", mann=TRUE, students=FALSE, effectSize=TRUE)

    # Test rank biserial correlation
    ttestTable <- r$ttest$asDF
    testthat::expect_equal(1, ttestTable[['stat[mann]']])
    testthat::expect_equal(0.2, ttestTable[['p[mann]']])
    testthat::expect_equal(-0.778, ttestTable[['es[mann]']], tolerance = 1e-3)
})
