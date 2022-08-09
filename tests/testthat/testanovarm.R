context('anovarm')

testthat::test_that('anovarm works', {
    # simulate data set
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(210)
    data <- list(between = factor(sample(c("A", "B", "C"), 60, replace = TRUE)),
                 'm o n' = rnorm(60, .5),
                 tue = rnorm(60, .6),
                 fri = rnorm(60, .7))

    attr(data, 'row.names') <- seq_len(length(data[[1]]))
    attr(data, 'class') <- 'data.frame'

    rm <- list(
            list(
                label="intake",
                levels=c("m o n", "tue", "fri")
            ))

    rmCells <- list(
                list(
                    measure="m o n",
                    cell="m o n"),
                list(
                    measure="tue",
                    cell="tue"),
                list(
                    measure="fri",
                    cell="fri"))

    postHoc <- list(
        "between",
        c("intake", "between")
    )

    r <- jmv::anovaRM(data=data, rm=rm, rmCells=rmCells, bs="between",
                      rmTerms=list("intake"), bsTerms=list("between"),
                      postHoc = postHoc)

    # Test rm table
    testthat::expect_equal(2.26500538322502, r$rmTable$getCell(rowNo=2, "ss[none]")$value)
    testthat::expect_equal(4, r$rmTable$getCell(rowNo=2, "df[none]")$value)
    testthat::expect_equal(0.566251345806256, r$rmTable$getCell(rowNo=2, "ms[none]")$value)
    testthat::expect_equal(0.598380091504137, r$rmTable$getCell(rowNo=2, "F[none]")$value)
    testthat::expect_equal(0.664545185431145, r$rmTable$getCell(rowNo=2, "p[none]")$value)
    testthat::expect_equal('', r$rmTable$getCell(rowNo=3, "F[none]")$value)

    # Test rm table
    testthat::expect_equal(0.415787240953556, r$bsTable$getCell(rowNo=1, "ss")$value)
    testthat::expect_equal(0.842271353755428, r$bsTable$getCell(rowNo=1, "p")$value)
})

testthat::test_that("Test sphericity footnote when there's a singularity error", {
    data <- data.frame(
        'id' = 1:15,
        'x1' = c(4, 13, 15, 12, 12, 2, 19, 10, 22, 13, 10, 22, 10, 14, 22),
        'x2' = c(55, 40, 26, 6, 20, 37, 37, 12, 45, 29, 28, 4, 26, 39, 30),
        'x3' = c(51, 36, 22, 2, 16, 33, 33, 8, 41, 25, 24, 0, 22, 35, 26)
    )

    r <- jmv::anovaRM(
        data = data,
        rm = list(
            list(
                label="var",
                levels=c("x1", "x2", "x3"))),
        rmCells = list(
            list(
                measure="x1",
                cell="x1"),
            list(
                measure="x2",
                cell="x2"),
            list(
                measure="x3",
                cell="x3")),
        rmTerms = ~ var,
        spherTests = TRUE)

    spher <- r$assump$spherTable$asDF
    testthat::expect_equal(spher$mauch, NaN)
})

testthat::test_that('emmeans work for unbalanced data', {
    set.seed(1337)
    N <- 100
    data <- data.frame(
        measure1 = rnorm(N, 0, 1),
        measure2 = rnorm(N, 1, 1),
        measure3 = rnorm(N, 2, 1),
        bsFactor = sample(letters[1:2], replace=TRUE, prob=c(0.3, 0.7), size=N),
        stringsAsFactors = TRUE
    )

    rm = list(list(
        label="rmFactor",
        levels=c("measure1", "measure2", "measure3")
    ))

    rmCells = list(
        list(
            measure="measure1",
            cell="measure1"),
        list(
            measure="measure2",
            cell="measure2"),
        list(
            measure="measure3",
            cell="measure3")
    )

    r <- jmv::anovaRM(
        data=data, rm=rm, rmCells=rmCells, bs="bsFactor",
        rmTerms=list("rmFactor"), bsTerms=list("bsFactor"),
        emMeans = ~bsFactor:rmFactor, emmPlots = FALSE, emmTables = TRUE
    )

    means <- aggregate(data[, -4], data[4], mean)
    emmeans <- r$emm[[1]]$emmTable$asDF

    testthat::expect_equal(means[1, 2], emmeans[1, "mean"], tolerance = 1e-4)
    testthat::expect_equal(means[2, 2], emmeans[2, "mean"], tolerance = 1e-4)
    testthat::expect_equal(means[1, 3], emmeans[3, "mean"], tolerance = 1e-4)
    testthat::expect_equal(means[2, 3], emmeans[4, "mean"], tolerance = 1e-4)
    testthat::expect_equal(means[1, 4], emmeans[5, "mean"], tolerance = 1e-4)
    testthat::expect_equal(means[2, 4], emmeans[6, "mean"], tolerance = 1e-4)
})

testthat::test_that('Provide error message when there are empty cells in bs design', {
    df <- data.frame(
        measure1 = 20:24,
        measure2 = 24:20,
        bsFactor1 = c("A", "A", "B", "A", "A"),
        bsFactor2 = c("A", "A", "B", "A", "A"),
        stringsAsFactors = TRUE
    )

    rm = list(list(
        label="rmFactor",
        levels=c("measure1", "measure2")
    ))

    rmCells = list(
        list(measure="measure1", cell="measure1"),
        list(measure="measure2", cell="measure2")
    )

    testthat::expect_error(
        jmv::anovaRM(
            data=df,
            rm=rm,
            rmCells=rmCells,
            bs=c("bsFactor1", "bsFactor2"),
            rmTerms=list("rmFactor"),
            bsTerms=list("bsFactor1", "bsFactor2")
        ),
        "Empty cells in between subject design"
    )
})


