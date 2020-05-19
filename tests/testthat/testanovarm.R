context('anovarm')

test_that('anovarm works', {

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
    expect_equal(2.26500538322502, r$rmTable$getCell(rowNo=2, "ss[none]")$value)
    expect_equal(4, r$rmTable$getCell(rowNo=2, "df[none]")$value)
    expect_equal(0.566251345806256, r$rmTable$getCell(rowNo=2, "ms[none]")$value)
    expect_equal(0.598380091504137, r$rmTable$getCell(rowNo=2, "F[none]")$value)
    expect_equal(0.664545185431145, r$rmTable$getCell(rowNo=2, "p[none]")$value)
    expect_equal('', r$rmTable$getCell(rowNo=3, "F[none]")$value)

    # Test rm table
    expect_equal(0.415787240953556, r$bsTable$getCell(rowNo=1, "ss")$value)
    expect_equal(0.842271353755428, r$bsTable$getCell(rowNo=1, "p")$value)


    # Test sphericity footnote when there's a singularity error
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
