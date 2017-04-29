context('anovarm')

test_that('anovarm works', {

    # simulate data set
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
})
