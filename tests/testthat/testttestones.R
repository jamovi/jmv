
context('ttestOneS')

test_that('ttestOneS works', {

    x <- as.factor(c(NA,rep(c("a", "b"), 6)))
    y <- c(8,51,2,74,1,91,5,25,1,59,5,32,7) # breaks equality of variance
    z <- c(2,NA,NaN,3,-1,-2,1,1,-2,2,-2,-3,3)
    w <- c(0,4,19,5,9,15,1,4,19,10,13,7,5)

    data <- data.frame(x=x,y=y,z=z,w=w)
    ttest <- ttestOneS(data, c("y","z","w"), testValue=0.5, hypothesis="dt",
                       wilcoxon=TRUE, meanDiff=TRUE, effectSize=TRUE, ci=TRUE, desc=TRUE)

    # Main table
    expect_equal(-0.142815511749496, ttest$ttest$getCell(rowNo=2, "es[stud]")$value)
    expect_equal(0.00233653321941677, ttest$ttest$getCell(rowNo=3, "p[wilc]")$value)

    # Descriptives
    expect_equal(27.7692307692308, ttest$descriptives$getCell(rowNo=1, "mean")$value)
    expect_equal(1.75636346559423, ttest$descriptives$getCell(rowNo=3, "se")$value)

})

test_that('matched rank biserial correlation is correct', {

    df <- data.frame(
        before = c(20, 22, 19, 20, 22, 18, 24, 20, 25),
        after = c(38, 37, 33, 29, 14, 12, 20, 22, 25)
    )

    df$dif <- df$after - df$before

    res <- ttestOneS(df, vars(dif), wilcoxon=TRUE, students=FALSE, effectSize=TRUE)$ttest$asDF

    expect_equal(0.5, res[['es[wilc]']])

})
