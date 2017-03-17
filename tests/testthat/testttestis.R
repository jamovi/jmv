
context('ttestIS')

test_that('ttestIS works', {
    x <- as.factor(c(NA,rep(c("a", "b"), 6)))
    y <- c(8,51,2,74,1,91,5,25,1,59,5,32,7) # breaks equality of variance
    z <- c(2,NA,NaN,3,-1,-2,1,1,-2,2,-2,-3,3)

    data <- data.frame(x=x,y=y,z=z)
    ttest <- ttestIS(data, c("y","z"), "x", hypothesis="different", bf=TRUE, welch=TRUE, mann=TRUE, norm=TRUE, eqv=TRUE, meanDiff=TRUE, effectSize=TRUE, ci=TRUE, desc=TRUE)

    expect_equal(c("a"=0.167541563316678), ttest$ttest$getCell(rowNo=2, "es[stud]")$value)

    expect_error(ttestIS(data.frame(badGroupingVar=as.factor(c("a", "b", "c")),y=c(1,7,4)), "y", "badGroupingVar"), "Grouping variable 'badGroupingVar' must have exactly 2 levels", fixed=TRUE)
    expect_error(ttestIS(data, "y", "x", hypothesis="error"), "Argument 'hypothesis' must be one of 'different', 'oneGreater', 'twoGreater'", fixed=TRUE)
})
