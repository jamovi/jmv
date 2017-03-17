
context('ttestPS')

test_that('ttestPS works', {

    x <- as.factor(c(NA,rep(c("a", "b"), 6)))
    y <- c(8,51,2,74,1,91,5,25,1,59,5,32,7) # breaks equality of variance
    z <- c(2,NA,NaN,3,-1,-2,1,1,-2,2,-2,-3,3)
    w <- c(0,4,19,5,9,15,1,4,19,10,13,7,5)

    data <- data.frame(x=x,y=y,z=z,w=w)
    pairs <- list(
                list(i1='z', i2='w'))

    ttest <- ttestPS(data, pairs, hypothesis="different", bf=TRUE, wilcoxon=TRUE, meanDiff=TRUE, effectSize=TRUE, ci=TRUE, desc=TRUE)

    # Main table
    expect_equal(-1.03769723962266, ttest$ttest$getCell(rowNo=1, "es[stud]")$value)
    expect_equal(8.78867448064374, ttest$ttest$getCell(rowNo=1, "stat[bf]")$value)

    # Descriptives table
    expect_equal(1, ttest$desc$getCell(rowNo=1, "med")$value)
    expect_equal(5.89915248150105, ttest$desc$getCell(rowNo=2, "sd")$value)
})
