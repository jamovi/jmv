
context('descriptives')

test_that('descriptives works', {

    w <- as.factor(rep(c("1", "2","3"), each=4))
    x <- as.factor(rep(c("a", "b","c"), 4))
    y <- c(4,4,3,4,8,0,9,8,8,6,0,3)
    z <- c(NA,NaN,3,-1,-2,1,1,-2,2,-2,-3,3)

    data <- data.frame(w=w, x=x, y=y, z=z)
    # desc <- jmv::descriptives(data, vars=c("y", "z"), splitBy = c("x","w"), freq=TRUE, median=TRUE, mode=TRUE, skew=TRUE, kurt=TRUE, quart=TRUE)

    # Test frequency table numerical values
    # expect_equal(4, desc$frequencies$get('x')$getCell(rowNo=1,"counts")$value)

    # expect_false(is.numeric(desc$descriptives$getCell(rowNo=1, "stat[mean]")$value))

    # expect_error(descriptives(data.frame(x=c(Inf,-Inf)),c("x")), "Argument 'vars' specifies column 'x' which contains (and must not) infinite values", fixed=TRUE)

})
