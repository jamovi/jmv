
context('descriptives')

test_that('descriptives works', {

    w <- as.factor(rep(c("1", "2","3"), each=4))
    x <- as.factor(rep(c("a", "b","c"), 4))
    y <- c(4,4,3,4,8,0,9,8,8,6,0,3)
    z <- c(NA,NaN,3,-1,-2,1,1,-2,2,-2,-3,3)

    data <- data.frame(w=w, x=x, y=y, z=z)
    desc <- jmv::descriptives(data, vars=c("w", "y", "z"), splitBy = "x", freq=TRUE, median=TRUE, mode=TRUE, skew=TRUE, kurt=TRUE, pc=TRUE)

    freq <- desc$frequencies[[1]]$asDF
    descr <- desc$descriptives$asDF

    # Test frequency table numerical values
    expect_equal(1, freq[1,3], tolerance = 1e-3)
    expect_equal(2, freq[3,4], tolerance = 1e-3)

    # Test descriptives table numerical values
    expect_equal(2.619, descr$`y[seKurtb]`, tolerance = 1e-3)
    expect_equal(-1.289, descr$`z[kurtc]`, tolerance = 1e-3)
    expect_equal(1, descr$`z[missinga]`, tolerance = 1e-3)
    expect_equal(5.750, descr$`y[meana]`, tolerance = 1e-3)
    expect_equal(-2, descr$`z[modeb]`, tolerance = 1e-3)
    expect_equal(4, descr$`y[mina]`, tolerance = 1e-3)
    expect_equal(2.25, descr$`y[perc1c]`, tolerance = 1e-3)

})

test_that('histogram is created for nominal numeric variable', {

    set.seed(1337)
    data <- data.frame(
        a1 = rnorm(100, 0, 10),
        a2 = factor(sample(1:10, 100, replace = TRUE))
    )

    attr(data$a2, 'values') <- 1:10

    desc <- jmv::descriptives(data, c('a1', 'a2'), hist=TRUE)

    expect_true(desc$plots[[2]]$.render())
})

test_that('warning message is shown when numeric plot is created for a non-numeric variable', {

    set.seed(1337)
    data <- data.frame(
        a1 = rnorm(100, 0, 10),
        a3 = factor(sample(letters[1:4], 100, replace = TRUE))
    )

    desc <- jmv::descriptives(data, c('a1', 'a3'), hist=TRUE)

    desc$plots[[3]]$.render()

    expect_match(desc$plots[[1]]$asString(), "class=\"warning\"")
})
