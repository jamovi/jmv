context('descriptives')

test_that('descriptive statistics work for continuous variables without split by sunny', {
    CI_WIDTH <- 0.95
    QUANTS <- c(0.25, 0.50, 0.75)

    set.seed(1337)
    x <- rnorm(100, 0, 1)
    df <- data.frame(x=x)

    desc <- jmv::descriptives(
        data=df, vars=x, mode=TRUE, sum=TRUE, variance=TRUE, range=TRUE,
        se=TRUE, ci=TRUE, skew=TRUE, kurt=TRUE, sw=TRUE, pcEqGr=TRUE, pc=TRUE
    )

    r <- desc$descriptives$asDF

    # Calculate statistics
    missing <- sum(is.na(x))
    n <- length(x) - missing
    mean <- mean(x)
    se <- sd(x) / sqrt(n)
    zQuant <- 1 - ((1 - CI_WIDTH) / 2)
    ciLower <- mean - qnorm(zQuant) * se
    ciUpper <- mean + qnorm(zQuant) * se
    mode <- as.numeric(names(table(x)[table(x)==max(table(x))]))[1]
    shapiro <- shapiro.test(x)
    quantiles <- quantile(x, QUANTS)

    # Test descriptives table
    expect_equal(n, r[["x[n]"]], tolerance = 1e-5)
    expect_equal(missing, r[["x[missing]"]], tolerance = 1e-5)
    expect_equal(mean, r[["x[mean]"]], tolerance = 1e-5)
    expect_equal(se, r[["x[se]"]], tolerance = 1e-5)
    expect_equal(ciLower, r[["x[ciLower]"]], tolerance = 1e-5)
    expect_equal(ciUpper, r[["x[ciUpper]"]], tolerance = 1e-5)
    expect_equal(median(x), r[["x[median]"]], tolerance = 1e-5)
    expect_equal(mode, r[["x[mode]"]], tolerance = 1e-5)
    expect_equal(sum(x), r[["x[sum]"]], tolerance = 1e-5)
    expect_equal(sd(x), r[["x[sd]"]], tolerance = 1e-5)
    expect_equal(var(x), r[["x[variance]"]], tolerance = 1e-5)
    expect_equal(range(x)[2] - range(x)[1], r[["x[range]"]], tolerance = 1e-5)
    expect_equal(min(x), r[["x[min]"]], tolerance = 1e-5)
    expect_equal(max(x), r[["x[max]"]], tolerance = 1e-5)
    expect_equal(0.11014, r[["x[skew]"]], tolerance = 1e-5)
    expect_equal(0.24138, r[["x[seSkew]"]], tolerance = 1e-5)
    expect_equal(-0.11958, r[["x[kurt]"]], tolerance = 1e-5)
    expect_equal(0.47833, r[["x[seKurt]"]], tolerance = 1e-5)
    expect_equal(as.numeric(shapiro$statistic), r[["x[sww]"]], tolerance = 1e-5)
    expect_equal(as.numeric(shapiro$p.value), r[["x[sw]"]], tolerance = 1e-5)
    expect_equal(as.numeric(quantiles[1]), r[["x[quant1]"]], tolerance = 1e-5)
    expect_equal(as.numeric(quantiles[2]), r[["x[quant2]"]], tolerance = 1e-5)
    expect_equal(as.numeric(quantiles[3]), r[["x[quant3]"]], tolerance = 1e-5)
})


test_that('descriptives works old scenario', {

    w <- as.factor(rep(c("1", "2","3"), each=4))
    x <- as.factor(rep(c("a", "b","c"), 4))
    y <- c(4,4,3,4,8,0,9,8,8,6,0,3)
    z <- c(NA,NaN,3,-1,-2,1,1,-2,2,-2,-3,3)

    data <- data.frame(w=w, x=x, y=y, z=z)
    desc <- jmv::descriptives(data, vars=c("w", "y", "z"), splitBy = "x",
                              freq=TRUE, median=TRUE, mode=TRUE, skew=TRUE,
                              kurt=TRUE, pc=TRUE)

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
