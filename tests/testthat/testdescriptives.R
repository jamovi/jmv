
x <- as.factor(rep(c("a", "b","c"), 4))
y <- c(4,4,3,4,8,0,9,8,8,6,0,3)
z <- c(NA,NaN,3,-1,-2,1,1,-2,2,-2,-3,3)

data <- data.frame(x=x, y=y, z=z)
desc <- Descriptives(data, c("x", "y", "z"), freq=TRUE, median=TRUE, mode=TRUE, skew=TRUE, kurt=TRUE, quart=TRUE)

expect_output(desc$results$get("descriptives"),
    paste(sep="\n",
        "",
        " Descriptives                                     ",
        " ──────────────────────────────────────────────── ",
        "                      x       y           z       ",
        " ──────────────────────────────────────────────── ",
        "   Mean                         4.75       0.00   ",
        "   Median                       4.00       0.00   ",
        "   Mode                         4.00 ᵃ    -2.00   ",
        "   Minimum                      0.00      -3.00   ",
        "   Maximum                      9.00       3.00   ",
        "   Skewness                   -0.127      0.104   ",
        "   Kurtosis                     1.58       1.19   ",
        "   25th percentile              3.00      -2.00   ",
        "   50th percentile              4.00       0.00   ",
        "   75th percentile              8.00       1.75   ",
        " ──────────────────────────────────────────────── ",
        " ᵃ More than one mode exists, only the first ",
        "   is reported ",
        ""), fixed=TRUE)

expect_output(desc$results$get("frequencies"),
    paste(sep="\n",
        " Frequencies",
        "",
        " Frequencies of x                            ",
        " ─────────────────────────────────────────── ",
        "   Level    Counts    %       Cumulative %   ",
        " ─────────────────────────────────────────── ",
        "   a          4.00    33.3            33.3   ",
        "   b          4.00    33.3            66.7   ",
        "   c          4.00    33.3           100.0   ",
        " ─────────────────────────────────────────── "), fixed=TRUE)
    
# Test footnote appearance
expect_true(!length(desc$results$get("descriptives")$getCell(1, "mode")$sups))
expect_false(!length(desc$results$get("descriptives")$getCell(2, "mode")$sups))
expect_true(!length(desc$results$get("descriptives")$getCell(3, "mode")$sups))

# Test frequency table numerical values
expect_equal(4, desc$results$get("frequencies")$get("x")$getCell(1,"counts")$value)
expect_equal(100/3, desc$results$get("frequencies")$get("x")$getCell(1,"percentage")$value)
expect_equal(200/3, desc$results$get("frequencies")$get("x")$getCell(2,"cumpercentage")$value)

expect_false(is.numeric(desc$results$get("descriptives")$getCell(1, "mean")$value))

expect_error(Descriptives(data.frame(x=c(Inf,-Inf)),c("x")), "Error : Argument 'vars' specifies column 'x' which contains (and must not) infinite values\n", fixed=TRUE)


