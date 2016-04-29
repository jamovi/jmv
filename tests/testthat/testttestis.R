x <- as.factor(c(NA,rep(c("a", "b"), 6)))
y <- c(8,51,2,74,1,91,5,25,1,59,5,32,7) # breaks equality of variance
z <- c(2,NA,NaN,3,-1,-2,1,1,-2,2,-2,-3,3)

data <- data.frame(x=x,y=y,z=z)
ttest <- TTestIS(data, c("y","z"), "x", hypothesis="different", welch=TRUE, mann=TRUE, norm=TRUE, equality=TRUE, meanDiff=TRUE, effectSize=TRUE, ci=TRUE, desc=TRUE)
#print(ttest)

# Add test for ttest table when row folding completed
# expect_output(ttest$results$get("ttest"), "output", fixed=TRUE)

expect_identical(ttest$results$get("normality")$asString(),
    paste(sep="\n",
        "",
        " Test of Normality (Shapiro-Wilk)    ",
        " ─────────────────────────────────── ",
        "           Group    W        p       ",
        " ─────────────────────────────────── ",
        "   y       a        0.968    0.879   ",
        "   y       b        0.873    0.240   ",
        "   z       a        0.915    0.501   ",
        "   z       b        0.871    0.272   ",
        " ─────────────────────────────────── ",
        "", ""))

expect_output(print(ttest$results$get("equalityOfV")),
    paste(sep="\n",
        "",
        " Test of Equality of Variances (Levene's) ",
        " ──────────────────────────────────────── ",
        "           F         df      p         ",
        " ──────────────────────────────────────── ",
        "   y       10.069    1.00    0.00993   ",
        "   z        0.472    1.00    0.51162   ",
        " ──────────────────────────────────────── ",
        ""), fixed=TRUE)

expect_output(print(ttest$results$get("descriptives")),
    paste(sep="\n",
        "",
        " Group Descriptives                                     ",
        " ────────────────────────────────────────────────────── ",
        "           Group    N       Mean      SD       SE       ",
        " ────────────────────────────────────────────────────── ",
        "   y       a        6.00    55.333    24.97    10.194   ",
        "   y       b        6.00     3.500     2.51     1.025   ",
        "   z       a        5.00     0.200     2.59     1.158   ",
        "   z       b        5.00    -0.200     2.17     0.970   ",
        " ────────────────────────────────────────────────────── ",
        ""), fixed=TRUE)

expect_output(print(TTestIS(data, c("y","z"), "x", hypothesis="different", miss="listwise", desc=TRUE)$results$get("descriptives")),
    paste(sep="\n",
        " Group Descriptives                                     ",
        " ────────────────────────────────────────────────────── ",
        "           Group    N       Mean      SD       SE       ",
        " ────────────────────────────────────────────────────── ",
        "   y       a        5.00    56.200    27.82    12.439   ",
        "   y       b        5.00     3.800     2.68     1.200   ",
        "   z       a        5.00     0.200     2.59     1.158   ",
        "   z       b        5.00    -0.200     2.17     0.970   ",
        " ────────────────────────────────────────────────────── ",
        ""), fixed=TRUE)

expect_error(TTestIS(data, c("x","y"), "x"), "Grouping variable 'x' must not also be a dependent variable", fixed=TRUE)
expect_error(TTestIS(data, c("x","y"), c("x","y")), "There must only be one grouping variable", fixed=TRUE)
expect_error(TTestIS(data.frame(badGroupingVar=as.factor(c("a", "b", "c")),y=c(1,7,4)), "y", "badGroupingVar"), "Grouping variable 'badGroupingVar' must have exactly 2 levels", fixed=TRUE)
expect_error(TTestIS(data, "y", "x", hypothesis="error"), "Argument 'hypothesis' must be one of 'different', 'oneGreater', 'twoGreater'", fixed=TRUE)

expect_error(TTestIS(data.frame(x=c(rep("a",6),rep("b",6)),y=c(rep(NA,6),runif(6))), "y", "x"), "Grouping variable 'x' has less than 2 levels after missing values of dependent variable 'y' are excluded", fixed=TRUE)
expect_error(TTestIS(data.frame(x=c(rep("a",6),rep("b",6)),y=c(rep(NA,6),runif(6))), "y", "x", miss = "listwise"), "Grouping variable 'x' has less than 2 levels after missing values are excluded", fixed=TRUE)
expect_error(TTestIS(data.frame(x=as.factor(rep(c("a","b"),5)),y=c(Inf,runif(9))),"y","x"), "Argument 'vars' specifies column 'y' which contains (and must not) infinite values", fixed=TRUE)

ttest <- TTestIS(data.frame(x=as.factor(c(rep("a",5001),"b","b")),y=runif(5003)),"y","x",norm=TRUE)
expect_output(print(ttest$results$get("normality")),"\n Test of Normality (Shapiro-Wilk)   \n ────────────────────────────────── \n           Group     W       p      \n ────────────────────────────────── \n   y       a    ᵃ                   \n   y       b    ᵇ                   \n ────────────────────────────────── \n ᵃ Too many observations (N > \n   5000) to compute statistic \n ᵇ Too few observations (N < 3) \n   to compute statistic \n",fixed=TRUE)


# need to figure out how to catch problems with low variance before computation
ttest <- TTestIS(data.frame(x=as.factor(rep(c("a","b"),5)),y=c(rep(1,8),1.00000001,1.00000001)),"y","x")
#print(ttest)
