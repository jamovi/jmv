x <- as.factor(c(NA,rep(c("a", "b"), 6)))
y <- c(8,51,2,74,1,91,5,25,1,59,5,32,7) # breaks equality of variance
z <- c(2,NA,NaN,3,-1,-2,1,1,-2,2,-2,-3,3)
w <- c(0,4,19,5,9,15,1,4,19,10,13,7,5)

data <- data.frame(x=x,y=y,z=z,w=w)
ttest <- TTestPS(data, c("y","z","y","w"), hypothesis="different", wilcoxon=TRUE, meanDiff=TRUE, effectSize=TRUE, ci=TRUE, desc=TRUE)
#print(ttest)

# Add test for ttest table when row folding completed
# expect_output(ttest$results$get("ttest"), "output", fixed=TRUE)
expect_output(ttest$results$get("normality"), "\n Test of Normality (Shapiro-Wilk) \n ──────────────────────────────── \n            W        p        \n ──────────────────────────────── \n   y - z    0.803    0.0102   \n   y - w    0.911    0.1890   \n ──────────────────────────────── \n", fixed=TRUE)
expect_output(ttest$results$get("descriptives"), "\n Descriptives                                 \n ──────────────────────────────────────────── \n           N       Mean      SD       SE      \n ──────────────────────────────────────────── \n   y       13.0    27.769    31.13    8.635   \n   z       11.0     0.182     2.23    0.672   \n   w       13.0     8.538     6.33    1.756   \n ──────────────────────────────────────────── \n", fixed=TRUE)

expect_error(TTestPS(data, c("x","y"), hypothesis="error"), "Error : Argument 'hypothesis' must be one of 'different', 'oneGreater', 'twoGreater'\n", fixed=TRUE)


ttest <- TTestPS(data.frame(x=runif(5001),y=runif(5001)),c("x","y"),norm=TRUE)
expect_output(ttest$results$get("normality"), "\n Test of Normality (Shapiro-Wilk) \n ──────────────────────────────── \n              W       p      \n ──────────────────────────────── \n   x - y ᵃ                   \n ──────────────────────────────── \n ᵃ Too many observations (N > \n   5000) to compute statistic \n", fixed=TRUE)
ttest <- TTestPS(data.frame(x=runif(2),y=runif(2)),c("x","y"),norm=TRUE)
expect_output(ttest$results$get("normality"), "\n Test of Normality (Shapiro-Wilk) \n ──────────────────────────────── \n              W       p      \n ──────────────────────────────── \n   x - y ᵃ                   \n ──────────────────────────────── \n ᵃ Too few observations (N < \n   3) to compute statistic \n", fixed=TRUE)

expect_error(TTestPS(data, c("y","w","z")), "Error : At least one variable not paired\n", fixed=TRUE)
expect_error(TTestPS(data.frame(x=rep(1,10),y=c(rep(1,9),1.000000000000000001)),c("y","x"), norm=TRUE), "Error : 'y - x' is essentially constant\n", fixed=TRUE)

expect_error(TTestPS(data.frame(x=rep(NA,10),y=c(runif(10))), c("y", "x")), "Error : At least one variable only contains missing values\n", fixed=TRUE)
expect_error(TTestPS(data.frame(x=runif(10),y=c(Inf,runif(9))), c("y","x")), "Error : Argument 'vars' specifies column 'y' which contains (and must not) infinite values\n", fixed=TRUE)

