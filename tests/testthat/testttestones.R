x <- as.factor(c(NA,rep(c("a", "b"), 6)))
y <- c(8,51,2,74,1,91,5,25,1,59,5,32,7) # breaks equality of variance
z <- c(2,NA,NaN,3,-1,-2,1,1,-2,2,-2,-3,3)
w <- c(0,4,19,5,9,15,1,4,19,10,13,7,5)

data <- data.frame(x=x,y=y,z=z,w=w)
ttest <- TTestOneS(data, c("y","z","w"), testValue=0.5, hypothesis="different", mann=TRUE, meanDiff=TRUE, effectSize=TRUE, ci=TRUE, desc=TRUE)
#print(ttest)

# Add test for ttest table when row folding completed
# expect_output(ttest$results$get("ttest"), "output", fixed=TRUE)
expect_output(print(ttest$results$get("normality")), "\n Test of Normality (Shapiro-Wilk) \n ──────────────────────────────── \n           W        p        \n ──────────────────────────────── \n   y       0.826    0.0142   \n   z       0.887    0.1266   \n   w       0.927    0.3121   \n ──────────────────────────────── \n", fixed=TRUE)
expect_output(print(ttest$results$get("descriptives")), "\n Descriptives                                 \n ──────────────────────────────────────────── \n           N       Mean      SD       SE      \n ──────────────────────────────────────────── \n   y       13.0    27.769    31.13    8.635   \n   z       11.0     0.182     2.23    0.672   \n   w       13.0     8.538     6.33    1.756   \n ──────────────────────────────────────────── \n", fixed=TRUE)

expect_error(TTestOneS(data, c("x","y"), hypothesis="error"), "Argument 'hypothesis' must be one of 'different', 'oneGreater', 'twoGreater'", fixed=TRUE)


ttest <- TTestOneS(data.frame(x=runif(5001),y=runif(5001)),c("x","y"),norm=TRUE)
expect_output(print(ttest$results$get("normality")), "\n Test of Normality (Shapiro-Wilk) \n ──────────────────────────────── \n             W       p      \n ──────────────────────────────── \n   x    ᵃ                   \n   y    ᵃ                   \n ──────────────────────────────── \n ᵃ Too many observations (N > \n   5000) to compute statistic \n", fixed=TRUE)
ttest <- TTestOneS(data.frame(x=runif(2),y=runif(2)),c("x","y"),norm=TRUE)
expect_output(print(ttest$results$get("normality")), "\n Test of Normality (Shapiro-Wilk) \n ──────────────────────────────── \n             W       p      \n ──────────────────────────────── \n   x    ᵃ                   \n   y    ᵃ                   \n ──────────────────────────────── \n ᵃ Too few observations (N < \n   3) to compute statistic \n", fixed=TRUE)

expect_error(TTestOneS(data.frame(x=rep(1,10),y=c(rep(1,9),1.000000000000000001)),c("y","x"), norm=TRUE), "Variable 'y' has essentially constant values", fixed=TRUE)

expect_error(TTestOneS(data.frame(x=rep(NA,10),y=c(runif(10))), c("y", "x")), "Variable 'x' only contains missing values", fixed=TRUE)
expect_error(TTestOneS(data.frame(x=runif(10),y=c(Inf,runif(9))), c("y","x")), "Argument 'vars' specifies column 'y' which contains (and must not) infinite values", fixed=TRUE)

