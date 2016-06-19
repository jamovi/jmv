
TTestOneSClass <- R6Class("TTestOneSClass",
    inherit=silkycore::Analysis,
    private=list(
        .run=function() {
            
            dataset <- self$options$dataset()
            naHandling <- self$options$get("miss")
            
            ttest <- self$results$get("ttest")
            desc <- self$results$get("descriptives")
            normality <- self$results$get("normality")
            
            variables <- self$options$get("vars")
            
            testValue <- self$options$get("testValue")
            cl <- self$options$get("ciWidth")/100
            
            ## Listwise NA cleanup
            if (naHandling == "listwise")
                dataset <- na.omit(dataset)
            
            ## Hypothesis options checking
            if (self$options$get("hypothesis") == "oneGreater") {
                Ha <- "greater"
                # Footnote message TBC
            } else if (self$options$get("hypothesis") == "twoGreater") {
                Ha <- "less"
                # Footnote message TBC
            } else {
                Ha <- "two.sided"
            }
            
            for (i in seq_along(variables)) {
                
                name <- variables[[i]]
                column <- silkycore::toNumeric(dataset[[name]])
                column <- na.omit(column)
                
                n <- length(column)
                
                m <- mean(column)
                stdDev <- sd(column)
                se <- stdDev/sqrt(n)
                d <- (m-testValue)/stdDev #Cohen's d
                
                if (self$options$get("students")) {
                    
                    res <- t.test(column, mu=testValue, paired=FALSE, conf.level=cl, alternative=Ha)
                    ttest$setRow(rowNo=i, list(
                        "stat[stud]"=res$statistic,
                        "df[stud]"=res$parameter,
                        "p[stud]"=res$p.value,
                        "md[stud]"=res$estimate - testValue,
                        "es[stud]"=d,
                        "lci[stud]"=res$conf.int[1],
                        "uci[stud]"=res$conf.int[2]))
                }
                
                if (self$options$get("mann")) {
                    
                    res <- suppressWarnings(wilcox.test(column, mu=testValue, alternative=Ha, paired=FALSE, conf.int=TRUE, conf.level=cl))
                    ttest$setRow(rowNo=i, list(
                        "stat[mann]"=res$statistic,
                        "p[mann]"=res$p.value,
                        "md[mann]"=res$estimate - testValue,
                        "es[mann]"=d,
                        "lci[mann]"=res$conf.int[1],
                        "uci[mann]"=res$conf.int[2]))
                }
                
                # Normality test table
                if (n < 3) {

                    normality$addFootnote(rowNo=i, "name", "Too few observations (N < 3) to compute statistic")
                    res$statistic <- NaN
                    res$p.value <- ""

                } else if (n > 5000) {

                    normality$addFootnote(rowNo=i, "name", "Too many observations (N > 5000) to compute statistic")
                    res$statistic <- NaN
                    res$p.value <- ""

                }
                else {

                    res <- shapiro.test(column - testValue)
                }
                
                normality$setRow(rowNo=i, list(
                    w=res$statistic,
                    p=res$p.value))
                
                ## Descriptives table
                desc$setRow(rowNo=i, list(num=n, mean=m, sd=stdDev, se=se))
            }
        }
    )
)

