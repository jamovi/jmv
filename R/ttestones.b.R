
TTestOneSClass <- R6Class("TTestOneSClass",
    inherit=silkycore::Analysis,
    private=list(
        .run=function() {
            
            dataset <- self$options$dataset()
            naHandling <- self$options$values()$miss
            
            ttest <- self$results$get("ttest")
            desc <- self$results$get("descriptives")
            normality <- self$results$get("normality")
            
            variables <- self$options$values()$vars
            
            wantsStudents <- self$options$values()$student
            wantsMannWhitney <- self$options$values()$mann
            
            testValue <- self$options$values()$testValue
            cl <- self$options$values()$ciWidth/100
            
            ## Listwise NA cleanup
            if (naHandling == "listwise")
                dataset.clean <- dataset[complete.cases(dataset[variables]),]
            
            ## Hypothesis options checking
            if (self$options$values()$hypothesis == "oneGreater") {
                
                altHypothesis <- "greater"
                # Footnote message TBC
                
            } else if (self$options$values()$hypothesis == "twoGreater") {
                
                altHypothesis <- "less"
                # Footnote message TBC
                
            } else {
                altHypothesis <- "two.sided"
            }
            
            for (i in seq_along(variables)) {
                
                name <- variables[[i]]
                ## NA handling analysis by analysis
                if (naHandling == "perAnalysis")
                    dataset.clean <- dataset[complete.cases(dataset[name]),]
                
                column <- dataset.clean[[name]]
                
                n <- length(column)
                
                if (n == 0)
                    silkycore::reject("Variable '{a}' only contains missing values", code="na_variable", a=name)
                
                m <- mean(column)
                stdDev <- sd(column)
                se <- stdDev/sqrt(n)
                d <- (m-testValue)/stdDev #Cohen's d
                
                ## Normality test table
                res <- NULL
                if (n < 3) {
                    
                    normality$addFootnote(rowNo=i, "name", "Too few observations (N < 3) to compute statistic")
                    res$statistic <- ""
                    res$p.value <- ""
                    
                } else if (n > 5000) {
                    
                    normality$addFootnote(rowNo=i, "name", "Too many observations (N > 5000) to compute statistic")
                    res$statistic <- ""
                    res$p.value <- ""
                    
                } else if (column[n]-column[1L] == 0) {
                    
                    silkycore::reject("Variable '{a}' has essentially constant values", code="constant_variable", a=name)
                }
                else {
                    
                    res <- shapiro.test(column - testValue)
                }
                
                normality$setCell(rowNo=i, "w", res$statistic)
                normality$setCell(rowNo=i, "p", res$p.value)
                
                res <- NULL
                
                if (wantsStudents) {
                    
                    res <- t.test(column, mu=testValue, paired=FALSE, conf.level=cl, alternative=altHypothesis)
                    
                    ttest$setCell(rowNo=i, "studT", res$statistic)
                    ttest$setCell(rowNo=i, "studDf", res$parameter)
                    ttest$setCell(rowNo=i, "studP", res$p.value)
                    ttest$setCell(rowNo=i, "studMeanDiff", res$estimate - testValue)
                    ttest$setCell(rowNo=i, "studEffectSize", d)
                    ttest$setCell(rowNo=i, "studLowerCI", res$conf.int[1])
                    ttest$setCell(rowNo=i, "studUpperCI", res$conf.int[2])
                    
                }
                
                res<- NULL
                if (wantsMannWhitney) {
                    
                    res <- suppressWarnings(wilcox.test(column, mu=testValue, alternative=altHypothesis, paired=FALSE, conf.int=TRUE, conf.level=cl))
                    
                    ttest$setCell(rowNo=i, "mannV", res$statistic)
                    ttest$setCell(rowNo=i, "mannP", res$p.value)
                    ttest$setCell(rowNo=i, "mannMeanDiff", res$estimate - testValue)
                    ttest$setCell(rowNo=i, "mannEffectSize", d)
                    ttest$setCell(rowNo=i, "mannLowerCI", res$conf.int[1])
                    ttest$setCell(rowNo=i, "mannUpperCI", res$conf.int[2])
                    
                }
                
                ## Descriptives table
                desc$setCell(rowNo=i, "num", n)
                desc$setCell(rowNo=i, "mean", m)
                desc$setCell(rowNo=i, "sd", stdDev)
                desc$setCell(rowNo=i, "se", se)
                
            }
        }
    )
)

