
TTestPSClass <- R6Class("TTestPSClass",
    inherit=silkycore::Analysis,
    private=list(
        .run=function() {
            
            dataset <- self$options$dataset()
            naHandling <- self$options$values()$miss
            
            ttest <- self$results$get("ttest")
            desc  <- self$results$get("descriptives")
            normality <- self$results$get("normality")
            
            variables <- self$options$values()$vars
            
            wantsStudents <- self$options$values()$student
            wantsWilcoxon <- self$options$values()$wilcoxon
            
            uniqueVars <- unique(variables)
            
            cl <- self$options$values()$ciWidth/100
            
            ## check if all variables come in pairs
            if (length(variables) %% 2 != 0)
                silkycore::reject("At least one variable not paired", code="unpaired_variables")
            
            ## Listwise NA cleanup
            if (naHandling == "listwise")
                dataset.clean <- dataset[complete.cases(dataset[unique(variables)]),]

            ## Hypothesis options checking
            if (self$options$values()$hypothesis == "oneGreater") {
                
                altHypothesis <- "greater"
                # Footnote message TBC
                
            } else if (self$options$values()$hypothesis == "twoGreater") {
                
                altHypothesis <- "less"
                # Footnote message TBC
                
            } else 
                altHypothesis <- "two.sided"
            
            for (i in seq_len(length(variables)/2)) {
                
                name1 <- variables[[2*i-1]]
                name2 <- variables[[2*i]]

                ## NA handling analysis by analysis
                if (naHandling == "perAnalysis")
                    dataset.clean <- dataset[complete.cases(dataset[c(name1,name2)]),]
                
                
                column1 <- dataset.clean[[name1]]
                column2 <- dataset.clean[[name2]]
                n <- length(column1)
                
                if (n == 0)
                    silkycore::reject("At least one variable only contains missing values", code="na_variable")
                
                sampleVar1 <- var(column1)
                sampleVar2 <- var(column2)
                n <- length(column1)
                m1 <- mean(column1)
                m2 <- mean(column2)
                se1 <- sqrt(sampleVar1/n)
                se2 <- sqrt(sampleVar2/n)
                
                pooledSD <- sd(column1-column2)
                sediff <- pooledSD/sqrt(n)
                d <- (m1-m2)/pooledSD #Cohen's d
                
                ## Normality test table
                res <- NULL
                if (n < 3) {
                    normality$addFootnote(i, "name", "Too few observations (N < 3) to compute statistic")
                    res$statistic <- ""
                    res$p.value <- ""
                }
                else if (n > 5000) {
                    normality$addFootnote(i, "name", "Too many observations (N > 5000) to compute statistic")
                    res$statistic <- ""
                    res$p.value <- ""
                } else if ((column1[n]-column2[n])-(column1[1L]-column2[1L]) == 0) {
                    silkycore::reject("'{a} - {b}' is essentially constant", code="constant_variable", a=name1, b=name2)
                }
                else {
                    res <- shapiro.test(column1-column2)
                }
                
                normality$setCell(i, "name", paste(name1, "-", name2))
                normality$setCell(i, "w", res$statistic)
                normality$setCell(i, "p", res$p.value)
                
                ## T Test table
                res<- NULL
                if (wantsStudents) {
                    
                    res <- t.test(column1, column2, paired=TRUE, conf.level=cl, alternative=altHypothesis)
                    
                    ttest$setCell(i, "name", paste(name1, "-", name2))
                    
                    ttest$setCell(i, "studT", res$statistic)
                    ttest$setCell(i, "studDf", res$parameter)
                    ttest$setCell(i, "studP", res$p.value)
                    ttest$setCell(i, "studMeanDiff", res$estimate)
                    ttest$setCell(i, "studSEDiff", sediff)
                    ttest$setCell(i, "studEffectSize", d)
                    ttest$setCell(i, "studLowerCI", res$conf.int[1])
                    ttest$setCell(i, "studUpperCI", res$conf.int[2])
                    
                }
                
                res<- NULL
                if (wantsWilcoxon) {
                    
                    res <- wilcox.test(column1, column2, alternative=altHypothesis, paired=TRUE, conf.int=TRUE, conf.level=cl)
                    
                    ttest$setCell(i, "name", paste(name1, "-", name2))
                    
                    ttest$setCell(i, "wilcoxonW", res$statistic)
                    ttest$setCell(i, "wilcoxonP", res$p.value)
                    ttest$setCell(i, "wilcoxonMeanDiff", res$estimate)
                    ttest$setCell(i, "wilcoxonSEDiff", sediff)
                    ttest$setCell(i, "wilcoxonEffectSize", d)
                    ttest$setCell(i, "wilcoxonLowerCI", res$conf.int[1])
                    ttest$setCell(i, "wilcoxonUpperCI", res$conf.int[2])
                    
                }
                
                ## Descriptives table
                descVar1 <- which(uniqueVars == name1)
                descVar2 <- which(uniqueVars == name2)
                
                desc$setCell(descVar1, "num", n)
                desc$setCell(descVar2, "num", n)
                desc$setCell(descVar1, "mean", m1)
                desc$setCell(descVar2, "mean", m2)
                desc$setCell(descVar1, "sd", sqrt(sampleVar1))
                desc$setCell(descVar2, "sd", sqrt(sampleVar2))
                desc$setCell(descVar1, "se", se1)
                desc$setCell(descVar2, "se", se2)
                
            }
        }
    )
)

