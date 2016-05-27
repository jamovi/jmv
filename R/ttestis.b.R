
TTestISClass <- R6Class("TTestISClass",
    inherit=silkycore::Analysis,
    private=list(
        .run=function() {
        
            dataset <- self$options$dataset()
            naHandling <- self$options$values()$miss
            
            ttest <- self$results$get("ttest")
            desc <- self$results$get("descriptives")
            normality <- self$results$get("normality")
            equality <- self$results$get("equalityOfV")
            
            groupingVariable <- self$options$values()$groupingVar
            dependentVariables <- self$options$values()$vars
            
            wantsStudents <- self$options$values()$student
            wantsWelchs <- self$options$values()$welch
            wantsMannWhitney <- self$options$values()$mann
            
            cl <- self$options$values()$ciWidth/100
            
            ## Clean up NAs in grouping variable from the whole dataset
            dataset <- dataset[complete.cases(dataset[groupingVariable]),]
            
            ## Error catching for improper grouping variables
            if (length(groupingVariable) > 1)
                silkycore::reject("There must only be one grouping variable", code="too_many_grouping_vars")
            
            else if (any(dependentVariables == groupingVariable))
                silkycore::reject("Grouping variable '{a}' must not also be a dependent variable", code="a_is_dependent_variable", a=groupingVariable)
            
            else if (length(unique(dataset[[groupingVariable]])) != 2 || length(levels(dataset[[groupingVariable]])) != 2)
                silkycore::reject("Grouping variable '{a}' must have exactly 2 levels", code="grouping_var_must_have_2_levels", a=groupingVariable)
            
            ## After checking for problems in the grouping variable, we can safely assign the levels
            groupingVariableLevels <- levels(dataset[[groupingVariable]])
            
            ## Listwise NA cleanup
            if (naHandling == "listwise") {
                
                dataset.clean <- dataset[complete.cases(dataset[dependentVariables]),]
                
                ## Check if removing NA has excluded all of one group in the grouping variable
                if (length(unique(dataset.clean[[groupingVariable]])) != 2)
                    silkycore::reject("Grouping variable '{a}' has less than 2 levels after missing values are excluded", code="grouping_var_must_have_2_levels", a=groupingVariable)
                
            }
            
            ## Hypothesis options checking
            if (self$options$values()$hypothesis == "oneGreater") {
                
                altHypothesis <- "greater"
                # Footnote message TBC
                
            } else if (self$options$values()$hypothesis == "twoGreater") {
                
                altHypothesis <- "less"
                # Footnote message TBC
                
            } else 
                altHypothesis <- "two.sided"
            
            
            for (i in seq_along(dependentVariables)) {

                name   <- dependentVariables[[i]]
                
                ## NA handling analysis by analysis
                if (naHandling == "perAnalysis") {
                    
                    dataset.clean <- dataset[complete.cases(dataset[[name]]),]
                
                    ## Check if removing NA has excluded all of one group in the grouping variable
                    if (length(unique(dataset.clean[[groupingVariable]])) != 2)
                        silkycore::reject("Grouping variable '{a}' has less than 2 levels after missing values of dependent variable '{b}' are excluded", code="grouping_var_must_have_2_levels", a=groupingVariable, b=name)
                
                }
                
                column <- dataset.clean[[name]]
                
                f <- as.formula(paste(dependentVariables[i], "~", groupingVariable))
                
                sampleVar <- tapply(column, dataset.clean[[groupingVariable]], var)
                n <- tapply(column, dataset.clean[[groupingVariable]], length)
                m <- tapply(column, dataset.clean[[groupingVariable]], mean)
                se <- sqrt(sampleVar/n)
                
                sediff <- sqrt((sampleVar[1]/n[1])+(sampleVar[2]/n[2]))
                
                pooledSD <- sqrt(((n[1]-1)*sampleVar[1]+(n[2]-1)*sampleVar[2])/(n[1]+n[2]-2))
                d <- (m[1]-m[2])/pooledSD # Cohen's d
                
                ## Levene's test and equality of variances table
                levene <- NULL
                levene <- car::leveneTest(f, data=dataset.clean, "mean")
                
                if (is.na(levene[1,"F value"])){
                    
                    equality$addFootnote(rowNo=i,"name","F-statistic could not be calculated")
                    equality$setCell(rowNo=i,"f","")
                    equality$setCell(rowNo=i,"df","")
                    equality$setCell(rowNo=i,"p","")
                    
                } else {
                    
                    equality$setCell(rowNo=i,"f",levene[1,"F value"])
                    equality$setCell(rowNo=i,"df",levene[1,"Df"])
                    equality$setCell(rowNo=i,"p",levene[1,"Pr(>F)"])
                    
                }
                
                ## T-test table implementation
                res <- NULL
                if (wantsStudents) {
                    
                    res <- t.test(f, data=dataset.clean, var.equal=TRUE, paired=FALSE, alternative=altHypothesis, conf.level=cl)
                    
                    ttest$setCell(rowNo=i, "studT", res$statistic)
                    ttest$setCell(rowNo=i, "studDf", res$parameter)
                    ttest$setCell(rowNo=i, "studP", res$p.value)
                    ttest$setCell(rowNo=i, "studMeanDiff", res$estimate[1]-res$estimate[2])
                    ttest$setCell(rowNo=i, "studSEDiff", sediff)
                    ttest$setCell(rowNo=i, "studEffectSize", d)
                    ttest$setCell(rowNo=i, "studLowerCI", res$conf.int[1])
                    ttest$setCell(rowNo=i, "studUpperCI", res$conf.int[2])
                    
                    ## Inform if a student's t-test is appropriate using Levene's test
                    if (!wantsWelchs && !is.na(levene[1,"Pr(>F)"]) && levene[1,"Pr(>F)"] < .05)
                        ttest$addFootnote(rowNo=i, "studTest", "Levene's test is significant (p < .05), suggesting a violation of the equal variance assumption")
                    
                }
                
                res <- NULL
                if (wantsWelchs) {
                    
                    res <- t.test(f, data=dataset.clean, var.equal=FALSE, paired=FALSE, alternative=altHypothesis, conf.level=cl)
                    
                    ttest$setCell(rowNo=i, "welchT", res$statistic)
                    ttest$setCell(rowNo=i, "welchDf", res$parameter)
                    ttest$setCell(rowNo=i, "welchP", res$p.value)
                    ttest$setCell(rowNo=i, "welchMeanDiff", res$estimate[1]-res$estimate[2])
                    ttest$setCell(rowNo=i, "welchSEDiff", sediff)
                    ttest$setCell(rowNo=i, "welchEffectSize", d)
                    ttest$setCell(rowNo=i, "welchLowerCI", res$conf.int[1])
                    ttest$setCell(rowNo=i, "welchUpperCI", res$conf.int[2])
                }
                
                res <- NULL
                if (wantsMannWhitney) {
                    
                    res <- suppressWarnings(wilcox.test(f, data=dataset.clean, alternative=altHypothesis, paired=FALSE ,conf.int=TRUE, conf.level=cl))
                    
                    ttest$setCell(rowNo=i, "mannW", res$statistic)
                    ttest$setCell(rowNo=i, "mannP", res$p.value)
                    ttest$setCell(rowNo=i, "mannMeanDiff", res$estimate)
                    ttest$setCell(rowNo=i, "mannSEDiff", sediff)
                    ttest$setCell(rowNo=i, "mannEffectSize", d)
                    ttest$setCell(rowNo=i, "mannLowerCI", res$conf.int[1])
                    ttest$setCell(rowNo=i, "mannUpperCI", res$conf.int[2])
                }
                
                ## Normality test table & Descriptives table
                for (k in seq_along(groupingVariableLevels)) {
                    
                    res <- NULL
                    currentGroup <- 2*(i-1)+k
                    
                    groupColumn <- column[dataset.clean[[groupingVariable]] == groupingVariableLevels[k]]
                    
                    if (length(groupColumn) < 3) {
                        normality$addFootnote(rowNo=currentGroup, "group", "Too few observations (N < 3) to compute statistic")
                        res$statistic <- ""
                        res$p.value <- ""
                    }
                    else if (length(groupColumn) > 5000) {
                        normality$addFootnote(rowNo=currentGroup, "group", "Too many observations (N > 5000) to compute statistic")
                        res$statistic <- ""
                        res$p.value <- ""
                    }
                    else {
                        res <- shapiro.test(groupColumn)
                    }
                    
                    normality$setCell(rowNo=currentGroup, "group", groupingVariableLevels[k])
                    normality$setCell(rowNo=currentGroup, "w", res$statistic)
                    normality$setCell(rowNo=currentGroup, "p", res$p.value)
                    
                    desc$setCell(rowNo=currentGroup, "group", groupingVariableLevels[k])
                    desc$setCell(rowNo=currentGroup, "num", n[k])
                    desc$setCell(rowNo=currentGroup, "mean", m[k])
                    desc$setCell(rowNo=currentGroup, "sd", sqrt(sampleVar[k]))
                    desc$setCell(rowNo=currentGroup, "se", se[k])
                    
                }
            }
        }
    )
)

