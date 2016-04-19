
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
                    
                    equality$addFootnote(i,"name","F-statistic could not be calculated")
                    equality$setCell(i,"f","")
                    equality$setCell(i,"df","")
                    equality$setCell(i,"p","")
                    
                } else {
                    
                    equality$setCell(i,"f",levene[1,"F value"])
                    equality$setCell(i,"df",levene[1,"Df"])
                    equality$setCell(i,"p",levene[1,"Pr(>F)"])
                    
                }
                
                ## T-test table implementation
                res <- NULL
                if (wantsStudents) {
                    
                    res <- t.test(f, data=dataset.clean, var.equal=TRUE, paired=FALSE, alternative=altHypothesis, conf.level=cl)
                    
                    ttest$setCell(i, "studT", res$statistic)
                    ttest$setCell(i, "studDf", res$parameter)
                    ttest$setCell(i, "studP", res$p.value)
                    ttest$setCell(i, "studMeanDiff", res$estimate[1]-res$estimate[2])
                    ttest$setCell(i, "studSEDiff", sediff)
                    ttest$setCell(i, "studEffectSize", d)
                    ttest$setCell(i, "studLowerCI", res$conf.int[1])
                    ttest$setCell(i, "studUpperCI", res$conf.int[2])
                    
                    ## Inform if a student's t-test is appropriate using Levene's test
                    if (!wantsWelchs && !is.na(levene[1,"Pr(>F)"]) && levene[1,"Pr(>F)"] < .05)
                        ttest$addFootnote(i, "studTest", "Levene's test is significant (p < .05), suggesting a violation of the equal variance assumption")
                    
                }
                
                res <- NULL
                if (wantsWelchs) {
                    
                    res <- t.test(f, data=dataset.clean, var.equal=FALSE, paired=FALSE, alternative=altHypothesis, conf.level=cl)
                    
                    ttest$setCell(i, "welchT", res$statistic)
                    ttest$setCell(i, "welchDf", res$parameter)
                    ttest$setCell(i, "welchP", res$p.value)
                    ttest$setCell(i, "welchMeanDiff", res$estimate[1]-res$estimate[2])
                    ttest$setCell(i, "welchSEDiff", sediff)
                    ttest$setCell(i, "welchEffectSize", d)
                    ttest$setCell(i, "welchLowerCI", res$conf.int[1])
                    ttest$setCell(i, "welchUpperCI", res$conf.int[2])
                }
                
                res <- NULL
                if (wantsMannWhitney) {
                    
                    res <- wilcox.test(f, data=dataset.clean, alternative=altHypothesis, paired=FALSE ,conf.int=TRUE, conf.level=cl)
                    
                    ttest$setCell(i, "mannW", res$statistic)
                    ttest$setCell(i, "mannP", res$p.value)
                    ttest$setCell(i, "mannMeanDiff", res$estimate)
                    ttest$setCell(i, "mannSEDiff", sediff)
                    ttest$setCell(i, "mannEffectSize", d)
                    ttest$setCell(i, "mannLowerCI", res$conf.int[1])
                    ttest$setCell(i, "mannUpperCI", res$conf.int[2])
                }
                
                ## Normality test table & Descriptives table
                for (k in seq_along(groupingVariableLevels)) {
                    
                    res <- NULL
                    currentGroup <- 2*(i-1)+k
                    
                    groupColumn <- column[dataset.clean[[groupingVariable]] == groupingVariableLevels[k]]
                    
                    if (length(groupColumn) < 3) {
                        normality$addFootnote(currentGroup, "group", "Too few observations (N < 3) to compute statistic")
                        res$statistic <- ""
                        res$p.value <- ""
                    }
                    else if (length(groupColumn) > 5000) {
                        normality$addFootnote(currentGroup, "group", "Too many observations (N > 5000) to compute statistic")
                        res$statistic <- ""
                        res$p.value <- ""
                    }
                    else {
                        res <- shapiro.test(groupColumn)
                    }
                    
                    normality$setCell(currentGroup, "group", groupingVariableLevels[k])
                    normality$setCell(currentGroup, "w", res$statistic)
                    normality$setCell(currentGroup, "p", res$p.value)
                    
                    desc$setCell(currentGroup, "group", groupingVariableLevels[k])
                    desc$setCell(currentGroup, "num", n[k])
                    desc$setCell(currentGroup, "mean", m[k])
                    desc$setCell(currentGroup, "sd", sqrt(sampleVar[k]))
                    desc$setCell(currentGroup, "se", se[k])
                    
                }
            }
        }
    )
)

