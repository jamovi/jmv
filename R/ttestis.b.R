
TTestISClass <- R6Class("TTestISClass",
    inherit=silkycore::Analysis,
    private=list(
        .run=function() {
        
            groupVarName <- self$options$get('group')
            depVarNames <- self$options$get('vars')
            
            if (is.null(groupVarName) || length(depVarNames) == 0)
                return()
            
            data <- self$options$dataset()
            
            for (name in depVarNames)
                data[[name]] <- silkycore::toNumeric(data[[name]])
            
            ttestTable <- self$results$get("ttest")
            descTable <- self$results$get("desc")
            normTable <- self$results$get("assum")$get("norm")
            eqvTable <- self$results$get("assum")$get("eqv")
            
            confInt <- self$options$get('ciWidth') / 100
            
            if (any(depVarNames == groupVarName))
                silkycore::reject("Grouping variable '{a}' must not also be a dependent variable", code="a_is_dependent_variable", a=groupVarName)
            
            # exclude rows with missings in the grouping variable
            data <- data[ ! is.na(data[[groupVarName]]),]
            
            #if (dimn(data)[1] == 0)
            #    silkycore::reject("Grouping variable '{a}' must not also be a dependent variable", code="a_is_dependent_variable", a=groupVarName)
            
            groupLevels <- base::levels(data[[groupVarName]])
            
            if (length(groupLevels) != 2)
                silkycore::reject("Grouping variable '{a}' must have exactly 2 levels", code="grouping_var_must_have_2_levels", a=groupVarName)
            
            if (self$options$get('miss') == "listwise") {
                data <- na.omit(data)
                if (dim(data)[1] == 0)
                    silkycore::reject("Grouping variable '{a}' has less than 2 levels after missing values are excluded", code="grouping_var_must_have_2_levels", a=groupVarName)
            }
            
            ## Hypothesis options checking
            if (self$options$values()$hypothesis == "oneGreater") {
                Ha <- "greater"
                # Footnote message TBC
            } else if (self$options$values()$hypothesis == "twoGreater") {
                Ha <- "less"
                # Footnote message TBC
            } else {
                Ha <- "two.sided"
            }
            
            
            for (depName in depVarNames) {
                
                if (self$options$get('miss') == "perAnalysis") {
                    dataTTest <- data.frame(dep=data[[depName]], group=data[[groupVarName]])
                    dataTTest <- na.omit(dataTTest)
                } else {
                    dataTTest <- data
                }
                
                groupLevels <- base::levels(dataTTest$group)
                v <- tapply(dataTTest$dep, dataTTest$group, var)
                n <- tapply(dataTTest$dep, dataTTest$group, length)
                m <- tapply(dataTTest$dep, dataTTest$group, mean)
                se <- sqrt(v/n)
                
                sediff <- sqrt((v[1]/n[1])+(v[2]/n[2]))
                
                pooledSD <- sqrt(((n[1]-1)*v[1]+(n[2]-1)*v[2])/(n[1]+n[2]-2))
                d <- (m[1]-m[2])/pooledSD # Cohen's d
                
                
                ## Levene's test and equality of variances table
                
                levene <- car::leveneTest(dep ~ group, data=dataTTest, "mean")

                if (is.na(levene[1,"F value"])){

                    eqvTable$setRow(rowKey=depName, list("f"=NaN, "df"="", "p"=""))
                    eqvTable$addFootnote(rowNo=i, "f", "F-statistic could not be calculated")

                } else {

                    eqvTable$setRow(rowKey=depName, list(
                        "f"=levene[1,"F value"],
                        "df"=levene[1,"Df"],
                        "p"=levene[1,"Pr(>F)"]))
                }
                
                if (self$options$get('students')) {
                    
                    res <- t.test(dep ~ group, data=dataTTest, var.equal=TRUE, paired=FALSE, alternative=Ha, conf.level=confInt)
                    
                    ttestTable$setRow(rowKey=depName, list(
                        "stat[stud]"=res$statistic,
                        "df[stud]"=res$parameter,
                        "p[stud]"=res$p.value,
                        "md[stud]"=res$estimate[1]-res$estimate[2],
                        "sed[stud]"=sediff,
                        "es[stud]"=d,
                        "cil[stud]"=res$conf.int[1],
                        "ciu[stud]"=res$conf.int[2]))
                    
                    # ## Inform if a student's t-test is appropriate using Levene's test
                    # if (!wantsWelchs && !is.na(levene[1,"Pr(>F)"]) && levene[1,"Pr(>F)"] < .05)
                    #     ttestTable$addFootnote(rowNo=i, "studTest", "Levene's test is significant (p < .05), suggesting a violation of the equal variance assumption")
                    # 
                }
                
                if (self$options$get('welchs')) {

                    res <- t.test(dep ~ group, data=dataTTest, var.equal=FALSE, paired=FALSE, alternative=Ha, conf.level=confInt)
                    
                    ttestTable$setRow(rowKey=depName, list(
                        "stat[welc]"=res$statistic,
                        "df[welc]"=res$parameter,
                        "p[welc]"=res$p.value,
                        "md[welc]"=res$estimate[1]-res$estimate[2],
                        "sed[welc]"=sediff,
                        "es[welc]"=d,
                        "cil[welc]"=res$conf.int[1],
                        "ciu[welc]"=res$conf.int[2]))
                }
                
                if (self$options$get('mann')) {

                    res <- suppressWarnings(wilcox.test(dep ~ group, data=dataTTest, alternative=Ha, paired=FALSE, conf.int=TRUE, conf.level=confInt))

                    ttestTable$setRow(rowKey=depName, list(
                        "stat[mann]"=res$statistic,
                        "df[mann]"=res$parameter,
                        "p[mann]"=res$p.value,
                        "md[mann]"=res$estimate,
                        "sed[mann]"=sediff,
                        "es[mann]"=d,
                        "cil[mann]"=res$conf.int[1],
                        "ciu[mann]"=res$conf.int[2]))
                }
                
                if (self$options$get('norm')) {
                    
                    columns <- tapply(dataTTest$dep, dataTTest$group, na.omit)
                    values <- list()
                    values[["name"]] <- depName
                    
                    column1 <- columns[[1]]
                    column2 <- columns[[2]]
                    
                    for (i in 1:2) {
                        group <- groupLevels[i]
                        column <- columns[[i]]
                        
                        values[[paste0('group[', i, ']')]] <- group
                        
                        if (length(column) < 3) {
                            values[[paste0('w[', i, ']')]] <- NaN
                            values[[paste0('p[', i, ']')]] <- ''
                        } else if (length(column) > 5000) {
                            values[[paste0('w[', i, ']')]] <- NaN
                            values[[paste0('p[', i, ']')]] <- ''
                        } else {
                            res <- shapiro.test(column)
                            values[[paste0('w[', i, ']')]] <- res$statistic
                            values[[paste0('p[', i, ']')]] <- res$p.value
                        }
                    }
                    
                    normTable$setRow(rowKey=depName, values)
                }
                
                if (self$options$get('desc')) {
                    
                    descTable$setRow(rowKey=depName, list(
                        "dep"=depName,
                        "group[1]"=groupLevels[1],
                        "num[1]"=n[1],
                        "mean[1]"=m[1],
                        "sd[1]"=sqrt(v[1]),
                        "se[1]"=se[1],
                        "group[2]"=groupLevels[2],
                        "num[2]"=n[2],
                        "mean[2]"=m[2],
                        "sd[2]"=sqrt(v[2]),
                        "se[2]"=se[2]
                    ))
                }
            }
        }
    )
)

