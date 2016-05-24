
AnovaClass <- R6::R6Class(
    "AnovaClass",
    inherit=silkycore::Analysis,
    public=list(
        run=function() {
            super$run()
            
            dependentName <- self$options$get('dependent')
            fixedFactors <- self$options$get('fixedFactors')
            
            if (is.null(dependentName) || is.null(fixedFactors))
                return()
            
            data <- self$data
            
            for (factorName in fixedFactors)
                data[[factorName]] <- as.factor(data[[factorName]])
            
            data[[dependentName]] <- as.numeric(data[[dependentName]])
            
            factors <- paste0('`', self$options$get('fixedFactors'), '`', collapse='*')
            formula <- paste0('`', self$options$get('dependent'), '`~', factors)
            
            formula <- stats::as.formula(formula)
            model <- stats::aov(formula, data)
            results <- car::Anova(model, type='II', singular.ok=TRUE)
            
            anovaTable <- self$results$get('anova')
            rowCount <- dim(results)[1]
            rowNames <- dimnames(results)[[1]]
            
            for (i in 1:rowCount) {
                rowName <- rowNames[i]
                tableRow <- list(
                    ss=results[i,'Sum Sq'],
                    df=results[i,'Df'],
                    ms=results[i,'Sum Sq'] / results[i,'Df'],
                    F=results[i,'F value'],
                    p=results[i,'Pr(>F)'])
                
                anovaTable$addRow(rowKey=rowName, tableRow)
            }
        })
)

