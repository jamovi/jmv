
AnovaRMClass <- R6::R6Class(
    "AnovaRMClass",
    inherit=silkycore::Analysis,
    private=list(
        .model=NA,
        .init=function() {
            
            anovaTable    <- self$results$get('anova')
            
            rmTerms <- private$.rmTerms()
            if (length(rmTerms) > 0) {
                for (term in rmTerms)
                    anovaTable$addRow(rowKey=term, list(name=stringifyTerm(term)))
            } else {
                anovaTable$addRow(rowKey='.', list(name='.'))
                anovaTable$addRow(rowKey='', list(name='Residuals'))
            }
        },
        .run=function() {
            
        },
        .rmTerms=function() {
            
            rmFactors <- self$options$get('rm')
            bsFactors <- self$options$get('bs')
            covariates <- self$options$get('cov')
            
            if (length(rmFactors) == 0)
                rmFactors <- list(list(label="RM Factor 1"))
            
            bsNames <- c(bsFactors, covariates)
            
            rmNames <- sapply(rmFactors, function(x) x$label, simplify=TRUE)
            rmFormula <- as.formula(paste('~', paste(paste0('`', rmNames, '`'), collapse='*')))
            rmTerms <- attr(stats::terms(rmFormula), 'term.labels')
            rmTerms <- sapply(rmTerms, function(x) as.list(strsplit(x, ':')), USE.NAMES=FALSE)
            
            if (length(bsFactors) > 0) {
                bsFormula <- as.formula(paste('~', paste(paste0('`', bsFactors, '`'), collapse='*')))
                bsTerms <- attr(stats::terms(bsFormula), 'term.labels')
                bsTerms <- sapply(bsTerms, function(x) as.list(strsplit(x, ':')), USE.NAMES=FALSE)
            } else {
                bsTerms <- NULL
            }
            
            terms <- list()
            
            for (i in seq_along(rmTerms)) {
                
                rmTerm <- rmTerms[[i]]
                terms[[length(terms)+1]] <- rmTerm
                
                for (j in seq_along(bsTerms))
                    terms[[length(terms)+1]] <- c(rmTerm, bsTerms[[j]])
                
                terms[[length(terms)+1]] <- "Residual"
                
                #groups[[length(groups)+1]] <- terms
            }

            return(terms)
        })
)

