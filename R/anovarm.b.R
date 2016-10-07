
AnovaRMClass <- R6::R6Class(
    "AnovaRMClass",
    inherit=jmvcore::Analysis,
    private=list(
        .model=NA,
        .init=function() {
            
            rmTable <- self$results$get('rmTable')
            
            rmTerms <- private$.rmTerms()
            if (length(rmTerms) > 0) {
                for (term in rmTerms)
                    rmTable$addRow(rowKey=term, list(name=stringifyTerm(term)))
            } else {
                rmTable$addRow(rowKey='.', list(name='.'))
                rmTable$addRow(rowKey='', list(name='Residual'))
            }
            
            bsTable <- self$results$get('bsTable')
            bsTable$addRow(rowKey='', list(name='Residual'))
            
            spher <- self$results$get('assump')$get('spher')
            term <- stringifyTerm(rmTerms[[1]])
            spher$addRow(rowKey='', list(name=term))
        },
        .run=function() {
            
            data <- naOmit(self$data)
            
            rmFactors <- self$options$get('rm')
            rmCells <- self$options$get('rmCells')
                
            mat <- matrix(nrow=nrow(data), ncol=length(rmCells))
            
            for (i in seq_along(rmCells)) {
                rmCell <- rmCells[[i]]
                mat[,i] <- toNumeric(data[[rmCell$measure]])
            }
            
            model <- lm(mat~1)
            
            idata <- NULL
            
            for (i in length(rmFactors):1) {
                factor <- rmFactors[[i]]
                if (is.null(idata)) {
                    idata <- as.data.frame(as.factor(factor$levels))
                    names(idata) <- factor$label
                } else {
                    append <- idata
                    for (i in 2:length(factor$levels))
                        idata <- rbind(idata, append)
                    reps <- nrow(idata) / length(factor$levels)
                    idata <- cbind(idata, as.factor(rep(factor$levels, each=reps)))
                    names(idata)[[ncol(idata)]] <- factor$label
                }
            }
            
            rmNames <- sapply(rmFactors, function(x) x$label, simplify=TRUE)
            idesign <- as.formula(paste('~', paste(paste0('`', rmNames, '`'), collapse='*')))
            
            anova <- car::Anova(model, idata=idata, idesign=idesign, type='3')
            summ <- summary(anova, multivariate=FALSE)
            tests <- summ$univariate.tests
            
            table <- self$results$get('rmTable')
            
            for (i in 2:nrow(tests)-1) {
                table$setRow(rowNo=i*2-1, list(
                    ss=tests[i+1,'SS'],
                    df=tests[i+1,'num Df'],
                    ms=tests[i+1,'SS'] / tests[i+1,'num Df'],
                    F=tests[i+1,'F'],
                    p=tests[i+1,'Pr(>F)']))
                table$setRow(rowNo=i*2, list(
                    ss=tests[i+1,'Error SS'],
                    df=tests[i+1,'den Df'],
                    ms=tests[i+1,'Error SS'] / tests[i+1,'den Df'],
                    F='',
                    p=''))
            }
            
            table <- self$results$get('bsTable')
            
            table$setRow(rowNo=1, list(
                ss=tests[1,'Error SS'],
                df=tests[1,'den Df'],
                ms=tests[1,'Error SS'] / tests[1,'den Df'],
                F='',
                p=''))
            
            if (nrow(summ$sphericity.tests)) {
                tests <- summ$sphericity.tests
                table <- self$results$get('assump')$get('spher')
                table$setRow(rowNo=1, list(
                    mauch=tests[1,'Test statistic'],
                    p=tests[1,'p-value']))
                
                tests <- summ$pval.adjustments
                table$setRow(rowNo=1, list(
                    gg=tests[1,'GG eps'],
                    hf=min(1, tests[1,'HF eps'])))
            }
            
            
            #modelTerms <- private$.rmTerms()
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

