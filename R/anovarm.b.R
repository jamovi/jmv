
AnovaRMClass <- R6::R6Class(
    "AnovaRMClass",
    inherit=AnovaRMBase,
    private=list(
        .model=NA,
        .init=function() {
            
            rmTable <- self$results$get('rmTable')
            
            rm <- private$.rmTerms()
            rmTerms <- rm$terms
            rmSpacing <- rm$spacing
            
            if (length(rmTerms) > 0) {
                for (term in rmTerms)
                    rmTable$addRow(rowKey=term, list(name=stringifyTerm(term)))
            } else {
                rmTable$addRow(rowKey='.', list(name='.'))
                rmTable$addRow(rowKey='', list(name='Residual'))
            }
            
            for (i in seq_along(rmSpacing)) {
                if ( ! is.null(rmSpacing[[i]])) {
                    if (rmSpacing[[i]] == "both")
                        rmTable$addFormat(rowNo=i, col=1, Cell.BEGIN_END_GROUP)
                    else if (rmSpacing[[i]] == "above")
                        rmTable$addFormat(rowNo=i, col=1, Cell.BEGIN_GROUP)
                    else if (rmSpacing[[i]] == "below")
                        rmTable$addFormat(rowNo=i, col=1, Cell.END_GROUP)    
                }
            }

            bsTable <- self$results$get('bsTable')
            
            bsTerms <- private$.bsTerms()
            if (length(bsTerms) > 0) {
                for (term in bsTerms)
                    bsTable$addRow(rowKey=term, list(name=stringifyTerm(term)))
            } else {
                bsTable$addRow(rowKey='', list(name='Residual'))
            }
            
            spher <- self$results$get('assump')$get('spher')
            term <- stringifyTerm(rmTerms[[1]])
            spher$addRow(rowKey='', list(name=term))
        },
        .run=function() {
            
            bs <- self$options$bs
            cov <- self$options$cov
            
            dataSelected <- ! sapply(lapply(self$options$rmCells, function(x) return(x$measure)), is.null)
            ready <- sum(dataSelected) == length(self$options$rmCells)
            
            if (ready) {
                
                data <- private$.wideToLong()
                modelFormula <- private$.modelFormula()
                model <- afex::aov_car(modelFormula, data)
                
                print(summary(model))
            }
        },
        .rmTerms=function() {
            
            if (is.null(self$options$rmTerms) || is.null(self$options$bsTerms)) { # if no specific model is specified
                
                rmFactors <- self$options$rm
                bsFactors <- self$options$bs
                covariates <- self$options$cov
                
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
                spacing <- list()
                
                for (i in seq_along(rmTerms)) {
                    
                    rmTerm <- rmTerms[[i]]
                    terms[[length(terms)+1]] <- rmTerm
                    
                    for (j in seq_along(bsTerms))
                        terms[[length(terms)+1]] <- c(rmTerm, bsTerms[[j]])
                    
                    terms[[length(terms)+1]] <- "Residual"
                    spacing[[length(terms)]] <- "below"
                }
                
            } else { # if the user specifies a model
                
                rmTerms <- self$options$rmTerms
                bsTerms <- self$options$bsTerms
                
                terms <- list()
                spacing <- list()
                    
                for (i in seq_along(rmTerms)) {
                    
                    rmTerm <- rmTerms[[i]]
                    terms[[length(terms) + 1]] <- rmTerm
                    spacing[[length(terms)]] <- "above"
                    
                    for (j in seq_along(bsTerms))
                        terms[[length(terms) + 1]] <- c(rmTerm, bsTerms[[j]])
                    
                    terms[[length(terms) + 1]] <- "Residual"
                    spacing[[length(terms)]] <- "below"
                }
            }
            
            return(list(terms = terms, spacing = spacing))
        },
        .bsTerms=function() {
            
            if (is.null(self$options$rmTerms) || is.null(self$options$bsTerms)) { # if no specific model is specified
                
                bsFactors <- self$options$bs
                covariates <- self$options$cov
                
                if (length(bsFactors) > 0) {
                    bsFormula <- as.formula(paste('~', paste(paste0('`', bsFactors, '`'), collapse='*')))
                    bsTerms <- attr(stats::terms(bsFormula), 'term.labels')
                    bsTerms <- sapply(bsTerms, function(x) as.list(strsplit(x, ':')), USE.NAMES=FALSE)
                } else {
                    bsTerms <- list()
                }
                
                terms <- bsTerms
                
                for (i  in seq_along(covariates))
                    terms[[length(terms) + 1]] <- covariates[i]
                
                terms[[length(terms) + 1]] <- "Residual"
                
            } else { # if the user specifies a model
                
                bsTerms <- self$options$bsTerms
                bsFactors <- self$options$bs
                covariates <- self$options$cov
                
                terms <- list()
                
                # terms that include covariates:
                covTerms <- c()
                if (length(covariates) > 0)
                    covTerms <- apply(sapply(as.list(covariates), function (y) sapply(bsTerms, function(x) y %in% x)), 1, any)

                if (sum(covTerms) != length(covariates) || length(covTerms) == 0) {
                    
                    terms <- c(terms, bsTerms)
                    terms[[length(terms) + 1]] <- "Residual"    
                    
                } else {
                    
                    terms <- c(terms, bsTerms[ ! covTerms])
                    
                    for (i in seq_along(covariates))
                        terms[[length(terms) + 1]] <- covariates[[i]]
                    
                    terms[[length(terms) + 1]] <- "Residual"
                    
                }
            }
            
            return(terms)
        },
        .wideToLong=function() {
            
            rmFactors <- self$options$rm
            rmCells <- self$options$rmCells
            
            rmVars <- sapply(rmCells, function(x) return(x$measure)) 
            bsVars <- self$options$bs
            covVars <- self$options$cov
            
            labels <- sapply(rmFactors, function(x) return(x$label))
            levels <- lapply(rmFactors, function(x) return(x$levels))
            rmCells <- lapply(rmCells, function(x) return(x$cell))
            
            data <- list()
            for (var in c(rmVars, covVars))
                data[[var]] <- jmvcore::toNumeric(self$data[[var]])
            
            for (var in bsVars)
                data[[var]] <- self$data[[var]]
            
            data <- jmvcore::naOmit(data)
            attr(data, 'row.names') <- seq_len(length(data[[1]]))
            attr(data, 'class') <- 'data.frame'
            
            data <- cbind(data, "subject" = factor(1:nrow(data)))
            
            data_long <- as.list(reshape2::melt(data, id.vars=c(bsVars, covVars, "subject"), measure.vars=rmVars, value.name="dependent"))
            
            col <- data_long[["variable"]]
            temp <- numeric(length(col))
            for (j in seq_along(col))
                temp[j] <- which(rmVars %in% col[j])
            
            for (i in seq_along(labels)) {
                data_long[[labels[[i]]]] <- sapply(rmCells[temp], function(x) x[i])
                levels(data_long[[labels[[i]]]]) <- levels[[i]]
            }
            data_long[["variable"]] <- NULL
            
            data_long <- jmvcore::naOmit(data_long)
            attr(data_long, 'row.names') <- seq_len(length(data_long[[1]]))
            attr(data_long, 'class') <- 'data.frame'

            return(data_long)
        },
        .modelFormula=function() {
            
            if (is.null(self$options$rmTerms)) {
                
                
                
            } else {
                
                bsTerms <- self$options$bsTerms
                rmTerms <- self$options$rmTerms
                
                bsItems <- list()
                for (i in seq_along(bsTerms))
                    bsItems[[length(bsItems)+1]] <- paste0('`', bsTerms[[i]], '`', collapse=":")
                bsTerm <- paste0("(", paste0(bsItems, collapse = " + "), ")")
                
                rmItems <- list()
                for (i in seq_along(rmTerms))
                    rmItems[[length(rmItems)+1]] <- paste0('`', rmTerms[[i]], '`', collapse=":")
                rmTerm <- paste0("Error(", paste0("subject/(", rmItems, ")", collapse=" + "),")")
                
                allItems <- c(bsItems, rmItems)
                for (i in seq_along(rmItems)) {
                    for (j in seq_along(bsItems)) {
                        allItems[[length(allItems) + 1]] <- paste0(unlist(c(rmItems[[i]], bsItems[[j]])), collapse=":")
                    }
                }
                
                mainTerm <- paste0("(", paste0(allItems, collapse = " + "), ")")
                
                if (length(bsTerms) == 0) {
                    formula <- as.formula(paste("dependent", "~", paste(mainTerm, rmTerm, sep=" + ")))
                } else {
                    formula <- as.formula(paste("dependent", "~", paste(mainTerm, rmTerm, bsTerm, sep=" + ")))
                }
                
                return(formula)
                
            }
        })
)

