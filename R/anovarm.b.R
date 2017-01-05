
AnovaRMClass <- R6::R6Class(
    "AnovaRMClass",
    inherit=AnovaRMBase,
    private=list(
        .model=NA,
        .init=function() {
            
            rmTable <- self$results$get('rmTable')
            rmTable$setNote("Note", jmvcore::format("Type {} Sums of Squares", self$options$ss))
            
            rm <- private$.rmTerms()
            rmTerms <- rm$terms
            rmSpacing <- rm$spacing
            
            if (length(rmTerms) > 0) {
                for (i in seq_along(rmTerms)) {
                    if (rmTerms[i] == "Residual")
                        rmTable$addRow(rowKey=unlist(c(rmTerms[i-1],".RES")), list(name=stringifyTerm(rmTerms[i])))
                    else
                        rmTable$addRow(rowKey=unlist(rmTerms[i]), list(name=stringifyTerm(rmTerms[i])))
                }
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
            bsTable$setNote("Note", jmvcore::format("Type {} Sums of Squares", self$options$ss))
            
            bsTerms <- private$.bsTerms()
            if (length(bsTerms) > 0) {
                for (term in bsTerms)
                    bsTable$addRow(rowKey=unlist(term), list(name=stringifyTerm(term)))
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
                
                print(head(data))
                print(modelFormula)
                
                suppressWarnings({
                    
                    result <- try(afex::aov_car(modelFormula, data, type=self$options$ss), silent=TRUE)
                    
                }) # suppressWarnings
                
                if (isError(result)) {
                    print(extractErrorMessage(result))
                } else {
                    private$.populateTables(result)
                }
            }
        },
        .rmTerms=function() {
            
            if (length(self$options$rmTerms) == 0) { # if no specific model is specified
                
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
            
            if (length(self$options$bsTerms) == 0) { # if no specific model is specified
                
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
            
            attr(data, 'row.names') <- seq_len(length(data[[1]]))
            attr(data, 'class') <- 'data.frame'
            data <- jmvcore::naOmit(data)
            
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
            
            attr(data_long, 'row.names') <- seq_len(length(data_long[[1]]))
            attr(data_long, 'class') <- 'data.frame'
            data_long <- jmvcore::naOmit(data_long)

            return(data_long)
        },
        .modelFormula=function() {
            
            if (is.null(self$options$rmTerms)) {
                
                
                
            } else {
                
                bsTerms <- lapply(self$options$bsTerms, function(x) gsub('`', '\\`', x, fixed=TRUE))
                rmTerms <- lapply(self$options$rmTerms, function(x) gsub('`', '\\`', x, fixed=TRUE))
                
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
        },
        .populateTables=function(result) {
            
            rmTable <- self$results$get('rmTable')
            bsTable <- self$results$get('bsTable')
            
            rmTerms <- private$.rmTerms()$terms
            bsTerms <- private$.bsTerms()
            
            summaryResult <- summary(result)
            model <- summaryResult$univariate.tests
            epsilon <- summaryResult$pval.adjustments
            mauchly <- summaryResult$sphericity.tests
            
            rmRowKeys <- rmTable$rowKeys
            bsRowKeys <- bsTable$rowKeys
            
            resultRows <- jmvcore::decomposeTerms(as.list(rownames(model)))
            
            for (i in seq_along(resultRows)) {
                
                rmIndex <- which(sapply(rmRowKeys, function(x) setequal(resultRows[[i]], x)))
                bsIndex <- which(sapply(bsRowKeys, function(x) setequal(resultRows[[i]], x)))
                
                if (length(rmIndex) > 0 || length(bsIndex) > 0) {
                    
                    row <- list()
                    row[["ss"]] <- model[i,"SS"]
                    row[["df"]] <- model[i,"num Df"]
                    row[["ms"]] <- row[["ss"]] / row[["df"]]
                    row[["F"]] <- model[i,"F"]
                    row[["p"]] <- model[i,"Pr(>F)"]
                    
                    # SSr <- model[i,"Error SS"]
                    # # SSt <- sum(model[indices,"SS"]) + SSr
                    # SSt <- 10
                    # MSr <- SSr/model[i,"den Df"]
                    # 
                    # row[["eta"]] <- row[["ss"]] / SSt
                    # row[["partEta"]] <- row[["ss"]] / (row[["ss"]] + SSr)
                    # row[["omega"]] <- (row[["ss"]] - (row[["df"]] * MSr)) / (SSt + MSr)
                    
                    if (length(rmIndex) > 0) {
                        rmTable$setRow(rowNo=rmIndex, values=row)
                    } else {
                        bsTable$setRow(rowNo=bsIndex, values=row)
                    }
                }
            }
            
            resRowKeys <- rmRowKeys[which(sapply(rmRowKeys, function(x) ".RES" %in% x))]
            
            for (i in seq_along(resRowKeys)) {
                
                resid <- resRowKeys[[i]][ ! resRowKeys[[i]] %in% ".RES"]
                index <- which(sapply(as.list(resultRows), function(x) setequal(x, resid)))
                
                row <- list()
                row[["ss"]] <- model[index,"Error SS"]
                row[["df"]] <- model[index,"den Df"]
                row[["ms"]] <- row[["ss"]] / row[["df"]]
                row[["F"]] <- ""
                row[["p"]] <- ""
                
                rmTable$setRow(rowKey=resRowKeys[[i]], values=row)
            }
            
            row <- list()
            row[["ss"]] <- model["(Intercept)","Error SS"]
            row[["df"]] <- model["(Intercept)","den Df"]
            row[["ms"]] <- row[["ss"]] / row[["df"]]
            row[["F"]] <- ""
            row[["p"]] <- ""
            
            bsTable$setRow(rowKey="Residual", values=row)
        })
)

