
#' @rdname jamovi
#' @importFrom afex aov_car
#' @importFrom ggplot2 ggplot position_dodge
#' @importFrom lsmeans lsmeans
#' @export
anovaRMClass <- R6::R6Class(
    "anovaRMClass",
    inherit=anovaRMBase,
    private=list(
        .model=NA,
        .postHocRows=NA,
        .run=function() {
            
            bs <- self$options$bs
            cov <- self$options$cov
            
            dataSelected <- ! sapply(lapply(self$options$rmCells, function(x) return(x$measure)), is.null)
            ready <- sum(dataSelected) == length(self$options$rmCells)
            
            if (ready) {
                
                data <- private$.wideToLong()
                modelFormula <- private$.modelFormula()
                
                suppressWarnings({
                    
                    result <- try(afex::aov_car(modelFormula, data, type=self$options$ss, factorize = FALSE), silent=TRUE)
                    
                }) # suppressWarnings
                
                if (isError(result)) {
                    jmvcore::reject(format('\n{}', extractErrorMessage(result)), code="error")
                } else {
                    private$.populateTables(result)
                }

                private$.populateSpher(result)
                private$.postHocTest(result)
                private$.prepareDescPlots(data)
            }
        },
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
            
            isAxis <- ! is.null(self$options$descPlotsHAxis)
            isMulti <- ! is.null(self$options$descPlotsSepPlots)
            
            self$results$get('descPlot')$setVisible( ! isMulti && isAxis)
            self$results$get('descPlots')$setVisible(isMulti)
            
            if (isMulti) {

                sepPlotsName <- self$options$descPlotsSepPlots
                
                if (sepPlotsName %in% self$options$bs) {
                    sepPlotsVar <- self$data[[sepPlotsName]]
                    sepPlotsLevels <- levels(sepPlotsVar)
                } else  {
                    rmLabels <- sapply(self$options$rm, function(x) return(x$label))
                    rmLevels <- lapply(self$options$rm, function(x) return(x$levels))
                    sepPlotsLevels <- rmLevels[[which(rmLabels %in% sepPlotsName)]]
                }
                
                array <- self$results$descPlots

                for (level in sepPlotsLevels)
                    array$addItem(level)
            }
            
            spher <- self$results$get('assump')$get('spher')
            for (term in self$options$rmTerms)
                spher$addRow(rowKey=term, list(name=stringifyTerm(term)))
            
            levene <- self$results$get('assump')$get('eqVar')
            rmVars <- sapply(self$options$rmCells, function(x) return(x$measure))
            for (var in rmVars)
                levene$addRow(rowKey=var, list(name=var))
            
            private$.initPostHoc()
                
        },
        .initPostHoc=function() {
            
            bs <- self$options$bs
            rm <- self$options$rm
            phTerms <- self$options$postHoc
            
            bsLevels <- list()
            for (i in seq_along(bs))
                bsLevels[[bs[i]]] <- levels(self$data[[bs[i]]])
            
            rmVars <- sapply(rm, function(x) return(x$label))
            rmLevels <- list()
            for (i in seq_along(rmVars))
                rmLevels[[rmVars[i]]] <- rm[[i]]$levels
            
            allLevels <- c(bsLevels, rmLevels)
            tables <- self$results$postHoc
            
            postHocRows <- list()
            
            for (ph in phTerms) {
                
                table <- tables$get(key=ph)
                
                table$setTitle(paste0("Post Hoc Comparisons - ", stringifyTerm(ph)))
                
                for (i in seq_along(ph))
                    table$addColumn(name=paste0(ph[i],"1"), title=ph[i], type='text', superTitle='Comparison', combineBelow=TRUE)
                
                table$addColumn(name="sep", title="", type='text', superTitle='Comparison')
                
                for (i in seq_along(ph))
                    table$addColumn(name=paste0(ph[i],"2"), title=ph[i], type='text', superTitle='Comparison')
                
                table$addColumn(name="md", title="Mean Difference", type="number")
                table$addColumn(name="se", title="SE", type="number")
                table$addColumn(name="df", title="df", type="number")
                table$addColumn(name="t", title="t", type="number")

                if (self$options$corrTukey)
                    table$addColumn(name="ptukey", title="p<sub>tukey</p>", type="number", format="zto,pvalue")
                if (self$options$corrScheffe)
                    table$addColumn(name="pscheffe", title="p<sub>sheffe</p>", type="number", format="zto,pvalue")
                if (self$options$corrBonf)
                    table$addColumn(name="pbonferroni", title="p<sub>bonferroni</p>", type="number", format="zto,pvalue")
                if (self$options$corrHolm)
                    table$addColumn(name="pholm", title="p<sub>holm</p>", type="number", format="zto,pvalue")
                
                combin <- expand.grid(allLevels[rev(ph)])
                combin <- sapply(combin, as.character, simplify = "matrix")
                if (length(ph) > 1)
                    combin <- combin[,rev(1:length(combin[1,]))]

                comp <- list()
                iter <- 1
                for (i in 1:(length(combin[,1]) - 1)) {
                    for (j in (i+1):length(combin[,1])) {
                        comp[[iter]] <- list()
                        comp[[iter]][[1]] <- combin[i,]
                        comp[[iter]][[2]] <- combin[j,]
                        
                        if (j == length(combin[,1]))
                            comp[[iter]][[3]] <- TRUE
                        else 
                            comp[[iter]][[3]] <- FALSE
                        
                        iter <- iter + 1
                    }
                }
                
                postHocRows[[composeTerm(ph)]] <- comp
                
                for (i in seq_along(comp)) {
                    row <- list()
                    for (c in seq_along(comp[[i]][[1]]))
                        row[[paste0(names(comp[[i]][[1]][c]),"1")]] <- as.character(comp[[i]][[1]][c])
                    row[["sep"]] <- "–"
                    for (c in seq_along(comp[[i]][[2]]))
                        row[[paste0(names(comp[[i]][[2]][c]),"2")]] <- as.character(comp[[i]][[2]][c])
                    
                    table$setRow(rowNo=i, row)
                    if (comp[[i]][[3]] == TRUE)
                        table$addFormat(rowNo=i, col=1, Cell.END_GROUP)
                }
            }
            private$.postHocRows <- postHocRows
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
            
            rmVars <- sapply(self$options$rmCells, function(x) return(x$measure)) 
            bsVars <- self$options$bs
            covVars <- self$options$cov
            
            labels <- sapply(self$options$rm, function(x) return(x$label))
            levels <- lapply(self$options$rm, function(x) return(x$levels))
            rmCells <- lapply(self$options$rmCells, function(x) return(x$cell))
            
            data <- list()
            for (var in c(rmVars, covVars))
                data[[var]] <- jmvcore::toNumeric(self$data[[var]])
            
            for (var in bsVars)
                data[[var]] <- self$data[[var]]
            
            attr(data, 'row.names') <- seq_len(length(data[[1]]))
            attr(data, 'class') <- 'data.frame'
            data <- jmvcore::naOmit(data)
            
            data <- cbind(data, ".SUBJECT"=1:nrow(data))
            
            data_long <- as.list(reshape2::melt(data, id.vars=c(bsVars, covVars, ".SUBJECT"), measure.vars=rmVars, value.name=".DEPENDENT"))
            
            col <- data_long[["variable"]]
            temp <- numeric(length(col))
            for (j in seq_along(col))
                temp[j] <- which(rmVars %in% col[j])
            
            for (i in seq_along(labels)) {
                data_long[[labels[[i]]]] <- factor(sapply(rmCells[temp], function(x) x[i]))
                levels(data_long[[labels[[i]]]]) <- levels[[i]]
            }
            data_long[["variable"]] <- NULL
            
            data_long <- lapply(data_long, function(x) {
                if (is.factor(x)) 
                    levels(x) <- toB64(levels(x))
                return(x)
            })
            
            attr(data_long, 'row.names') <- seq_len(length(data_long[[1]]))
            attr(data_long, 'names') <- toB64(names(data_long))
            attr(data_long, 'class') <- 'data.frame'
            data_long <- jmvcore::naOmit(data_long)
            
            return(data_long)
        },
        .modelFormula=function() {
            
            if (is.null(self$options$rmTerms)) {
                
                
                
            } else {
                
                bsTerms <- lapply(self$options$bsTerms, function(x) toB64(x))
                rmTerms <- lapply(self$options$rmTerms, function(x) toB64(x))
                
                bsItems <- composeTerms(bsTerms)
                bsTerm <- paste0("(", paste0(bsItems, collapse = " + "), ")")
                
                rmItems <- composeTerms(rmTerms)
                rmTerm <- paste0("Error(", paste0(toB64(".SUBJECT"),"/(", rmItems, ")", collapse=" + "),")")
                
                allTerms <- c(bsTerms, rmTerms)
                for (term1 in rmTerms) {
                    for (term2 in bsTerms) {
                        allTerms[[length(allTerms) + 1]] <- unlist(c(term1, term2))
                    }
                }
                
                allItems <- composeTerms(allTerms)
                mainTerm <- paste0("(", paste0(allItems, collapse = " + "), ")")
                
                if (length(self$options$bsTerms) == 0) {
                    formula <- as.formula(paste(toB64(".DEPENDENT"), "~", paste(mainTerm, rmTerm, sep=" + ")))
                } else {
                    formula <- as.formula(paste(toB64(".DEPENDENT"), "~", paste(mainTerm, rmTerm, bsTerm, sep=" + ")))
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

            rmRowKeys <- rmTable$rowKeys
            bsRowKeys <- bsTable$rowKeys
            
            resultRows <- jmvcore::decomposeTerms(as.list(rownames(model)))
            
            for (i in seq_along(resultRows)) {
                
                rmIndex <- which(sapply(lapply(rmRowKeys, toB64), function(x) setequal(resultRows[[i]], x)))
                bsIndex <- which(sapply(lapply(bsRowKeys, toB64), function(x) setequal(resultRows[[i]], x)))
                
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
                index <- which(sapply(lapply(as.list(resultRows), fromB64), function(x) setequal(x, resid)))
                
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
        },
        .populateSpher=function(result) {
            
            spher <- self$results$get('assump')$get('spher')
            summaryResult <- summary(result)
            epsilon <- summaryResult$pval.adjustments
            mauchly <- summaryResult$sphericity.tests
            
            nLevels <- sapply(self$options$rm, function(x) return(length(x$levels)))
            
            if (any(nLevels > 2)) {
                
                resultRows <- decomposeTerms(rownames(mauchly))
                
                for (term in self$options$rmTerms) {
                    
                    index <- which(sapply(as.list(resultRows), function(x) setequal(x, toB64(term))))
                    
                    if (length(index) == 0) {
                        
                        spher$setRow(rowKey=term, values=list("mauch"=1, "p"=NaN, "gg"=1, "hf"=1))
                        spher$addFootnote(rowKey=term, "name", "The repeated measures has only two levels. The assumption of sphericity is always met when the repeated measures has only two levels")
                        
                    } else {
                        
                        row <- list()
                        row[["mauch"]] <- mauchly[index,"Test statistic"]
                        row[["p"]] <- mauchly[index,"p-value"]
                        row[["gg"]] <- epsilon[index, "GG eps"]
                        row[["hf"]] <- if (epsilon[index, "HF eps"] > 1) 1 else epsilon[index, "HF eps"]
                        
                        spher$setRow(rowKey=term, values=row)
                    }
                }
            } else {
                
                for (term in self$options$rmTerms) {
                    
                    spher$setRow(rowKey=term, values=list("mauch"=1, "p"=NaN, "gg"=1, "hf"=1))
                    spher$addFootnote(rowKey=term, "name", "The repeated measures has only two levels. The assumption of sphericity is always met when the repeated measures has only two levels")
                }
            }
        },
        .levene=function () {
            
            # leveneTable <- self$results$get('assump')$get('eqVar')
            # rmVars <- sapply(self$options$rmCells, function(x) return(x$measure))
            # bsTerm <- paste0(composeTerms(self$options$bsTerms), collapse = "+")
            # 
            # for (var in rmVars) {
            #     
            #     formula <- as.formula(paste0(composeTerm(var),"~", bsTerm)
            #     
            #     myaov_t1 <- aov(formula, data=self$data)
            #     summary(aov(abs(myaov_t1$residuals) ~ Group, data=subset(ldf, Time==1)))}
            #     
            # }
            #     levene$addRow(rowKey=var, list(name=var))
            # 
            # 
            # myaov_t1 <- aov(DV ~ Group + Cov, data=subset(ldf, Time==1))
            # summary(aov(abs(myaov_t1$residuals) ~ Group, data=subset(ldf, Time==1)))}
            
        },
        .postHocTest=function (result) {
            
            terms <- self$options$postHoc
            
            if (length(terms) == 0)
                return()
            
            tables <- self$results$postHoc
            
            postHocRows <- list()
            
            for (ph in terms) {
                
                table <- tables$get(key=ph)
                
                term <- jmvcore::composeTerm(ph)
                termB64 <- jmvcore::composeTerm(toB64(ph))
                
                formula <- as.formula(paste("~", termB64))
                
                suppressWarnings({
                    
                    referenceGrid <- lsmeans::lsmeans(result, formula)
                    tukey <- summary(pairs(referenceGrid, adjust="tukey"))

                    if (self$options$corrScheffe)
                        scheffe <- summary(pairs(referenceGrid, adjust="scheffe"))
                    if (self$options$corrBonf)
                        bonferroni <- summary(pairs(referenceGrid, adjust="bonferroni"))
                    if (self$options$corrHolm)
                        holm <- summary(pairs(referenceGrid, adjust="holm"))

                }) # suppressWarnings
                
                resultRows <- lapply(strsplit(as.character(tukey$contrast), " - "), function(x) strsplit(x, ","))
                tableRows <- private$.postHocRows[[term]]
                
                for (i in seq_along(tableRows)) {
                    
                    index <- which(sapply(resultRows, function(x) {
                        c1 <- setequal(x[[1]], toB64(tableRows[[i]][[1]]))
                        c2 <- setequal(x[[1]], toB64(tableRows[[i]][[2]]))
                        c3 <- setequal(x[[2]], toB64(tableRows[[i]][[1]]))
                        c4 <- setequal(x[[2]], toB64(tableRows[[i]][[2]]))
                        
                        if ((c1 && c4) || (c2 && c3))
                            return(TRUE)
                        else 
                            return(FALSE)
                    }))
                    
                    row <- list()
                    row[["md"]] <- tukey[index,"estimate"]
                    row[["se"]] <- tukey[index,"SE"]
                    row[["df"]] <- tukey[index,"df"]
                    row[["t"]] <- tukey[index,"t.ratio"]
                    
                    if (self$options$corrTukey)
                        row[["ptukey"]] <- tukey[index,"p.value"]
                    if (self$options$corrScheffe)
                        row[["pscheffe"]] <- scheffe[index,"p.value"]
                    if (self$options$corrBonf)
                        row[["pbonferroni"]] <- bonferroni[index,"p.value"]
                    if (self$options$corrHolm)
                        row[["pholm"]] <- holm[index,"p.value"]
                    
                    table$setRow(rowNo=i, values=row)
                    private$.checkpoint()
                }
            }
        },
        .prepareDescPlots=function(data) {
            
            depName <- ".DEPENDENT"
            groupName <- self$options$descPlotsHAxis
            linesName <- self$options$descPlotsSepLines
            plotsName <- self$options$descPlotsSepPlots
            
            ciWidth   <- self$options$ciWidth
            errorBarType <- self$options$errBarDef
            
            # rm <- sapply(self$options$rm, function(x) return(x$label))
            # bs <- self$options$bs
            # 
            # rmVars <- c()
            # bsVars <- c()
            # if (groupName %in% rm)
            #     rmVars[length(rmVars)+1] <- groupName
            # else
            #     bsVars[length(bsVars)+1] <- groupName
            #     
            # 
            # if (length(rm) == 0)
            #     summaryStat <- .summarySE(data, measurevar = "dependent", groupvars = bs,
            #                               conf.interval = ciWidth, na.rm = TRUE, .drop = FALSE, errorBarType = errorBarType)
            # else
            #     summaryStat <- .summarySEwithin(data, measurevar="dependent", betweenvars=bs, withinvars=rm,
            #                                     idvar="subject", conf.interval=ciWidth, na.rm=TRUE, .drop=FALSE, errorBarType=errorBarType)
            # 
            if (length(depName) == 0 || length(groupName) == 0)
                return()
            
            by <- list()
            by[['group']] <- data[[toB64(groupName)]]
            levels(by[['group']]) <- fromB64(levels(by[['group']]))
            
            if ( ! is.null(linesName)) {
                by[['lines']] <- data[[toB64(linesName)]]
                levels(by[['lines']]) <- fromB64(levels(by[['lines']]))
            }
            
            if ( ! is.null(plotsName)) {
                by[['plots']] <- data[[toB64(plotsName)]]
                levels(by[['plots']]) <- fromB64(levels(by[['plots']]))
            }
                
            
            dep <- data[[toB64(depName)]]
            
            ciMult <- qt(ciWidth / 200 + .5, nrow(data)-1)
            
            means <- aggregate(dep, by=by, mean, simplify=FALSE)
            ses   <- aggregate(dep, by=by, function(x) { sd(x) / sqrt(length(x)) }, simplify=FALSE)
            cis   <- aggregate(dep, by=by, function(x) { sd(x) / sqrt(length(x)) * ciMult }, simplify=FALSE)
            
            plotData <- data.frame(group=means$group)
            if ( ! is.null(linesName))
                plotData <- cbind(plotData, lines=means$lines)
            if ( ! is.null(plotsName))
                plotData <- cbind(plotData, plots=means$plots)
            
            plotData <- cbind(plotData, mean=unlist(means$x))
            
            if (errorBarType == 'ci')
                plotData <- cbind(plotData, err=unlist(cis$x))
            else
                plotData <- cbind(plotData, err=unlist(ses$x))
            
            plotData <- cbind(plotData, lower=plotData$mean-plotData$err, upper=plotData$mean+plotData$err)
            
            if (self$options$dispErrBars) {
                yAxisRange <- pretty(c(plotData$lower, plotData$upper))
            } else {
                yAxisRange <- plotData$mean
            }
            
            if (is.null(plotsName)) {
                
                image <- self$results$get('descPlot')
                image$setState(list(data=plotData, range=yAxisRange))
                
            } else {
                
                images <- self$results$descPlots
                for (level in images$itemKeys) {
                    image <- images$get(key=level)
                    image$setState(list(data=subset(plotData, plots == level), range=yAxisRange))
                }
            }
        },
        .descPlot=function(image, ...) {
            
            if (is.null(image$state))
                return(FALSE)
            
            depName <- "dependent"
            groupName <- self$options$descPlotsHAxis
            linesName <- self$options$descPlotsSepLines
            plotsName <- self$options$descPlotsSepPlots
            
            the <- theme(
                text=element_text(size=16, colour='#333333'),
                plot.background=element_rect(fill='transparent', color=NA),
                legend.background=element_rect(fill='transparent', colour=NA),
                panel.background=element_rect(fill='#E8E8E8'),
                plot.margin=margin(15, 15, 15, 15),
                axis.text.x=element_text(margin=margin(5,0,0,0)),
                axis.text.y=element_text(margin=margin(0,5,0,0)),
                axis.title.x=element_text(margin=margin(10,0,0,0)),
                axis.title.y=element_text(margin=margin(0,10,0,0)))
            
            dodge <- position_dodge(0.2)
            
            
            errorType <- ''
            if (self$options$dispErrBars) {
                if (self$options$errBarDef == 'ci') {
                    ciWidth <- self$options$ciWidth
                    errorType <- paste0('(', ciWidth, '% CI)')
                } else {
                    errorType <- '(SE)'
                }
            }
            
            if ( ! is.null(linesName)) {
                
                p <- ggplot(data=image$state$data, aes(x=group, y=mean, group=lines, colour=lines)) +
                    geom_line(size=.8, position=dodge) +
                    geom_point(shape=21, fill='white', size=3, position=dodge) +
                    labs(x=groupName, y=depName, colour=paste(linesName, errorType)) +
                    scale_y_continuous(limits=c(min(image$state$range), max(image$state$range))) +
                    the
                
                if (self$options$dispErrBars)
                    p <- p + geom_errorbar(aes(x=group, ymin=lower, ymax=upper, width=.1, group=lines), size=.8, position=dodge)
                
                print(p)
                
            } else {
                
                p <- ggplot(data=image$state$data) +
                    geom_point(aes(x=group, y=mean, colour='colour'), shape=21, fill='white', size=3) +
                    labs(x=groupName, y=depName, colour=paste(depName, errorType)) +
                    scale_colour_manual(name=paste(depName, errorType), values=c(colour='#333333'), labels='') +
                    scale_y_continuous(limits=c(min(image$state$range), max(image$state$range))) +
                    the
                
                if (self$options$dispErrBars)
                    p <- p + geom_errorbar(aes(x=group, ymin=lower, ymax=upper, colour='colour', width=.1), size=.8)
                
                print(p)
            }
            
            TRUE
        },
        .summarySE=function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE, errorBarType="confidenceInterval") {
            
            # New version of length which can handle NA's: if na.rm==T, don't count them
            length2 <- function (x, na.rm=FALSE) {
                if (na.rm) {
                    sum(!is.na(x))
                } else {
                    length(x)
                }
            }
            
            # This does the summary. For each group's data frame, return a vector with
            # N, mean, and sd
            datac <- plyr::ddply(data, groupvars, .drop=.drop,
                                 .fun = function(xx, col) {
                                     c(N    = length2(xx[[col]], na.rm=na.rm),
                                       mean = mean   (xx[[col]], na.rm=na.rm),
                                       sd   = sd     (xx[[col]], na.rm=na.rm)
                                     )
                                 },
                                 measurevar
            )
            
            # Rename the "mean" column
            datac <- plyr::rename(datac, c("mean" = measurevar))
            
            datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
            
            # Confidence interval multiplier for standard error
            # Calculate t-statistic for confidence interval:
            # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
            ciMult <- qt(conf.interval/2 + .5, datac$N-1)
            datac$ci <- datac$se * ciMult
            
            if (errorBarType == "confidenceInterval") {
                
                datac$ciLower <- datac[,measurevar] - datac[,"ci"]
                datac$ciUpper <- datac[,measurevar] + datac[,"ci"]
                
            } else {
                
                datac$ciLower <- datac[,measurevar] - datac[,"se"]
                datac$ciUpper <- datac[,measurevar] + datac[,"se"]
                
            }
            
            return(datac)
        },
        .normDataWithin=function(data=NULL, idvar, measurevar, betweenvars=NULL, na.rm=FALSE, .drop=TRUE) {
            
            # Measure var on left, idvar + between vars on right of formula.
            data.subjMean <- plyr::ddply(data, c(idvar, betweenvars), .drop=.drop,
                                         .fun = function(xx, col, na.rm) {
                                             c(subjMean = mean(xx[,col], na.rm=na.rm))
                                         },
                                         measurevar,
                                         na.rm
            )
            
            
            
            # Put the subject means with original data
            data <- base::merge(data, data.subjMean)
            
            # Get the normalized data in a new column
            measureNormedVar <- paste(measurevar, "_norm", sep="")
            data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
                mean(data[,measurevar], na.rm=na.rm)
            
            # Remove this subject mean column
            data$subjMean <- NULL
            
            return(data)
        },
        .summarySEwithin=function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL, idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE, errorBarType="confidenceInterval") {
            
            # Get the means from the un-normed data
            datac <- .summarySE(data, measurevar, groupvars=c(betweenvars, withinvars), na.rm=na.rm, conf.interval=conf.interval, .drop=.drop, errorBarType=errorBarType)
            
            # Drop all the unused columns (these will be calculated with normed data)
            datac$sd <- NULL
            datac$se <- NULL
            datac$ci <- NULL
            datac$ciLower <- NULL
            datac$ciUpper <- NULL
            
            # Norm each subject's data
            ndata <- .normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
            
            # This is the name of the new column
            measurevar_n <- paste(measurevar, "_norm", sep="")
            
            # Collapse the normed data - now we can treat between and within vars the same
            ndatac <- .summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars), na.rm=na.rm, conf.interval=conf.interval, .drop=.drop, errorBarType=errorBarType)
            
            # Apply correction from Morey (2008) to the standard error and confidence interval
            # Get the product of the number of conditions of within-S variables
            nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels, FUN.VALUE=numeric(1)))
            correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
            
            # Apply the correction factor
            ndatac$sd <- ndatac$sd * correctionFactor
            ndatac$se <- ndatac$se * correctionFactor
            ndatac$ci <- ndatac$ci * correctionFactor
            
            if (errorBarType == "confidenceInterval") {
                
                ndatac$ciLower <- datac[,measurevar] - ndatac[,"ci"]
                ndatac$ciUpper <- datac[,measurevar] + ndatac[,"ci"]
                
            } else {
                
                ndatac$ciLower <- datac[,measurevar] - ndatac[,"se"]
                ndatac$ciUpper <- datac[,measurevar] + ndatac[,"se"]
                
            }
            
            # Combine the un-normed means with the normed results
            merge(datac, ndatac)
        })
)

