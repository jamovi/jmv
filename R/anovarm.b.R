
#' @importFrom jmvcore .
anovaRMClass <- R6::R6Class(
    "anovaRMClass",
    inherit=anovaRMBase,
    #### Active bindings ----
    active = list(
        rmTerms = function() {
            if (is.null(private$.rmTerms))
                private$.rmTerms <- private$.getRmTerms()

            return(private$.rmTerms)
        },
        bsTerms = function() {
            if (is.null(private$.bsTerms))
                private$.bsTerms <- private$.getBsTerms()

            return(private$.bsTerms)
        }
    ),
    private = list(
        #### Member variables ----
        .rmTerms = NULL,
        .bsTerms = NULL,
        .model = NULL,
        .postHocRows = NULL,
        emMeans = list(),

        #### Init + run functions ----
        .init=function() {
            private$.initRMTable()
            private$.initBSTable()
            private$.initSpericityTable()
            private$.initLeveneTable()
            private$.initPostHocTables()
            private$.initEmm()
            private$.initEmmTable()
            private$.initGroupSummary()

            measures <- lapply(self$options$rmCells, function(x) x$measure)
            areNull  <- vapply(measures, is.null, FALSE, USE.NAMES=FALSE)

            if (any(areNull))
                self$setStatus('complete')
        },
        .run=function() {
            dataSelected <- ! sapply(lapply(self$options$rmCells, function(x) return(x$measure)), is.null)

            ready <- sum(dataSelected) == length(self$options$rmCells) && length(self$rmTerms) > 0

            if (ready) {

                private$.dataCheck()
                data <- private$.wideToLong()
                modelFormula <- private$.modelFormula()

                suppressMessages({
                    suppressWarnings({
                        result <- try(
                            afex::aov_car(
                                modelFormula,
                                data,
                                type=self$options$ss,
                                factorize = FALSE
                            ),
                            silent=TRUE
                        )
                    }) # suppressWarnings
                }) # suppressMessages

                if (isError(result))
                    jmvcore::reject(extractErrorMessage(result), code='error')

                private$.populateEffectsTables(result)
                private$.populateSpericityTable(result)
                private$.populateLeveneTable()
                private$.prepareQQPlot(result)

                private$.populatePostHocTables(result)

                private$.prepareEmmPlots(result, data)
                private$.populateEmmTables()
                private$.populateGroupSummaryTable()
            }
        },

        #### Init tables/plots functions ----
        .initRMTable=function() {
            rmTable <- self$results$rmTable

            rmTable$setNote(
                'Note',
                jmvcore::format(
                    .("Type {ssType} Sums of Squares"),
                    ssType=self$options$ss
                )
            )

            rm <- private$.rmTableRowLabels()
            rmTerms <- rm$terms
            rmSpacing <- rm$spacing

            if (length(rmTerms) > 0) {
                for (i in seq_along(rmTerms)) {
                    if (rmTerms[i] == 'Residual') {
                        key <- unlist(c(rmTerms[[i-1]],'.RES'))
                        name <- .("Residual")
                    } else {
                        key <- unlist(rmTerms[[i]])
                        name <- stringifyTerm(rmTerms[[i]])
                    }
                    values <- list(
                        `name[none]`=name,
                        `name[GG]`=name,
                        `name[HF]`=name
                    )
                    rmTable$addRow(rowKey=key, values)
                }
            } else {
                name <- '.'
                values <- list(
                    `name[none]`=name,
                    `name[GG]`=name,
                    `name[HF]`=name
                )
                rmTable$addRow(rowKey='.', values)
                rmTable$addRow(rowKey='', list(name=.('Residual')))
            }

            for (i in seq_along(rmSpacing)) {
                if ( ! is.null(rmSpacing[[i]])) {
                    if (rmSpacing[[i]] == 'both')
                        rmTable$addFormat(rowNo=i, col=1, Cell.BEGIN_END_GROUP)
                    else if (rmSpacing[[i]] == 'above')
                        rmTable$addFormat(rowNo=i, col=1, Cell.BEGIN_GROUP)
                    else if (rmSpacing[[i]] == 'below')
                        rmTable$addFormat(rowNo=i, col=1, Cell.END_GROUP)
                }
            }
        },
        .initBSTable=function() {
            bsTable <- self$results$bsTable

            bsTable$setNote(
                'Note',
                jmvcore::format(
                    .("Type {ssType} Sums of Squares"),
                    ssType=self$options$ss
                )
            )

            bsTerms <- c(self$bsTerms, 'Residual')

            if (length(bsTerms) > 0) {
                for (term in bsTerms) {
                    if (length(term) == 1 && term == 'Residual') {
                        name <- .('Residual')
                    } else {
                        name <- stringifyTerm(term)
                    }
                    bsTable$addRow(rowKey=unlist(term), list(name=name))
                }
            } else {
                bsTable$addRow(rowKey='', list(name=.('Residual')))
            }
        },
        .initSpericityTable=function() {
            spherTable <- self$results$get('assump')$get('spherTable')
            for (term in self$rmTerms)
                spherTable$addRow(rowKey=term, list(name=stringifyTerm(term)))
        },
        .initLeveneTable=function() {
            leveneTable <- self$results$get('assump')$get('leveneTable')
            rmVars <- sapply(self$options$rmCells, function(x) return(x$measure))
            for (var in rmVars)
                leveneTable$addRow(rowKey=var, list(name=var))
        },
        .initPostHocTables=function() {

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
            postHocTableTitle <- .('Post Hoc Comparisons - {term}')

            for (ph in phTerms) {

                table <- tables$get(key=ph)

                table$setTitle(jmvcore::format(postHocTableTitle, term=stringifyTerm(ph)))

                for (i in seq_along(ph))
                    table$addColumn(name=paste0(ph[i],'1'), title=ph[i], type='text', superTitle=.('Comparison'), combineBelow=TRUE)

                table$addColumn(name='sep', title='', type='text', content='-', superTitle=.('Comparison'), format='narrow')

                for (i in seq_along(ph))
                    table$addColumn(name=paste0(ph[i],'2'), title=ph[i], type='text', superTitle=.('Comparison'))

                table$addColumn(name='md', title=.('Mean Difference'), type='number')
                table$addColumn(name='se', title='SE', type='number')
                table$addColumn(name='df', title='df', type='number')
                table$addColumn(name='t', title='t', type='number')

                table$addColumn(name='pnone', title='p', type='number', format='zto,pvalue', visible="(postHocCorr:none)")
                table$addColumn(name='ptukey', title='p<sub>tukey</sub>', type='number', format='zto,pvalue', visible="(postHocCorr:tukey)")
                table$addColumn(name='pscheffe', title='p<sub>scheffe</sub>', type='number', format='zto,pvalue', visible="(postHocCorr:scheffe)")
                table$addColumn(name='pbonferroni', title='p<sub>bonferroni</sub>', type='number', format='zto,pvalue', visible="(postHocCorr:bonf)")
                table$addColumn(name='pholm', title='p<sub>holm</sub>', type='number', format='zto,pvalue', visible="(postHocCorr:holm)")

                combin <- expand.grid(allLevels[rev(ph)])
                combin <- sapply(combin, as.character, simplify = 'matrix')
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
                        row[[paste0(names(comp[[i]][[1]][c]),'1')]] <- as.character(comp[[i]][[1]][c])
                    for (c in seq_along(comp[[i]][[2]]))
                        row[[paste0(names(comp[[i]][[2]][c]),'2')]] <- as.character(comp[[i]][[2]][c])

                    table$addRow(rowKey=i, row)
                    if (comp[[i]][[3]] == TRUE)
                        table$addFormat(rowNo=i, col=1, Cell.END_GROUP)
                }
            }
            private$.postHocRows <- postHocRows
        },
        .initEmm = function() {

            emMeans <- self$options$emMeans
            group <- self$results$emm

            for (j in seq_along(emMeans)) {

                emm <- emMeans[[j]]

                if ( ! is.null(emm)) {
                    group$addItem(key=j)
                    emmGroup <- group$get(key=j)
                    emmGroup$setTitle(jmvcore::stringifyTerm(emm))

                    image <- emmGroup$emmPlot
                    size <- private$.emmPlotSize(emm)
                    image$setSize(size[1], size[2])
                }
            }
        },
        .initEmmTable = function() {

            emMeans <- self$options$emMeans
            rmFactors <- self$options$rm

            rmNames <- sapply(rmFactors, function(x) return(x$label))
            rmLevels <- lapply(rmFactors, function(x) return(x$levels))

            group <- self$results$emm

            emMeansTableTitle <- .('Estimated Marginal Means - {term}')
            ciWidthTitle <- jmvcore::format(.('{ciWidth}% Confidence Interval'), ciWidth=self$options$ciWidthEmm)

            for (j in seq_along(emMeans)) {

                emm <- emMeans[[j]]

                if ( ! is.null(emm)) {

                    emmGroup <- group$get(key=j)

                    table <- emmGroup$emmTable
                    table$setTitle(jmvcore::format(emMeansTableTitle, term=jmvcore::stringifyTerm(emm)))

                    nLevels <- numeric(length(emm))
                    for (k in rev(seq_along(emm))) {
                        table$addColumn(name=emm[k], title=emm[k], type='text', combineBelow=TRUE)

                        if (emm[k] %in% rmNames) {
                            nLevels[k] <- length(rmLevels[[which(emm[k] == rmNames)]])
                        } else {
                            nLevels[k] <- length(levels(self$data[[ emm[k] ]]))
                        }
                    }

                    table$addColumn(name='mean', title=.('Mean'), type='number')
                    table$addColumn(name='se', title='SE', type='number')
                    table$addColumn(name='lower', title='Lower', type='number', superTitle=ciWidthTitle)
                    table$addColumn(name='upper', title='Upper', type='number', superTitle=ciWidthTitle)

                    nRows <- prod(nLevels)

                    for (k in 1:nRows) {
                        row <- list()
                        table$addRow(rowKey=k, row)
                    }
                }
            }
        },

        .initGroupSummary = function() {

            table <- self$results$groupSummary
            bs <- self$options$bs

            bs <- lapply(bs, function(x) self$data[[x]])
            levels <- lapply(bs, levels)
            groups <- expand.grid(levels)
            if (nrow(groups) == 0) {
                groups <- data.frame(x='')
                colnames(groups) <- ''
            } else {
                colnames(groups) = self$options$bs
            }

            titles = colnames(groups)
            names = paste0('group:', titles)

            for (i in seq_len(ncol(groups))) {
                table$addColumn(
                    index=1,
                    name=names[i],
                    title=titles[i],
                    type='text',
                    combineBelow=TRUE
                )
            }

            for (i in seq_len(nrow(groups))) {
                values <- apply(groups[i,,drop=FALSE], 2, paste)
                names(values) <- names
                table$addRow(rowKey=unname(values), values=values)
            }
        },

        #### Populate tables functions ----
        .populateEffectsTables=function(result) {

            rmTable <- self$results$get('rmTable')
            bsTable <- self$results$get('bsTable')

            suppressWarnings({
                summaryResult <- summary(result)
            })
            model <- summaryResult$univariate.tests
            epsilon <- summaryResult$pval.adjustments
            ges <- result$anova_table

            rmRows <- rmTable$rowKeys
            bsRows <- bsTable$rowKeys
            modelRows <- jmvcore::decomposeTerms(as.list(rownames(model)))
            epsilonRows <- jmvcore::decomposeTerms(as.list(rownames(epsilon)))
            gesRows <- jmvcore::decomposeTerms(as.list(rownames(ges)))

            SSt <- private$.getSSt(model)

            # Populate RM table
            for (i in seq_along(rmRows)) {

                if (! '.RES' %in% rmRows[[i]]) { # if the row is not a residual

                    index <- which(sapply(modelRows, function(x) setequal(toB64(rmRows[[i]]), x)))

                    row <- list()
                    row[['ss[none]']] <- row[['ss[GG]']] <- row[['ss[HF]']] <- model[index,'Sum Sq']
                    row[['F[none]']] <- row[['F[GG]']] <- row[['F[HF]']] <- model[index,'F value']

                    row[['df[none]']] <- model[index,'num Df']
                    row[['ms[none]']] <- row[['ss[none]']] / row[['df[none]']]
                    row[['p[none]']] <- model[index,'Pr(>F)']

                    # Add sphericity corrected values
                    indexEps <- which(sapply(epsilonRows, function(x) setequal(toB64(rmRows[[i]]), x)))
                    dfRes <- model[index,'den Df']

                    if (length(indexEps) == 0) {
                        GG <- 1
                        HF <- 1
                    } else {
                        GG <- if (is.na(epsilon[indexEps,'GG eps'])) 1 else epsilon[indexEps,'GG eps']
                        HF <- if (is.na(epsilon[indexEps,'HF eps']) || epsilon[indexEps,'HF eps'] > 1) 1 else epsilon[indexEps,'HF eps']
                    }

                    row[['df[GG]']] <- row[['df[none]']] * GG
                    row[['ms[GG]']] <- row[['ss[GG]']] / row[['df[GG]']]
                    dfResGG <- dfRes * GG
                    row[['p[GG]']] <- pf(row[['F[GG]']], row[['df[GG]']], dfResGG, lower.tail=FALSE)

                    row[['df[HF]']] <- row[['df[none]']] * HF
                    row[['ms[HF]']] <- row[['ss[HF]']] / row[['df[HF]']]
                    dfResHF <- dfRes * HF
                    row[['p[HF]']] <- pf(row[['F[HF]']], row[['df[HF]']], dfResHF, lower.tail=FALSE)

                    gesIndex <- which(sapply(gesRows, function(x) setequal(toB64(rmRows[[i]]), x)))
                    gesValue <- ges[gesIndex, 'ges']

                    # Add effect sizes
                    SSr <- model[index,'Error SS']
                    MSr <- SSr/dfRes

                    row[['eta[none]']] <- row[['eta[GG]']] <- row[['eta[HF]']] <- row[['ss[none]']] / SSt
                    row[['ges[none]']] <- row[['ges[GG]']] <- row[['ges[HF]']] <- gesValue
                    row[['partEta[none]']] <- row[['partEta[GG]']] <- row[['partEta[HF]']] <- row[['ss[none]']] / (row[['ss[none]']] + SSr)

                    omega <- (row[['ss[none]']] - (row[['df[none]']] * MSr)) / (SSt + MSr)

                    row[['omega[none]']] <- row[['omega[GG]']] <- row[['omega[HF]']] <- if ( ! is.na(omega) && omega < 0) 0 else omega

                    rmTable$setRow(rowNo=i, values=row)

                } else { # if the row is a residual

                    term <- rmRows[[i]][-length(rmRows[[i]])]
                    index <- which(sapply(modelRows, function(x) setequal(toB64(term), x)))

                    row <- list()
                    row[['ss[none]']] <- row[['ss[GG]']] <- row[['ss[HF]']] <- model[index,'Error SS']
                    row[['df[none]']] <- model[index,'den Df']
                    row[['ms[none]']] <- row[['ss[none]']] / row[['df[none]']]
                    row[['F[none]']] <- row[['F[GG]']]  <- row[['F[HF]']] <- ''
                    row[['p[none]']] <- row[['p[GG]']] <- row[['p[HF]']] <- ''
                    row[['ges[none]']] <- row[['ges[GG]']] <- row[['ges[HF]']] <- ''
                    row[['eta[none]']] <- row[['eta[GG]']] <- row[['eta[HF]']] <- ''
                    row[['partEta[none]']] <- row[['partEta[GG]']] <- row[['partEta[HF]']] <- ''
                    row[['omega[none]']] <- row[['omega[GG]']] <- row[['omega[HF]']] <- ''

                    # Add sphericity corrected values
                    indexEps <- which(sapply(epsilonRows, function(x) setequal(toB64(term), x)))
                    dfRes <- model[index,'den Df']

                    if (length(indexEps) == 0) {
                        GG <- 1
                        HF <- 1
                    } else {
                        GG <- if (is.na(epsilon[indexEps,'GG eps'])) 1 else epsilon[indexEps,'GG eps']
                        HF <- if (is.na(epsilon[indexEps,'HF eps']) || epsilon[indexEps,'HF eps'] > 1) 1 else epsilon[indexEps,'HF eps']
                    }

                    row[['df[GG]']] <- row[['df[none]']] * GG
                    row[['ms[GG]']] <- row[['ss[GG]']] / row[['df[GG]']]
                    row[['df[HF]']] <- row[['df[none]']] * HF
                    row[['ms[HF]']] <- row[['ss[HF]']] / row[['df[HF]']]

                    rmTable$setRow(rowNo=i, values=row)
                }
            }

            # Populate BS table

            bsTerms <- lapply(bsRows[which( ! sapply(bsRows, function(x) x[1] == 'Residual'))], toB64)

            if (length(bsTerms) > 0)
                bsIndices <- sapply(bsTerms, function(x) which(sapply(modelRows, function(y) setequal(x,y))))

            for (i in seq_along(bsRows)) {

                if (! bsRows[[i]][1] == 'Residual') { # if the row is not a residual

                    index <- which(sapply(modelRows, function(x) setequal(toB64(bsRows[[i]]), x)))
                    gesIndex <- which(sapply(gesRows, function(x) setequal(toB64(bsRows[[i]]), x)))

                    row <- list()
                    row[['ss']] <- model[index,'Sum Sq']
                    row[['df']] <- model[index,'num Df']
                    row[['ms']] <- row[['ss']] / row[['df']]
                    row[['F']] <- model[index,'F value']
                    row[['p']] <- model[index,'Pr(>F)']

                    # Add effect sizes
                    SSr <- model[index,'Error SS']
                    MSr <- SSr/model[index,'den Df']
                    row[['ges']] <- ges[gesIndex, 'ges']
                    row[['eta']] <- row[['ss']] / SSt
                    row[['partEta']] <- row[['ss']] / (row[['ss']] + SSr)
                    omega <- (row[['ss']] - (row[['df']] * MSr)) / (SSt + MSr)
                    row[['omega']] <- if ( ! is.na(omega) && omega < 0) 0 else omega

                    bsTable$setRow(rowNo=i, values=row)

                } else { # if the row is a residual

                    row <- list()
                    row[['ss']] <- model['(Intercept)','Error SS']
                    row[['df']] <- model['(Intercept)','den Df']
                    row[['ms']] <- row[['ss']] / row[['df']]
                    row[['F']] <- row[['p']] <- row[['ges']] <- row[['eta']] <- row[['partEta']] <- row[['omega']] <-''

                    bsTable$setRow(rowNo=i, values=row)
                }
            }
        },
        .populateSpericityTable=function(result) {

            spherTable <- self$results$assump$spherTable

            summaryResult <- suppressWarnings({summary(result)})
            epsilon <- summaryResult$pval.adjustments
            mauchly <- summaryResult$sphericity.tests

            nLevels <- sapply(self$options$rm, function(x) return(length(x$levels)))
            resultRows <- decomposeTerms(rownames(mauchly))

            if (any(nLevels > 2) && length(resultRows) > 0) {

                for (term in self$rmTerms) {

                    index <- which(sapply(as.list(resultRows), function(x) setequal(x, toB64(term))))

                    if (length(index) == 0) {

                        spherTable$setRow(rowKey=term, values=list('mauch'=1, 'p'=NaN, 'gg'=1, 'hf'=1))
                        if (length(spherTable$getRow(rowKey=term)$name$footnotes) == 0)
                            spherTable$addFootnote(rowKey=term, 'p', .('The repeated measures has only two levels. The assumption of sphericity is always met when the repeated measures has only two levels.'))

                    } else {

                        row <- list()
                        row[['mauch']] <- mauchly[index,'Test statistic']
                        row[['p']] <- mauchly[index,'p-value']
                        row[['gg']] <- epsilon[index, 'GG eps']
                        row[['hf']] <- if (epsilon[index, 'HF eps'] > 1) 1 else epsilon[index, 'HF eps']

                        spherTable$setRow(rowKey=term, values=row)
                    }
                }
            } else {

                for (term in self$rmTerms) {

                    if (any(nLevels > 2)) {
                        spherTable$setRow(rowKey=term, values=list('mauch'=NaN, 'p'=NaN, 'gg'=NaN, 'hf'=NaN))
                        if (length(spherTable$getRow(rowKey=term)$name$footnotes) == 0)
                            spherTable$addFootnote(rowKey=term, 'name', .('Singularity error. Sphericity tests are not available'))

                    } else {
                        spherTable$setRow(rowKey=term, values=list('mauch'=1, 'p'=NaN, 'gg'=1, 'hf'=1))
                        if (length(spherTable$getRow(rowKey=term)$name$footnotes) == 0)
                            spherTable$addFootnote(rowKey=term, 'p', .('The repeated measures has only two levels. The assumption of sphericity is always met when the repeated measures has only two levels'))
                    }
                }
            }
        },
        .populateLeveneTable=function () {

            if (length(self$options$rmCells) == 0)
                return()

            leveneTable <- self$results$get('assump')$get('leveneTable')

            rmVars <- sapply(self$options$rmCells, function(x) return(x$measure))
            covVars <- self$options$cov
            bsVars <- self$options$bs

            if (length(bsVars) == 0) {
                for (var in rmVars) {
                    leveneTable$setRow(rowKey=var, values=list('F'=NaN, 'df1'='', 'df2'='', 'p'=''))
                    leveneTable$addFootnote(rowKey=var, 'F', .('As there are no between subjects factors specified this assumption is always met.'))
                }
                return()
            }

            data <- list()
            for (rm in c(rmVars,covVars))
                data[[rm]] <- jmvcore::toNumeric(self$data[[rm]])

            for (bs in bsVars)
                data[[bs]] <- factor(self$data[[bs]])

            attr(data, 'row.names') <- seq_len(length(data[[1]]))
            attr(data, 'class') <- 'data.frame'
            data <- jmvcore::naOmit(data)

            group <- interaction(data[bsVars])
            data <- cbind(data, .GROUP=group)

            for (var in rmVars) {

                if (length(covVars) > 0)
                    formula <- as.formula(paste0(composeTerm(var),'~ .GROUP +', paste0(composeTerm(covVars), collapse='+')))
                else
                    formula <- as.formula(paste0(composeTerm(var),'~ .GROUP'))

                res <- abs(aov(formula, data=data)$residuals)
                r <- summary(aov(res ~ group))[[1]]

                row <- list(F=r[1,'F value'], df1=r[1,'Df'], df2=r[2,'Df'], p=r[1,'Pr(>F)'])
                leveneTable$setRow(rowKey=var, values=row)
            }
        },
        .populatePostHocTables=function (result) {

            terms <- self$options$postHoc

            if (length(terms) == 0)
                return()

            tables <- self$results$postHoc

            postHocRows <- list()

            for (ph in terms) {

                table <- tables$get(key=ph)

                term <- jmvcore::composeTerm(ph)
                termB64 <- jmvcore::composeTerm(toB64(ph))

                formula <- as.formula(paste('~', termB64))

                suppressWarnings({

                    table$setStatus('running')

                    emmeans::emm_options(sep = ",", parens = "a^")
                    referenceGrid <- emmeans::emmeans(result, formula, model="multivariate")
                    none <- summary(pairs(referenceGrid, adjust='none'))
                    tukey <- summary(pairs(referenceGrid, adjust='tukey'))
                    scheffe <- summary(pairs(referenceGrid, adjust='scheffe'))
                    bonferroni <- summary(pairs(referenceGrid, adjust='bonferroni'))
                    holm <- summary(pairs(referenceGrid, adjust='holm'))

                }) # suppressWarnings

                resultRows <- lapply(strsplit(as.character(tukey$contrast), ' - '), function(x) strsplit(x, ','))
                tableRows <- private$.postHocRows[[term]]

                for (i in seq_along(tableRows)) {
                    location <- lapply(resultRows, function(x) {

                        c1 <- identical(x[[1]], toB64(as.character(tableRows[[i]][[1]])))
                        c2 <- identical(x[[1]], toB64(as.character(tableRows[[i]][[2]])))
                        c3 <- identical(x[[2]], toB64(as.character(tableRows[[i]][[1]])))
                        c4 <- identical(x[[2]], toB64(as.character(tableRows[[i]][[2]])))

                        if (c1 && c4)
                            return(list(TRUE,FALSE))
                        else if (c2 && c3)
                            return(list(TRUE,TRUE))
                        else
                            return(list(FALSE,FALSE))
                    })

                    index <- which(sapply(location, function(x) return(x[[1]])))
                    reverse <- location[[index]][[2]]

                    row <- list()
                    row[['md']] <- if(reverse) -tukey[index,'estimate'] else tukey[index,'estimate']
                    row[['se']] <- tukey[index,'SE']
                    row[['df']] <- tukey[index,'df']
                    row[['t']] <- if(reverse) -tukey[index,'t.ratio'] else tukey[index,'t.ratio']

                    row[['pnone']] <- none[index,'p.value']
                    row[['ptukey']] <- tukey[index,'p.value']
                    row[['pscheffe']] <- scheffe[index,'p.value']
                    row[['pbonferroni']] <- bonferroni[index,'p.value']
                    row[['pholm']] <- holm[index,'p.value']

                    table$setRow(rowNo=i, values=row)
                    private$.checkpoint()
                }
                table$setStatus('complete')
            }
        },
        .populateEmmTables = function() {

            emMeans <- self$options$emMeans
            emmTables <- private$emMeans

            group <- self$results$emm

            for (j in seq_along(emMeans)) {

                emm <- emMeans[[j]]

                if ( ! is.null(emm)) {

                    emmGroup <- group$get(key=j)
                    table <- emmGroup$emmTable

                    emmTable <- emmTables[[j]]

                    for (k in 1:nrow(emmTable)) {
                        row <- list()
                        sign <- list()

                        for (l in seq_along(emm)) {
                            value <- emmTable[k, jmvcore::toB64(emm[l])]
                            row[[emm[l]]] <- jmvcore::fromB64(value)
                        }

                        row[['mean']] <- emmTable[k, 'emmean']
                        row[['se']] <- emmTable[k, 'SE']
                        row[['lower']] <- emmTable[k, 'lower.CL']
                        row[['upper']] <- emmTable[k, 'upper.CL']

                        table$setRow(rowNo=k, values=row)
                    }
                }
            }
        },

        .populateGroupSummaryTable = function() {

            table <- self$results$groupSummary
            data <- self$data
            bs <- self$options$bs
            complete <- complete.cases(data)

            if (length(bs) == 0) {
                n <- sum(complete)
                ex <- length(complete) - n
                table$setRow(rowNo=1, values=list(n=n, ex=ex))
            } else {
                by <- lapply(bs, function(x) self$data[[x]])
                rm <- lapply(self$options$rmCells, function(x) x$measure)
                nt <- aggregate(complete, by=by, length)$x
                n <- aggregate(complete, by=by, sum)$x
                ex <- nt - n
                for (i in seq_along(n))
                    table$setRow(rowNo=i, values=list(n=n[i], ex=ex[i]))
            }
        },

        #### Plot functions ----
        .prepareQQPlot = function(model) {
            image <- self$results$assump$qq

            suppressMessages({
                suppressWarnings({
                    residuals <- scale(residuals(model))
                })
            })

            image$setState(residuals)
        },
        .qqPlot=function(image, ggtheme, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            df <- as.data.frame(qqnorm(image$state, plot.it=FALSE))

            p <- ggplot(data=df, aes(y=y, x=x)) +
                geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
                geom_point(aes(x=x,y=y), size=2, colour=theme$color[1]) +
                xlab(.("Theoretical Quantiles")) +
                ylab(.("Standardized Residuals")) +
                ggtheme

            return(p)
        },
        .prepareEmmPlots = function(model, data) {

            emMeans <- self$options$emMeans

            group <- self$results$emm
            emmTables <- list()

            for (j in seq_along(emMeans)) {

                term <- emMeans[[j]]

                if ( ! is.null(term)) {

                    image <- group$get(key=j)$emmPlot

                    termB64 <- jmvcore::toB64(term)
                    formula <- formula(paste('~', jmvcore::composeTerm(termB64)))

                    if (self$options$emmWeights)
                        weights <- 'equal'
                    else
                        weights <- 'cells'

                    suppressMessages({
                        emmeans::emm_options(sep = ",", parens = "a^")

                        mm <- try(
                            emmeans::emmeans(model, formula, options=list(level=self$options$ciWidthEmm / 100),
                                             weights = weights, model = "multivariate"),
                            silent = TRUE
                        )
                    })

                    d <- as.data.frame(summary(mm))
                    emmTables[[ j ]] <- d

                    for (k in 1:3) {
                        if ( ! is.na(termB64[k])) {
                            d[[ termB64[k] ]] <- factor(jmvcore::fromB64(d[[ termB64[k] ]]),
                                                        jmvcore::fromB64(levels(d[[ termB64[k] ]])))
                        }
                    }

                    names <- list('x'=termB64[1], 'y'='emmean', 'lines'=termB64[2], 'plots'=termB64[3], 'lower'='lower.CL', 'upper'='upper.CL')
                    names <- lapply(names, function(x) if (is.na(x)) NULL else x)

                    labels <- list('x'=term[1], 'y'=self$options$depLabel, 'lines'=term[2], 'plots'=term[3])
                    labels <- lapply(labels, function(x) if (is.na(x)) NULL else x)

                    dataNew <- lapply(data, function(x) {
                        if (is.factor(x))
                            levels(x) <- jmvcore::fromB64(levels(x))
                        return(x)
                    })

                    image$setState(list(emm=d, data=dataNew, names=names, labels=labels))

                }
            }

            private$emMeans <- emmTables
        },
        .emmPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            data <- as.data.frame(image$state$data)
            emm <- image$state$emm
            names <- image$state$names
            labels <- image$state$labels

            emm$lowerSE <- emm[[names$y]] - emm[['SE']]
            emm$upperSE <- emm[[names$y]] + emm[['SE']]

            if (theme$bw) {
                lty <- names$lines
            } else {
                lty <- NULL
            }

            if (self$options$emmPlotData)
                dodge <- position_dodge(0.7)
            else
                dodge <- position_dodge(0.3)

            if (is.null(names$lines))
                jitterdodge <- position_jitter(width = 0.1)
            else
                jitterdodge <- position_jitterdodge(dodge.width = 0.7, jitter.width = 0.4)

            p <- ggplot(
                data=emm,
                aes_string(
                    x=names$x,
                    y=names$y,
                    color=names$lines,
                    fill=names$lines,
                    linetype=lty,
                    group=names$lines
                ),
                inherit.aes = FALSE
            )

            if (self$options$emmPlotData)
                p <- p + geom_point(data=data, aes_string(y=jmvcore::toB64('.DEPENDENT')), alpha=0.3, position=jitterdodge)

            p <- p + geom_line(size=.8, position=dodge)

            if (self$options$emmPlotError == 'ci') {
                p <- p + geom_errorbar(
                    aes_string(x=names$x, ymin=names$lower, ymax=names$upper, linetype=NULL),
                    width=.1, size=.8, position=dodge
                )
            } else if (self$options$emmPlotError == 'se') {
                p <- p + geom_errorbar(
                    aes_string(x=names$x, ymin='lowerSE', ymax='upperSE', linetype=NULL),
                    width=.1, size=.8, position=dodge
                )
            }

            p <- p + geom_point(shape=21, fill='white', size=3, position=dodge)

            if ( ! is.null(names$plots)) {
                formula <- as.formula(paste(". ~", names$plots))
                p <- p + facet_grid(formula)
            }

            p <- p +
                labs(x=labels$x, y=labels$y, fill=labels$lines, color=labels$lines, linetype=labels$lines) +
                ggtheme + theme(panel.spacing = unit(2, "lines"))

            return(p)
        },

        #### Helper functions ----
        .dataCheck=function() {
            data <- self$data

            rm <- sapply(self$options$rmCells, function(x) return(x$measure))
            bs <- unlist(self$options$bs)
            cov <- unlist(self$options$cov)

            varsNumeric <- c(rm, cov)

            dataFactors <- list()
            for (i in seq_along(bs))
                dataFactors[[bs[i]]] <- data[[bs[i]]]

            dataNumeric <- list()
            for (i in seq_along(varsNumeric))
                dataNumeric[[varsNumeric[i]]] <- jmvcore::toNumeric(data[[varsNumeric[i]]])

            # Check all values
            allNAItems <- sapply(c(dataFactors, dataNumeric), function(x) all(is.na(x)))
            if (any(allNAItems)) {
                onlyContainsMissingsMessage <- .("Item '{item}' contains only missing values")
                jmvcore::reject(onlyContainsMissingsMessage, code='error', item=c(bs,varsNumeric)[allNAItems])
            }

            # Check factor values
            if (length(dataFactors) > 0) {
                singleLevelItems <- sapply(dataFactors, function(x) length(levels(x)) == 1)
                if (any(singleLevelItems)) {
                    oneLevelOnlyMessage <- .("Item '{item}' consists of one level only")
                    jmvcore::reject(oneLevelOnlyMessage, code='error', item=bs[singleLevelItems])
                }

                factorLevelCounts = table(dataFactors)
                if (any(factorLevelCounts == 0)) {
                    jmvcore::reject(
                        .("Empty cells in between subject design: at least one combination of between subject factor levels has 0 observations"),
                        code=exceptions$dataError
                    )
                }
            }

            # Check numeric values
            factorItems <- sapply(dataNumeric, function(x) class(jmvcore::toNumeric(x)) == "factor")
            infItems <- sapply(dataNumeric, function(x) any(is.infinite(x)))
            noVarItems <- sapply(dataNumeric, function(x) var(x, na.rm = TRUE) == 0)
            if (any(factorItems)) {
                notNumericMessage <- .("Item '{item}' needs to be numeric")
                jmvcore::reject(notNumericMessage, code='error', item=varsNumeric[factorItems])
            }

            if (any(infItems)) {
                infiniteValuesMessage <- .("Item '{item}' contains infinite values")
                jmvcore::reject(infiniteValuesMessage, code='error', item=varsNumeric[infItems])
            }

            # if (any(noVarItems))
            #     jmvcore::reject("Item '{}' has no variance", code='error', varsNumeric[noVarItems])
        },
        .getAllInteractions = function(vars) {
            n <- length(vars)
            terms <- list()
            for (i in 1:n) {
                comb <- combn(vars, i, simplify = FALSE)
                terms <- c(terms, comb)
            }
            terms
        },
        .getRmTerms = function() {
            rmTerms <- self$options$rmTerms
            if (length(rmTerms) > 0)
                return(rmTerms)

            rm <- self$options$rm
            if (length(rm) == 0)
                return(list())

            rmNames <- sapply(rm, function(x) x$label, simplify = TRUE)
            rmTerms <- private$.getAllInteractions(rmNames)

            return(rmTerms)
        },
        .getBsTerms = function() {
            bsTerms <- self$options$bsTerms
            if (length(bsTerms) > 0)
                return(bsTerms)

            bs <- self$options$bs
            cov <- self$options$cov

            if (length(bs) == 0 && length(cov) == 0)
                return(list())

            if (length(bs) == 0) {
                bsTerms <- list()
            } else {
                bsTerms <- private$.getAllInteractions(bs)
            }

            for (i in seq_along(cov))
                bsTerms[[length(bsTerms) + 1]] <- cov[i]

            return(bsTerms)
        },
        .rmTableRowLabels = function() {
            rmTerms <- self$rmTerms
            bsTerms <- self$bsTerms

            terms <- list()
            spacing <- list()

            for (i in seq_along(rmTerms)) {
                rmTerm <- rmTerms[[i]]
                terms[[length(terms) + 1]] <- rmTerm
                spacing[[length(terms)]] <- 'above'

                for (j in seq_along(bsTerms))
                    terms[[length(terms) + 1]] <- c(rmTerm, bsTerms[[j]])

                terms[[length(terms) + 1]] <- 'Residual'
                spacing[[length(terms)]] <- 'below'
            }

            return(list(terms = terms, spacing = spacing))
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
                data[[var]] <- factor(self$data[[var]])

            attr(data, 'row.names') <- seq_len(length(data[[1]]))
            attr(data, 'class') <- 'data.frame'
            data <- jmvcore::naOmit(data)

            data <- cbind(data, '.SUBJECT'=1:nrow(data))

            dataLong <- as.list(
                reshape2:::melt.data.frame(
                    data,
                    id.vars=c(bsVars, covVars, '.SUBJECT'),
                    measure.vars=rmVars,
                    value.name='.DEPENDENT'
                )
            )

            col <- dataLong[['variable']]
            temp <- numeric(length(col))
            for (j in seq_along(col))
                temp[j] <- which(rmVars %in% col[j])

            for (i in seq_along(labels))
                dataLong[[labels[[i]]]] <- factor(sapply(rmCells[temp], function(x) x[i]), levels[[i]])

            dataLong[['variable']] <- NULL

            dataLong <- lapply(dataLong, function(x) {
                if (is.factor(x))
                    levels(x) <- toB64(levels(x))
                return(x)
            })

            attr(dataLong, 'row.names') <- seq_len(length(dataLong[[1]]))
            attr(dataLong, 'names') <- toB64(names(dataLong))
            attr(dataLong, 'class') <- 'data.frame'
            dataLong <- jmvcore::naOmit(dataLong)

            return(dataLong)
        },
        .modelFormula = function() {
            bsTerms <- lapply(self$bsTerms, function(x) toB64(x))
            rmTerms <- lapply(self$rmTerms, function(x) toB64(x))

            bsItems <- composeTerms(bsTerms)
            bsTerm <- paste0('(', paste0(bsItems, collapse = ' + '), ')')

            rmItems <- composeTerms(rmTerms)
            rmTerm <- paste0('Error(', paste0(toB64('.SUBJECT'),'/(', rmItems, ')', collapse=' + '),')')

            allTerms <- c(bsTerms, rmTerms)
            for (term1 in rmTerms) {
                for (term2 in bsTerms) {
                    allTerms[[length(allTerms) + 1]] <- unlist(c(term1, term2))
                }
            }

            allItems <- composeTerms(allTerms)
            mainTerm <- paste0('(', paste0(allItems, collapse = ' + '), ')')

            if (length(self$options$bsTerms) == 0) {
                formula <- as.formula(paste(toB64('.DEPENDENT'), '~', paste(mainTerm, rmTerm, sep=' + ')))
            } else {
                formula <- as.formula(paste(toB64('.DEPENDENT'), '~', paste(mainTerm, rmTerm, bsTerm, sep=' + ')))
            }

            return(formula)
        },
        .emmPlotSize = function(emm) {
            data <- self$data
            bsFactors <- self$options$bs
            rmFactors <- self$options$rm

            rmNames <- sapply(rmFactors, function(x) return(x$label))
            rmLevels <- lapply(rmFactors, function(x) return(x$levels))

            levels <- list()
            for (i in seq_along(emm)) {
                if (emm[i] %in% rmNames) {
                    levels[[ emm[i] ]] <- rmLevels[[which(emm[i] == rmNames)]]
                } else {
                    levels[[ emm[i] ]] <- levels(data[[ emm[i] ]])
                }
            }

            nLevels <- as.numeric(sapply(levels, length))
            nLevels <- ifelse(is.na(nLevels[1:3]), 1, nLevels[1:3])
            nCharLevels <- as.numeric(sapply(lapply(levels, nchar), max))
            nCharLevels <- ifelse(is.na(nCharLevels[1:3]), 0, nCharLevels[1:3])
            nCharNames <- as.numeric(nchar(names(levels)))
            nCharNames <- ifelse(is.na(nCharNames[1:3]), 0, nCharNames[1:3])

            xAxis <- 30 + 20
            yAxis <- 30 + 20

            width <- max(350, 25 * nLevels[1] * nLevels[2] * nLevels[3])
            height <- 300 + ifelse(nLevels[3] > 1, 20, 0)

            legend <- max(25 + 21 + 3.5 + 8.3 * nCharLevels[2] + 28, 25 + 10 * nCharNames[2] + 28)
            width <- yAxis + width + ifelse(nLevels[2] > 1, legend, 0)
            height <- xAxis + height

            return(c(width, height))
        },
        .getSSt=function (model) {
            rmTerms <- lapply(self$rmTerms, jmvcore::toB64)
            bsTerms <- lapply(self$bsTerms, jmvcore::toB64)

            if (length(bsTerms) == 0)
                bsTerms <- list('(Intercept)')

            terms <- c(rmTerms, bsTerms)

            modelRows <- jmvcore::decomposeTerms(as.list(rownames(model)))

            termSSt <- sum(model[-1, 'Sum Sq'])

            errorSSt <- 0
            for (i in seq_along(terms)) {
                for (j in seq_along(modelRows)) {
                    if (all(terms[[i]] %in% modelRows[[j]]) && length(terms[[i]]) == length(modelRows[[j]])) {
                        errorSSt <- errorSSt + model[j, 'Error SS']
                        break
                    }
                }
            }

            SSt <- termSSt + errorSSt

            return(SSt)
        },
        .sourcifyOption = function(option) {
            name <- option$name
            value <- option$value

            if (name == 'contrasts') {
                if (all(vapply(value, function(x) x$type == 'none', FALSE)))
                    return('')
            }

            super$.sourcifyOption(option)
        })
)
