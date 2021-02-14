
#' @import ggplot2
#' @importFrom jmvcore matchSet
ancovaClass <- R6::R6Class(
    "ancovaClass",
    inherit=ancovaBase,
    private=list(
        #### Member variables ----
        .model = NA,
        .postHocRows = NA,
        .data = NA,
        emMeans = list(),

        #### Init + run functions ----
        .init=function() {

            private$.initMainTable()

            factors <- self$options$factors
            modelTerms <- private$.modelTerms()

            if (length(factors) == 0 || length(modelTerms) == 0)
                return()

            private$.data <- private$.cleanData()

            private$.initContrastTables()
            private$.initPostHoc()
            private$.initEmm()
            private$.initEmmTable()

        },
        .run=function() {

            dep <- self$options$dep
            factors <- self$options$factors
            modelTerms <- private$.modelTerms()

            if (is.null(dep) || length(factors) == 0 || length(modelTerms) == 0)
                return()

            data <- private$.cleanData()
            private$.data <- data

            for (name in colnames(data)) {
                column <- data[[name]]
                if (is.factor(column) && any(table(column) == 0))
                    reject("Column '{}' contains unused levels (possibly only when rows with missing values are excluded)", name=name)
            }

            dataB64 <- lapply(data, function(x) {
                if (is.factor(x))
                    levels(x) <- toB64(levels(x))
                return(x)
            })

            private$.errorCheck(dataB64)

            results <- private$.compute(dataB64)

            private$.populateMainTable(results)
            private$.populateContrasts(dataB64)
            private$.populateLevenes(dataB64)
            private$.populateNormality()
            private$.populatePostHoc(dataB64)
            private$.prepareEmmPlots(data)
            private$.populateEmmTables()
            private$.populateOutputs()
        },

        #### Compute results ----
        .compute = function(data) {

            dep <- self$options$dep
            factors <- self$options$factors
            modelTerms <- private$.modelTerms()

            suppressWarnings({

                base::options(contrasts = c("contr.sum","contr.poly"))

                for (contrast in self$options$contrasts) {
                    levels <- base::levels(data[[contrast$var]])
                    stats::contrasts(data[[contrast$var]]) <- private$.createContrasts(levels, contrast$type)
                }

                formula <- jmvcore::constructFormula(dep, modelTerms)
                formula <- stats::as.formula(formula)

                model <- stats::aov(formula, data)
                private$.model <- model
                self$results$.setModel(model)

                singular <- NULL

                if (self$options$ss == '1') {

                    results <- try(stats::anova(private$.model), silent=TRUE)

                } else if (self$options$ss == '2') {

                    results <- try(car::Anova(private$.model, type=2, singular.ok=FALSE), silent=TRUE)
                    if (isError(results)) {
                        message <- extractErrorMessage(results)
                        if (message == 'there are aliased coefficients in the model')
                            singular <- 'Singular fit encountered; one or more predictor variables are a linear combination of other predictor variables'
                        results <- try(car::Anova(private$.model, type=2, singular.ok=TRUE), silent=TRUE)
                    }

                } else {

                    results <- try({
                        r <- car::Anova(private$.model, type=3, singular.ok=FALSE, silent=TRUE)
                        r <- r[-1,]
                    })

                    if (isError(results)) {
                        message <- extractErrorMessage(results)
                        if (message == 'there are aliased coefficients in the model')
                            singular <- 'Singular fit encountered; one or more predictor variables are a linear combination of other predictor variables'
                        results <- try({
                            r <- car::Anova(private$.model, type=3, singular.ok=TRUE, silent=TRUE)
                            r <- r[-1,]
                        })
                    }
                }

                if (isError(results)) {
                    message <- extractErrorMessage(results)
                    if (message == 'residual df = 0')
                        reject('Residual sum of squares and/or degrees of freedom is zero, indicating a perfect fit')
                }

                if (results['Residuals', 'Sum Sq'] == 0 || results['Residuals', 'Df'] == 0)
                    reject('Residual sum of squares and/or degrees of freedom is zero, indicating a perfect fit')

            }) # suppressWarnings

            return(list(r=results, singular=singular))

        },

        #### Init tables/plots functions ----
        .initMainTable = function() {

            table    <- self$results$main

            if (self$options$modelTest) {
                table$addRow(rowKey='.', list(name='Overall model'))
                table$addFormat(rowKey='.', col=1, format=Cell.BEGIN_END_GROUP)
            }

            modelTerms <- private$.modelTerms()
            if (length(modelTerms) > 0) {
                for (i in seq_along(modelTerms)) {
                    term <- modelTerms[[i]]
                    table$addRow(rowKey=term, list(name=stringifyTerm(term)))
                    if (i == 1)
                        table$addFormat(rowKey=term, col=1, format=Cell.BEGIN_GROUP)
                    else if (i == length(modelTerms))
                        table$addFormat(rowKey=term, col=1, format=Cell.END_GROUP)
                }

            } else {
                table$addRow(rowKey='...', list(name='...'))
                table$addFormat(rowKey='...', col=1, format=Cell.BEGIN_END_GROUP)
            }

            table$addRow(rowKey='', list(name='Residuals'))
            table$addFormat(rowKey='', col=1, format=Cell.BEGIN_END_GROUP)

            if (self$options$ss == '1') {
                table$setRefs('R')
            } else {
                table$setRefs('car')
            }
        },
        .initContrastTables = function() {

            data <- private$.data
            tables <- self$results$contrasts

            for (contrast in self$options$contrasts) {
                if (contrast$type == 'none')
                    next()
                table <- tables$addItem(contrast)

                var <- data[[contrast$var]]
                levels <- base::levels(var)
                labels <- private$.contrastLabels(levels, contrast$type)

                for (label in labels)
                    table$addRow(rowKey=label, list(contrast=label))
            }

        },
        .initPostHoc=function() {

            data <- private$.data
            bs <- self$options$factors
            phTerms <- self$options$postHoc

            bsLevels <- list()
            for (i in seq_along(bs))
                bsLevels[[bs[i]]] <- levels(data[[bs[i]]])

            tables <- self$results$postHoc

            postHocRows <- list()

            for (ph in phTerms) {

                table <- tables$get(key=ph)

                table$setTitle(paste0('Post Hoc Comparisons - ', stringifyTerm(ph)))

                for (i in seq_along(ph))
                    table$addColumn(name=paste0(ph[i],'1'), title=ph[i], type='text', superTitle='Comparison', combineBelow=TRUE)

                table$addColumn(name='sep', title='', type='text', content='-', superTitle='Comparison', format='narrow')

                for (i in seq_along(ph))
                    table$addColumn(name=paste0(ph[i],'2'), title=ph[i], type='text', superTitle='Comparison')

                table$addColumn(name='md', title='Mean Difference', type='number')
                table$addColumn(name='se', title='SE', type='number')
                table$addColumn(name='df', title='df', type='number')
                table$addColumn(name='t', title='t', type='number')

                table$addColumn(name='pnone', title='p', type='number', format='zto,pvalue', visible="(postHocCorr:none)")
                table$addColumn(name='ptukey', title='p<sub>tukey</sub>', type='number', format='zto,pvalue', visible="(postHocCorr:tukey)")
                table$addColumn(name='pscheffe', title='p<sub>scheffe</sub>', type='number', format='zto,pvalue', visible="(postHocCorr:scheffe)")
                table$addColumn(name='pbonferroni', title='p<sub>bonferroni</sub>', type='number', format='zto,pvalue', visible="(postHocCorr:bonf)")
                table$addColumn(name='pholm', title='p<sub>holm</sub>', type='number', format='zto,pvalue', visible="(postHocCorr:holm)")

                ciTitleES <- paste0(self$options$postHocEsCiWidth, '% Confidence Interval')
                table$addColumn(name='d', title='Cohen\'s d', type='number', visible="(postHocES:d)")
                table$addColumn(name='dlower', title='Lower', type='number', visible="(postHocES:d && postHocEsCi)", superTitle=ciTitleES)
                table$addColumn(name='dupper', title='Upper', type='number', visible="(postHocES:d && postHocEsCi)", superTitle=ciTitleES)

                combin <- expand.grid(bsLevels[rev(ph)])
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

                table$setNote('note', 'Comparisons are based on estimated marginal means')
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
            group <- self$results$emm

            for (j in seq_along(emMeans)) {

                emm <- emMeans[[j]]

                if ( ! is.null(emm)) {

                    emmGroup <- group$get(key=j)

                    table <- emmGroup$emmTable
                    table$setTitle(paste0('Estimated Marginal Means - ', jmvcore::stringifyTerm(emm)))

                    nLevels <- numeric(length(emm))
                    for (k in rev(seq_along(emm))) {
                        table$addColumn(name=emm[k], title=emm[k], type='text', combineBelow=TRUE)
                        nLevels[k] <- length(levels(self$data[[ emm[k] ]]))
                    }

                    table$addColumn(name='mean', title='Mean', type='number')
                    table$addColumn(name='se', title='SE', type='number')
                    table$addColumn(name='lower', title='Lower', type='number', superTitle=paste0(self$options$ciWidthEmm, '% Confidence Interval'))
                    table$addColumn(name='upper', title='Upper', type='number', superTitle=paste0(self$options$ciWidthEmm, '% Confidence Interval'))

                    nRows <- prod(nLevels)

                    for (k in 1:nRows) {
                        row <- list()
                        table$addRow(rowKey=k, row)
                    }
                }
            }
        },

        #### Populate tables functions ----
        .populateMainTable = function(results) {

            table <- self$results$main

            r <- results$r
            singular <- results$singular

            if ( ! is.null(singular))
                table$setNote('singular', singular)

            rowCount <- dim(r)[1]
            rowNames <- dimnames(r)[[1]]

            errIndex <- nrow(r)
            errSS <- r[errIndex,'Sum Sq']
            errDF <- r[errIndex,'Df']
            errMS <- errSS / errDF
            totalSS <- sum(r[['Sum Sq']], na.rm=TRUE)
            modelSS <- totalSS - errSS

            decomposed <- decomposeTerms(rowNames)

            for (rowKey in table$rowKeys) {
                if (identical(rowKey, ''))
                    index <- rowCount
                else if (identical(rowKey, '.'))
                    index <- 0
                else
                    index <- matchSet(rowKey, decomposed)

                if (index > 0) {
                    ss <- r[index, 'Sum Sq']
                    df <- r[index, 'Df']
                    ms <- ss / df
                    F  <- r[index, 'F value']
                    p  <- r[index, 'Pr(>F)']

                    if (length(F) == 1 && is.finite(F)) {
                        e <- ss / totalSS
                        ep <- ss / (ss + errSS)
                        w <- (ss - (df * errMS)) / (totalSS + errMS)
                    } else {
                        e <- ''
                        ep <- ''
                        w <- ''
                    }

                    if ( ! is.finite(ss))
                        ss <- 0
                    if ( ! is.finite(ms))
                        ms <- ''
                    if (length(F) != 1 || ! is.finite(F))
                        F <- ''
                    if ( ! is.finite(p))
                        p <- ''
                } else if (index == 0) {

                    summ <- summary.lm(private$.model)
                    fstat <- summ$fstatistic

                    ss <- modelSS
                    df <- unname(fstat[2])
                    ms <- modelSS / df
                    F  <- unname(fstat[1])
                    p <- stats::pf(fstat[1], fstat[2], fstat[3], lower.tail=FALSE)
                    e <- ''
                    ep <- ''
                    w <- ''

                } else {

                    ss <- 0
                    df <- NaN
                    ms <- ''
                    F  <- ''
                    p  <- ''
                    e <- ''
                    ep <- ''
                    w <- ''
                }

                tableRow <- list(ss=ss, df=df, ms=ms, F=F, p=p, etaSq=e, etaSqP=ep, omegaSq=w)
                table$setRow(rowKey=rowKey, tableRow)
            }
        },
        .populatePostHoc=function(data) {

            terms <- self$options$postHoc

            if (length(terms) == 0)
                return()

            tables <- self$results$postHoc

            postHocRows <- list()

            for (ph in terms) {

                table <- tables$get(key=ph)

                term <- jmvcore::composeTerm(ph)
                termB64 <- jmvcore::composeTerm(toB64(ph))

                formula <- as.formula(paste('~', term))

                suppressWarnings({

                    # table$setStatus('running')

                    emmeans::emm_options(sep = ",", parens = "a^")
                    referenceGrid <- emmeans::emmeans(private$.model, formula)
                    none <- summary(pairs(referenceGrid, adjust='none'))
                    tukey <- summary(pairs(referenceGrid, adjust='tukey'))
                    scheffe <- summary(pairs(referenceGrid, adjust='scheffe'))
                    bonferroni <- summary(pairs(referenceGrid, adjust='bonferroni'))
                    holm <- summary(pairs(referenceGrid, adjust='holm'))
                    effSize <- as.data.frame(
                        emmeans::eff_size(
                            referenceGrid,
                            sigma=sigma(private$.model),
                            edf=private$.model$df.residual,
                            level=self$options$postHocEsCiWidth/100
                        )
                    )
                }) # suppressWarnings

                resultRows <- lapply(strsplit(as.character(none$contrast), ' - '), function(x) strsplit(x, ','))
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

                    if (length(index) == 1) {

                        reverse <- location[[index]][[2]]

                        row <- list()
                        row[['md']] <- if(reverse) -none[index,'estimate'] else none[index,'estimate']
                        row[['se']] <- none[index,'SE']
                        row[['df']] <- none[index,'df']
                        row[['t']] <- if(reverse) -none[index,'t.ratio'] else none[index,'t.ratio']

                        row[['pnone']] <- none[index,'p.value']
                        row[['ptukey']] <- tukey[index,'p.value']
                        row[['pscheffe']] <- scheffe[index,'p.value']
                        row[['pbonferroni']] <- bonferroni[index,'p.value']
                        row[['pholm']] <- holm[index,'p.value']

                        row[['d']] <- effSize[index, 'effect.size']
                        row[['dlower']] <- effSize[index, 'lower.CL']
                        row[['dupper']] <- effSize[index, 'upper.CL']

                        table$setRow(rowNo=i, values=row)

                    } else {

                        table$setRow(rowNo=i, values=list(
                            md=NaN, se='', df='', t='',
                            pnone='', ptukey='', pscheffe='', pbonferroni='', pholm='',
                            es=''))
                    }
                }

                table$setStatus('complete')
                private$.checkpoint()
            }
        },
        .populateContrasts=function(data) {

            contrResults <- stats::summary.lm(private$.model)[["coefficients"]]
            contrasts <- self$options$contrasts

            for (contrast in contrasts) {

                var <- contrast$var
                type <- contrast$type

                if (type == 'none')
                    next()

                levels <- base::levels(data[[var]])
                labels <- private$.contrastLabels(fromB64(levels), type)

                table <- self$results$get('contrasts')$get(contrast)

                for (i in seq_along(labels)) {
                    label <- labels[[i]]
                    name <- paste0(var, i)
                    table$setRow(rowNo=i, list(
                        contrast=label,
                        est=contrResults[name, "Estimate"],
                        se=contrResults[name, "Std. Error"],
                        t=contrResults[name, "t value"],
                        p=contrResults[name, "Pr(>|t|)"]
                    ))
                }
            }
        },
        .populateLevenes=function(data) {

            if ( ! self$options$homo)
                return()

            data[[".RES"]] = abs(private$.model$residuals)
            modelTerms <- private$.modelTerms()
            factors <- self$options$factors

            rhs <- paste0('`', factors, '`', collapse=':')
            formula <- as.formula(paste0('.RES ~', rhs))

            model <- stats::lm(formula, data)
            summary <- stats::anova(model)

            table <- self$results$get('assump')$get('homo')

            table$setRow(rowNo=1, values=list(
                F=summary$`F value`[1],
                df1=summary$Df[1],
                df2=summary$Df[2],
                p=summary$`Pr(>F)`[1]))
        },
        .populateNormality = function() {
            if ( ! self$options$norm)
                return()

            residuals <- self$residuals[[1]]
            if (is.null(residuals))
                return()

            res <- try(shapiro.test(residuals))
            if (jmvcore::isError(res)) {
                values <- list(`s[sw]`=NaN, `p[sw]`='')
            } else {
                values <- list(`s[sw]`=res$statistic, `p[sw]`=res$p.value)
            }

            self$results$assump$norm$setRow(rowNo=1, values)
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
                            value <- emmTable[k, emm[l]]
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
        .populateOutputs=function() {

            if (self$options$resids && self$results$resids$isNotFilled()) {
                self$results$resids$setValues(self$residuals)
            }

            if (self$options$predict) {

            }
        },

        #### Plot functions ----
        .qqPlot=function(image, ggtheme, theme, ...) {
            residuals <- self$residuals[[1]]
            if (is.null(residuals))
                return()

            df <- as.data.frame(qqnorm(residuals, plot.it=FALSE))

            return(ggplot(data=df, aes(y=y, x=x)) +
                      geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
                      geom_point(aes(x=x,y=y), size=2, colour=theme$color[1]) +
                      xlab("Theoretical Quantiles") +
                      ylab("Standardized Residuals") +
                      ggtheme)
        },
        .prepareEmmPlots = function(data) {

            emMeans <- self$options$emMeans

            group <- self$results$emm
            emmTables <- list()
            model <- private$.model

            dep <- self$options$dep

            for (j in seq_along(emMeans)) {

                term <- emMeans[[j]]

                if ( ! is.null(term)) {

                    image <- group$get(key=j)$emmPlot

                    # termB64 <- jmvcore::toB64(term)
                    termB64 <- jmvcore::composeTerms(term)
                    formula <- formula(paste('~', jmvcore::composeTerm(term)))

                    if (self$options$emmWeights)
                        weights <- 'equal'
                    else
                        weights <- 'cells'

                    suppressMessages({
                        emmeans::emm_options(sep = ",", parens = "a^")

                        mm <- try(
                            emmeans::emmeans(model, formula, options=list(level=self$options$ciWidthEmm / 100), weights = weights),
                            silent = TRUE
                        )
                    })

                    d <- as.data.frame(summary(mm))
                    emmTables[[ j ]] <- d

                    for (k in 1:3) {
                        if ( ! is.na(termB64[k])) {
                            d[[ term[k] ]] <- factor(jmvcore::fromB64(d[[ term[k] ]]),
                                                        jmvcore::fromB64(levels(d[[ term[k] ]])))
                        }
                    }

                    names <- list('x'=termB64[1], 'y'='emmean', 'lines'=termB64[2], 'plots'=termB64[3], 'lower'='lower.CL', 'upper'='upper.CL')
                    names <- lapply(names, function(x) if (is.na(x)) NULL else x)

                    labels <- list('x'=term[1], 'y'=dep, 'lines'=term[2], 'plots'=term[3])
                    labels <- lapply(labels, function(x) if (is.na(x)) NULL else x)

                    image$setState(list(emm=d, data=data, names=names, labels=labels))

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

            if (self$options$emmPlotData)
                dodge <- position_dodge(0.7)
            else
                dodge <- position_dodge(0.3)

            if (is.null(names$lines))
                jitterdodge <- position_jitter(width = 0.1)
            else
                jitterdodge <- position_jitterdodge(dodge.width = 0.7, jitter.width = 0.4)

            p <- ggplot(data=emm, aes_string(x=names$x, y=names$y, color=names$lines, fill=names$lines, group=names$lines), inherit.aes = FALSE)

            if (self$options$emmPlotData)
                p <- p + geom_point(data=data, aes(y=!!data[[labels$y]]), alpha=0.3, position=jitterdodge)

            p <- p + geom_line(size=.8, position=dodge)

            if (self$options$emmPlotError == 'ci')
                p <- p + geom_errorbar(aes_string(x=names$x, ymin=names$lower, ymax=names$upper), width=.1, size=.8, position=dodge)
            else if (self$options$emmPlotError == 'se')
                p <- p + geom_errorbar(aes_string(x=names$x, ymin='lowerSE', ymax='upperSE'), width=.1, size=.8, position=dodge)

            p <- p + geom_point(shape=21, fill='white', size=3, position=dodge)

            if ( ! is.null(names$plots)) {
                formula <- as.formula(paste(". ~", names$plots))
                p <- p + facet_grid(formula)
            }

            p <- p +
                labs(x=labels$x, y=labels$y, fill=labels$lines, color=labels$lines) +
                ggtheme + theme(panel.spacing = unit(2, "lines"))

            return(p)
        },

        #### Helper functions ----
        .contrastLabels=function(levels, type) {

            nLevels <- length(levels)
            labels <- list()

            if (length(levels) <= 1) {

                # do nothing

            } else if (type == 'simple') {

                for (i in seq_len(nLevels-1))
                    labels[[i]] <- paste(levels[i+1], '-', levels[1])

            } else if (type == 'deviation') {

                all <- paste(levels, collapse=', ')
                for (i in seq_len(nLevels-1))
                    labels[[i]] <- paste(levels[i+1], '-', all)

            } else if (type == 'difference') {

                for (i in seq_len(nLevels-1)) {
                    rhs <- paste0(levels[1:i], collapse=', ')
                    labels[[i]] <- paste(levels[i + 1], '-', rhs)
                }

            } else if (type == 'helmert') {

                for (i in seq_len(nLevels-1)) {
                    rhs <- paste(levels[(i+1):nLevels], collapse=', ')
                    labels[[i]] <- paste(levels[i], '-', rhs)
                }

            } else if (type == 'repeated') {

                for (i in seq_len(nLevels-1))
                    labels[[i]] <- paste(levels[i], '-', levels[i+1])

            } else if (type == 'polynomial') {

                names <- c('linear', 'quadratic', 'cubic', 'quartic', 'quintic', 'sextic', 'septic', 'octic')

                for (i in seq_len(nLevels-1)) {
                    if (i <= length(names)) {
                        labels[[i]] <- names[i]
                    } else {
                        labels[[i]] <- paste('degree', i, 'polynomial')
                    }
                }
            }

            labels
        },
        .createContrasts=function(levels, type) {

            nLevels <- length(levels)

            if (type == 'simple') {

                dummy <- contr.treatment(levels)
                dimnames(dummy) <- NULL
                coding <- matrix(rep(1/nLevels, prod(dim(dummy))), ncol=nLevels-1)
                contrast <- (dummy - coding)

            } else if (type == 'deviation') {

                contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)

                for (i in seq_len(nLevels-1)) {
                    contrast[i+1, i] <- 1
                    contrast[1, i] <- -1
                }

            } else if (type == 'difference') {

                contrast <- stats::contr.helmert(levels)
                for (i in 1:ncol(contrast))
                    contrast[,i] <- contrast[,i] / (i + 1)

                dimnames(contrast) <- NULL

            } else if (type == 'helmert') {

                contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)

                for (i in seq_len(nLevels-1)) {
                    p <- (1 / (nLevels - i + 1))
                    contrast[i,i] <- p * (nLevels - i)
                    contrast[(i+1):nLevels,i] <- -p
                }

            } else if (type == 'polynomial') {

                contrast <- stats::contr.poly(levels)
                dimnames(contrast) <- NULL

            } else if (type == 'repeated') {

                contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)
                for (i in seq_len(nLevels-1)) {
                    contrast[1:i,i] <- (nLevels-i) / nLevels
                    contrast[(i+1):nLevels,i] <- -i / nLevels
                }

            } else {

                contrast <- NULL
            }

            contrast
        },
        .modelTerms=function() {
            modelTerms <- self$options$modelTerms
            if (length(modelTerms) == 0)
                modelTerms <- private$.ff()

            lengths <- vapply(modelTerms, length, 1)
            modelTerms <- modelTerms[order(lengths)]

            modelTerms
        },
        .ff=function() {
            factors <- self$options$factors
            if (length(factors) > 1) {
                formula <- as.formula(paste('~', paste(paste0('`', factors, '`'), collapse='*')))
                terms   <- attr(stats::terms(formula), 'term.labels')
                modelTerms <- sapply(terms, function(x) as.list(strsplit(x, ':')), USE.NAMES=FALSE)
            } else {
                modelTerms <- as.list(factors)
            }

            for (i in seq_along(modelTerms)) {
                term <- modelTerms[[i]]
                quoted <- grepl('^`.*`$', term)
                term[quoted] <- substring(term[quoted], 2, nchar(term[quoted])-1)
                modelTerms[[i]] <- term
            }

            covs <- NULL
            if ('covs' %in% names(self$options))
                covs <- self$options$covs

            for (covariate in covs)
                modelTerms[[ length(modelTerms) + 1 ]] <- covariate

            modelTerms
        },
        .formula=function() {
            jmvcore:::composeFormula(self$options$dep, self$options$modelTerms)
        },
        .sourcifyOption = function(option) {

            name <- option$name
            value <- option$value

            if (option$name %in% c('factors', 'dep', 'covs', 'modelTerms'))
                return('')

            if (name == 'contrasts') {
                i <- 1
                while (i <= length(value)) {
                    item <- value[[i]]
                    if (item$type == 'none')
                        value[[i]] <- NULL
                    else
                        i <- i + 1
                }
                if (length(value) == 0)
                    return('')
            } else if (name == 'modelTerms') {
                if (base::identical(as.list(value), private$.ff()))
                    return('')
            } else if (name == 'postHoc') {
                if (length(value) == 0)
                    return('')
            }

            super$.sourcifyOption(option)
        },
        .emmPlotSize = function(emm) {

            data <- self$data

            levels <- list()
            for (i in seq_along(emm))
                levels[[ emm[i] ]] <- levels(data[[ emm[i] ]])

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
        .errorCheck = function(data) {

            dep <- self$options$dep
            factors <- self$options$factors

            if (is.factor(data[[dep]]))
                reject('Dependent variable must be numeric')

            for (factorName in factors) {
                lvls <- base::levels(data[[factorName]])
                if (length(lvls) == 1)
                    reject("Factor '{}' contains only a single level", factorName=factorName)
                else if (length(lvls) == 0)
                    reject("Factor '{}' contains no data", factorName=factorName)
            }

        },
        .cleanData=function() {

            dep <- self$options$dep
            factors <- self$options$factors

            covs <- NULL
            if ('covs' %in% names(self$options))
                covs <- self$options$covs

            data <- self$data

            if ( ! is.null(dep))
                data[[dep]] <- jmvcore::toNumeric(data[[dep]])

            for (factor in factors)
                data[[factor]] <- as.factor(data[[factor]])

            for (covariate in covs)
                data[[covariate]] <- jmvcore::toNumeric(data[[covariate]])

            data <- na.omit(data)

            data
        }),
    active=list(
        residuals=function() {
            dep <- self$options$dep
            factors <- self$options$factors
            modelTerms <- private$.modelTerms()

            if (is.null(dep) || length(factors) == 0 || length(modelTerms) == 0)
                return(NULL)

            data <- private$.cleanData()

            formula <- jmvcore::constructFormula(dep, modelTerms)
            formula <- stats::as.formula(formula)
            model <- stats::aov(formula, data)

            residuals <- rstandard(model)
            residuals <- data.frame(residuals=residuals, row.names=rownames(data))
            return(residuals)
        }
    )
)
