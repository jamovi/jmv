
#' @import ggplot2
ancovaClass <- R6::R6Class(
    "ancovaClass",
    inherit=ancovaBase,
    private=list(
        .model=NA,
        .postHocRows=NA,
        .cleanData=function() {

            dep <- self$options$dep
            factors <- self$options$factors
            covs <- self$options$cov

            data <- self$data

            if ( ! is.null(dep))
                data[[dep]] <- jmvcore::toNumeric(data[[dep]])

            for (factor in factors)
                data[[factor]] <- as.factor(data[[factor]])

            for (covariate in covs)
                data[[covariate]] <- jmvcore::toNumeric(data[[covariate]])

            data <- na.omit(data)

            data
        },
        .init=function() {

            dep <- self$options$dep
            factors <- self$options$factors
            modelTerms <- private$.modelTerms()

            if (length(factors) == 0 || length(modelTerms) == 0)
                return()

            data <- private$.cleanData()

            anovaTable    <- self$results$main
            postHocTables <- self$results$postHoc
            contrastsTables <- self$results$contrasts

            # main table

            modelTerms <- private$.modelTerms()
            if (length(modelTerms) > 0) {
                for (term in modelTerms)
                    anovaTable$addRow(rowKey=term, list(name=stringifyTerm(term)))
                anovaTable$addFormat(col=1, rowNo=1,                  format=Cell.BEGIN_GROUP)
                anovaTable$addFormat(col=1, rowNo=length(modelTerms), format=Cell.END_GROUP)
            } else {
                anovaTable$addRow(rowKey='.', list(name='.'))
                anovaTable$addFormat(col=1, rowKey='.', format=Cell.BEGIN_END_GROUP)
            }

            anovaTable$addRow(rowKey='', list(name='Residuals'))
            anovaTable$addFormat(col=1, rowKey='', format=Cell.BEGIN_END_GROUP)


            # contrasts

            for (contrast in self$options$contrasts) {
                if (contrast$type == 'none')
                    next()
                table <- contrastsTables$addItem(contrast)

                var <- data[[contrast$var]]
                levels <- base::levels(var)
                labels <- private$.contrastLabels(levels, contrast$type)

                for (label in labels)
                    table$addRow(rowKey=label, list(contrast=label))
            }


            # post hoc
            private$.initPostHoc(data)

            # descriptives

            descTable <- self$results$desc
            factorNames <- self$options$factors

            if (length(factorNames) > 0) {

                data <- select(data, rev(factorNames))
                al <- as.list(data)
                names(al) <- rev(paste0('f', seq_len(length(al))))
                ll <- sapply(al, base::levels, simplify=FALSE)
                ll$stringsAsFactors <- FALSE
                grid <- do.call(base::expand.grid, ll)
                grid <- rev(grid)

                for (i in seq_len(ncol(grid))) {
                    colName <- colnames(grid)[[i]]
                    descTable$addColumn(name=colName, title=factorNames[[i]], index=i)
                }

                for (rowNo in seq_len(nrow(grid))) {
                    row <- grid[rowNo,]
                    if ( ! is.list(row))
                        row <- list(f1=row)
                    descTable$addRow(rowKey=row, values=row)
                }
            }

            # descriptives plots
            private$.initDescPlots(data)

        },
        .run=function() {

            suppressWarnings({

            dep <- self$options$dep
            factors <- self$options$factors
            modelTerms <- private$.modelTerms()

            if (is.null(dep) || length(factors) == 0 || length(modelTerms) == 0)
                return()

            base::options(contrasts = c("contr.sum","contr.poly"))

            data <- private$.cleanData()

            data <- lapply(data, function(x) {
                if (is.factor(x))
                    levels(x) <- toB64(levels(x))
                return(x)
            })

            if (is.factor(data[[dep]]))
                reject('Dependent variable must be numeric')

            for (factorName in factors) {
                lvls <- base::levels(data[[factorName]])
                if (length(lvls) == 1)
                    reject("Factor '{}' contains only a single level", factorName=factorName)
                else if (length(lvls) == 0)
                    reject("Factor '{}' contains no data", factorName=factorName)
            }

            for (contrast in self$options$contrasts) {
               levels <- base::levels(data[[contrast$var]])
               stats::contrasts(data[[contrast$var]]) <- private$.createContrasts(levels, contrast$type)
            }

            formula <- jmvcore::constructFormula(dep, modelTerms)
            formula <- stats::as.formula(formula)

            private$.model <- stats::aov(formula, data)

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

            anovaTable <- self$results$main

            if ( ! is.null(singular))
                anovaTable$setNote('singular', singular)

            rowCount <- dim(results)[1]
            rowNames <- dimnames(results)[[1]]

            errIndex <- nrow(results)
            errSS <- results[errIndex,'Sum Sq']
            errDF <- results[errIndex,'Df']
            errMS <- errSS / errDF
            totalSS <- sum(results[['Sum Sq']], na.rm=TRUE)

            ss <- as.integer(self$options$ss)
            es <- lsr::etaSquared(private$.model, ss)

            for (i in seq_len(rowCount)) {
                rowName <- rowNames[i]

                ss <- results[i,'Sum Sq']
                df <- results[i,'Df']
                ms <- ss / df
                F  <- results[i,'F value']
                p  <- results[i,'Pr(>F)']

                if (i <= nrow(es)) {
                    e <- es[i, 'eta.sq']
                    ep <- es[i, 'eta.sq.part']
                    w <- (df*(ms-errMS))/(totalSS+errMS)
                } else {
                    e <- ''
                    ep <- ''
                    w <- ''
                }

                if ( ! is.finite(ss))
                    ss <- 0
                if ( ! is.finite(ms))
                    ms <- ''
                if ( ! is.finite(F))
                    F <- ''
                if ( ! is.finite(p))
                    p <- ''

                tableRow <- list(ss=ss, df=df, ms=ms, F=F, p=p, etaSq=e, etaSqP=ep, omegaSq=w)

                if (i < rowCount) {
                    anovaTable$setRow(rowNo=i, tableRow)
                }
                else {
                    if (rowCount < anovaTable$rowCount) {
                        blankRow <- list(ss=0, df=0, ms='', F='', p='', etaSq='', etaSqP='', omegaSq='')
                        for (j in seq(i, anovaTable$rowCount-1))
                            anovaTable$setRow(rowNo=j, blankRow)
                    }
                    anovaTable$setRow(rowKey='', tableRow) # residual
                }
            }

            private$.populateContrasts(data)
            private$.populateLevenes(data)
            private$.populatePostHoc(data)
            private$.prepareDescPlots(data)
            private$.populateDescriptives(data)

            }) # suppressWarnings
        },
        .initPostHoc=function(data) {

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
                table$addColumn(name='pscheffe', title='p<sub>sheffe</sub>', type='number', format='zto,pvalue', visible="(postHocCorr:scheffe)")
                table$addColumn(name='pbonferroni', title='p<sub>bonferroni</sub>', type='number', format='zto,pvalue', visible="(postHocCorr:bonf)")
                table$addColumn(name='pholm', title='p<sub>holm</sub>', type='number', format='zto,pvalue', visible="(postHocCorr:holm)")

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
            }
            private$.postHocRows <- postHocRows
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

                    referenceGrid <- lsmeans::lsmeans(private$.model, formula)
                    none <- summary(pairs(referenceGrid, adjust='none'))
                    tukey <- summary(pairs(referenceGrid, adjust='tukey'))
                    scheffe <- summary(pairs(referenceGrid, adjust='scheffe'))
                    bonferroni <- summary(pairs(referenceGrid, adjust='bonferroni'))
                    holm <- summary(pairs(referenceGrid, adjust='holm'))

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

                    table$setRow(rowNo=i, values=row)
                    private$.checkpoint()
                }
                table$setStatus('complete')
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

            dep <- self$options$dep
            factors <- self$options$factors
            rhs <- paste0('`', factors, '`', collapse=':')
            formula <- as.formula(paste0('`', dep, '`', '~', rhs))

            result <- car::leveneTest(formula, data, center="mean")

            table <- self$results$get('assump')$get('homo')

            table$setRow(rowNo=1, values=list(
                F=result[1,'F value'],
                df1=result[1,'Df'],
                df2=result[2,'Df'],
                p=result[1,'Pr(>F)']))
        },
        .populateDescriptives=function(data) {

            if ( ! self$options$descStats)
                return()

            descTable <- self$results$desc
            dep <- self$options$dep
            dependent <- data[[dep]]
            factorNames <- rev(self$options$factors)
            factors <- as.list(select(data, factorNames))

            means <- aggregate(dependent, by=factors, base::mean, drop=FALSE)
            sds    <- aggregate(dependent, by=factors, stats::sd, drop=FALSE)
            ns <- aggregate(dependent, by=factors, base::length, drop=FALSE)

            stat <- data.frame(mean=means$x, sd=sds$x, n=ns$x)

            for (i in seq_len(nrow(stat))) {
                values <- stat[i,]
                values[is.na(values)] <- NaN
                descTable$setRow(rowNo=i, values)
            }

        },
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
                contrast <- (dummy - coding) * nLevels

            } else if (type == 'deviation') {

                contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)

                for (i in seq_len(nLevels-1)) {
                    contrast[i+1, i] <- -1
                    contrast[1, i] <- 1
                }

            } else if (type == 'difference') {

                contrast <- stats::contr.helmert(levels)
                dimnames(contrast) <- NULL

            } else if (type == 'helmert') {

                contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)

                for (i in seq_len(nLevels-1)) {
                    p <- (1 / (nLevels - i + 1))
                    contrast[i,i] <- p * (nLevels - i)
                    contrast[(i+1):nLevels,i] <- -p
                }

                contrast

            } else if (type == 'polynomial') {

                contrast <- stats::contr.poly(levels)
                dimnames(contrast) <- NULL

            } else if (type == 'repeated') {

                contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)
                for (i in seq_len(nLevels-1)) {
                    contrast[i,  i] <- 1
                    contrast[i+1,i] <- -1
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

            for (covariate in self$options$cov)
                modelTerms[[ length(modelTerms) + 1 ]] <- covariate

            modelTerms
        },
        .initDescPlots=function(data) {
            isAxis <- ! is.null(self$options$plotHAxis)
            isMulti <- ! is.null(self$options$plotSepPlots)

            self$results$get('descPlot')$setVisible( ! isMulti && isAxis)
            self$results$get('descPlots')$setVisible(isMulti)

            if (isMulti) {

                sepPlotsName <- self$options$plotSepPlots
                sepPlotsVar <- data[[sepPlotsName]]
                sepPlotsLevels <- levels(sepPlotsVar)

                array <- self$results$descPlots

                for (level in sepPlotsLevels)
                    array$addItem(level)
            }
        },
        .prepareDescPlots=function(data) {

            depName <- self$options$dep
            groupName <- self$options$plotHAxis
            linesName <- self$options$plotSepLines
            plotsName <- self$options$plotSepPlots

            ciWidth   <- self$options$ciWidth
            errorBarType <- self$options$plotError

            if (length(depName) == 0 || length(groupName) == 0)
                return()

            by <- list()
            by[['group']] <- data[[groupName]]
            levels(by[['group']]) <- fromB64(levels(by[['group']]))

            if ( ! is.null(linesName)) {
                by[['lines']] <- data[[linesName]]
                levels(by[['lines']]) <- fromB64(levels(by[['lines']]))
            }

            if ( ! is.null(plotsName)) {
                by[['plots']] <- data[[plotsName]]
                levels(by[['plots']]) <- fromB64(levels(by[['plots']]))
            }

            dep <- data[[depName]]

            means <- aggregate(dep, by=by, mean, simplify=FALSE)
            ses   <- aggregate(dep, by=by, function(x) { sd(x) / sqrt(length(x)) }, simplify=FALSE)
            cis   <- aggregate(dep, by=by, function(x) { sd(x) / sqrt(length(x)) * qt(ciWidth / 200 + .5, length(x)-1) }, simplify=FALSE)

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

            if (self$options$plotError != 'none') {
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

            depName <- self$options$dep
            groupName <- self$options$plotHAxis
            linesName <- self$options$plotSepLines
            plotsName <- self$options$plotSepPlots

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

            if (self$options$plotError != 'none')
                dodge <- position_dodge(0.2)
            else
                dodge <- position_dodge(0)

            errorType <- ''
            if (self$options$plotError != 'none') {
                if (self$options$plotError == 'ci') {
                    ciWidth <- self$options$ciWidth
                    errorType <- paste0('(', ciWidth, '% CI)')
                } else {
                    errorType <- '(SE)'
                }
            }

            if ( ! is.null(linesName)) {

                p <- ggplot(data=image$state$data, aes(x=group, y=mean, group=lines, colour=lines)) +
                    geom_line(size=.8, position=dodge) +
                    labs(x=groupName, y="", colour=paste(linesName, errorType)) +
                    scale_y_continuous(limits=c(min(image$state$range), max(image$state$range))) +
                    the

                if (self$options$plotError != 'none')
                    p <- p + geom_errorbar(aes(x=group, ymin=lower, ymax=upper, width=.1, group=lines), size=.8, position=dodge)

                p <- p + geom_point(shape=21, fill='white', size=3, position=dodge)

                print(p)

            } else {

                p <- ggplot(data=image$state$data) +
                    labs(x=groupName, y="", colour=paste("", errorType)) +
                    scale_colour_manual(name=paste("", errorType), values=c(colour='#333333'), labels='') +
                    scale_y_continuous(limits=c(min(image$state$range), max(image$state$range))) +
                    the

                if (self$options$plotError != 'none')
                    p <- p + geom_errorbar(aes(x=group, ymin=lower, ymax=upper, colour='colour', width=.1), size=.8)

                p <- p + geom_point(aes(x=group, y=mean, colour='colour'), shape=21, fill='white', size=3)

                print(p)
            }

            TRUE
        },
        .qqPlot=function(image, ...) {

            dep <- self$options$dep
            factors <- self$options$factors
            modelTerms <- private$.modelTerms()

            if (is.null(dep) || length(factors) == 0 || length(modelTerms) == 0)
                return(FALSE)

            data <- private$.cleanData()

            formula <- jmvcore::constructFormula(dep, modelTerms)
            formula <- stats::as.formula(formula)
            model <- stats::aov(formula, data)

            residuals <- rstandard(model)
            df <- as.data.frame(qqnorm(residuals, plot.it=FALSE))

            the <- theme(
                text=element_text(size=16, colour='#333333'),
                plot.background=element_rect(fill='transparent', color=NA),
                panel.background=element_rect(fill='#E8E8E8'),
                plot.margin=margin(15, 15, 15, 15),
                axis.text.x=element_text(margin=margin(5,0,0,0)),
                axis.text.y=element_text(margin=margin(0,5,0,0)),
                axis.title.x=element_text(margin=margin(10,0,0,0)),
                axis.title.y=element_text(margin=margin(0,10,0,0)),
                plot.title=element_text(margin=margin(0, 0, 15, 0)))

            print(ggplot(data=df, aes(y=y, x=x)) +
                geom_point(aes(x=x,y=y), colour='#333333') +
                geom_abline(slope=1, intercept=0, colour='#333333') +
                xlab("Theoretical Quantiles") +
                ylab("Standardized Residuals") +
                the)

            TRUE
        },
        .sourcifyOption = function(option) {

            name <- option$name
            value <- option$value

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
        })
)
