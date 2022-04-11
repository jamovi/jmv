
#' @importFrom jmvcore .
contTablesClass <- R6::R6Class(
    "contTablesClass",
    inherit=contTablesBase,
    private=list(
        #### Init + run functions ----
        .init=function() {

            rowVarName <- self$options$rows
            colVarName <- self$options$cols
            layerNames <- self$options$layers
            countsName <- self$options$counts

            freqs <- self$results$freqs
            chiSq <- self$results$chiSq
            nom   <- self$results$nom
            odds  <- self$results$odds
            gamma <- self$results$gamma
            taub  <- self$results$taub
            mh  <- self$results$mh

            data <- private$.cleanData()

            reversed <- rev(layerNames)
            for (i in seq_along(reversed)) {
                layer <- reversed[[i]]
                freqs$addColumn(name=layer, type='text', combineBelow=TRUE)
                chiSq$addColumn(index=i, name=layer, type='text', combineBelow=TRUE)
                odds$addColumn(index=i, name=layer, type='text', combineBelow=TRUE)
                nom$addColumn(index=i, name=layer, type='text', combineBelow=TRUE)
                gamma$addColumn(index=i, name=layer, type='text', combineBelow=TRUE)
                taub$addColumn(index=i, name=layer, type='text', combineBelow=TRUE)
                mh$addColumn(index=i, name=layer, type='text', combineBelow=TRUE)
            }

            # add the row column, containing the row variable
            # fill in dots, if no row variable specified

            if ( ! is.null(rowVarName))
                title <- rowVarName
            else
                title <- '.'

            freqs$addColumn(
                name=title,
                title=title,
                type='text')

            # add the column columns (from the column variable)
            # fill in dots, if no column variable specified

            if ( ! is.null(colVarName)) {
                superTitle <- colVarName
                levels <- base::levels(data[[colVarName]])
            }
            else {
                superTitle <- '.'
                levels <- c('.', '.')
            }

            subNames  <- c('[count]', '[expected]', '[pcRow]', '[pcCol]', '[pcTot]')
            subTitles <- c(.('Observed'), .('Expected'), .('% within row'), .('% within column'), .('% of total'))
            visible   <- c('(obs)', '(exp)', '(pcRow)', '(pcCol)', '(pcTot)')
            types     <- c('integer', 'number', 'number', 'number', 'number')
            formats   <- c('', '', 'pc', 'pc', 'pc')

            # iterate over the sub rows

            for (j in seq_along(subNames)) {
                subName <- subNames[[j]]
                if (subName == '[count]')
                   v <- '(obs && (exp || pcRow || pcCol || pcTot))'
                else
                    v <- visible[j]

                freqs$addColumn(
                    name=paste0('type', subName),
                    title='',
                    type='text',
                    visible=v)
            }

            for (i in seq_along(levels)) {
                level <- levels[[i]]

                for (j in seq_along(subNames)) {
                    subName <- subNames[[j]]
                    freqs$addColumn(
                        name=paste0(i, subName),
                        title=level,
                        superTitle=superTitle,
                        type=types[j],
                        format=formats[j],
                        visible=visible[j])
                }
            }

            # add the Total column

            if (self$options$obs) {
                freqs$addColumn(
                    name='.total[count]',
                    title=.('Total'),
                    type='integer')
            }

            if (self$options$exp) {
                freqs$addColumn(
                    name='.total[exp]',
                    title=.('Total'),
                    type='number')
            }

            if (self$options$pcRow) {
                freqs$addColumn(
                    name='.total[pcRow]',
                    title=.('Total'),
                    type='number',
                    format='pc')
            }

            if (self$options$pcCol) {
                freqs$addColumn(
                    name='.total[pcCol]',
                    title=.('Total'),
                    type='number',
                    format='pc')
            }

            if (self$options$pcTot) {
                freqs$addColumn(
                    name='.total[pcTot]',
                    title=.('Total'),
                    type='number',
                    format='pc')
            }

            # populate the first column with levels of the row variable

            values <- list()
            for (i in seq_along(subNames))
                values[[paste0('type', subNames[i])]] <- subTitles[i]

            rows <- private$.grid(data=data, incRows=TRUE)

            nextIsNewGroup <- TRUE

            for (i in seq_len(nrow(rows))) {

                for (name in colnames(rows)) {
                    value <- as.character(rows[i, name])
                    if (value == '.total')
                        value <- .('Total')
                    values[[name]] <- value
                }

                key <- paste0(rows[i,], collapse='`')
                freqs$addRow(rowKey=key, values=values)

                if (nextIsNewGroup) {
                    freqs$addFormat(rowNo=i, 1, Cell.BEGIN_GROUP)
                    nextIsNewGroup <- FALSE
                }

                if (as.character(rows[i, name]) == '.total') {
                    freqs$addFormat(rowNo=i, 1, Cell.BEGIN_END_GROUP)
                    nextIsNewGroup <- TRUE
                    if (i > 1)
                        freqs$addFormat(rowNo=i - 1, 1, Cell.END_GROUP)
                }
            }

            rows <- private$.grid(data=data, incRows=FALSE)
            values <- list()

            if (length(rows) == 0) {

                chiSq$addRow(rowKey=1, values=list())
                nom$addRow(rowKey=1, values=list())
                odds$addRow(rowKey=1, values=list())
                gamma$addRow(rowKey=1, values=list())
                taub$addRow(rowKey=1, values=list())
                mh$addRow(rowKey=1, values=list())

            } else {

                for (i in seq_len(nrow(rows))) {

                    for (name in dimnames(rows)[[2]]) {
                        value <- as.character(rows[i, name])
                        if (value == '.total')
                            value <- .('Total')
                        values[[name]] <- value
                    }

                    chiSq$addRow(rowKey=i, values=values)
                    nom$addRow(rowKey=i, values=values)
                    odds$addRow(rowKey=i, values=values)
                    gamma$addRow(rowKey=i, values=values)
                    taub$addRow(rowKey=i, values=values)
                    mh$addRow(rowKey=i, values=values)
                }
            }

            ciText <- jmvcore::format(.('{ciWidth}% Confidence Intervals'), ciWidth=self$options$ciWidth)
            odds$getColumn('cil[dp]')$setSuperTitle(ciText)
            odds$getColumn('ciu[dp]')$setSuperTitle(ciText)
            odds$getColumn('cil[lo]')$setSuperTitle(ciText)
            odds$getColumn('ciu[lo]')$setSuperTitle(ciText)
            odds$getColumn('cil[o]')$setSuperTitle(ciText)
            odds$getColumn('ciu[o]')$setSuperTitle(ciText)
            odds$getColumn('cil[rr]')$setSuperTitle(ciText)
            odds$getColumn('ciu[rr]')$setSuperTitle(ciText)
            gamma$getColumn('cil')$setSuperTitle(ciText)
            gamma$getColumn('ciu')$setSuperTitle(ciText)

            private$.initBarPlot()
        },
        .run=function() {

            rowVarName <- self$options$rows
            colVarName <- self$options$cols
            countsName <- self$options$counts

            if (is.null(rowVarName) || is.null(colVarName))
                return()

            data <- private$.cleanData()

            if (nlevels(data[[rowVarName]]) < 2)
                jmvcore::reject(.("Row variable '{var}' contains fewer than 2 levels"), code='', var=rowVarName)
            if (nlevels(data[[colVarName]]) < 2)
                jmvcore::reject(.("Column variable '{var}' contains fewer than 2 levels"), code='', var=colVarName)

            if ( ! is.null(countsName)) {
                countCol <- data[[countsName]]
                if (any(countCol < 0, na.rm=TRUE))
                    jmvcore::reject(.('Counts may not be negative'))
                if (any(is.infinite(countCol)))
                    jmvcore::reject(.('Counts may not be infinite'))
            }

            freqs <- self$results$freqs
            chiSq <- self$results$chiSq
            nom   <- self$results$nom
            odds  <- self$results$odds
            gamma <- self$results$gamma
            taub  <- self$results$taub
            mh    <- self$results$mh

            freqRowNo <- 1
            othRowNo <- 1

            # get group names according to compare
            groups <- NULL
            variable <- NULL
            if (self$options$compare == "rows") {
                if (!is.null(rowVarName)) {
                    variable <- rowVarName
                    groups <- base::levels(data[[rowVarName]])
                } else {
                    groups <- c(.('Group 1'), .('Group 2'))
                }
            } else { # compare columns
                if (!is.null(colVarName)) {
                    variable <- colVarName
                    groups <- base::levels(data[[colVarName]])
                } else {
                    groups <- c(.('Group 1'), .('Group 2'))
                }
            }

            hypothesis <- self$options$hypothesis

            ## Hypothesis options checking
            if (self$options$hypothesis == 'oneGreater')
                Ha <- "greater"
            else if (self$options$hypothesis == 'twoGreater')
                Ha <- "less"
            else
                Ha <- "two.sided"

            mats <- private$.matrices(data)

            nRows  <- base::nlevels(data[[rowVarName]])
            nCols  <- base::nlevels(data[[colVarName]])
            nCells <- nRows * nCols

            ciWidth <- self$options$ciWidth / 100

            for (mat in mats) {

                suppressWarnings({

                    test <- try(chisq.test(mat, correct=FALSE))
                    corr <- try(chisq.test(mat, correct=TRUE))
                    asso <- vcd::assocstats(mat)
                    gamm <- vcdExtra::GKgamma(mat)
                    n <- sum(mat)

                    if (base::inherits(test, 'try-error'))
                        exp <- mat
                    else
                        exp <- test$expected

                    if (self$options$taub || self$options$mh) {
                        df <- as.data.frame(as.table(mat))
                        v1 <- rep(as.numeric(df[[1]]), df$Freq)
                        v2 <- rep(as.numeric(df[[2]]), df$Freq)

                        if (self$options$taub) {
                            # this can be slow
                            tau <- try(cor.test(v1, v2, method='kendall', conf.level=ciWidth))
                        }
                        if (self$options$mh) {
                            if (all(dim(mat) > 2))
                                mhchi2 <- -1 # Mantel-Haenszel is only for 2xk tables
                            else
                                mhchi2 <- try((cor(v1, v2)^2) * (sum(df$Freq) - 1))
                        }
                    }

                    zP <- NULL
                    dp <- NULL
                    lor <- NULL
                    fish <- try(stats::fisher.test(mat, conf.level=ciWidth, alternative=Ha), silent=TRUE)
                    if (all(dim(mat) == 2) && all(rowSums(mat) > 0) && all(colSums(mat) > 0)) {
                        dp <- private$.diffProp(mat, Ha)
                        lor <- vcd::loddsratio(mat)
                        rr <- private$.relativeRisk(mat)
                        zP <- dp[[1]]
                    }

                }) # suppressWarnings

                total <- sum(mat)
                colTotals <- apply(mat, 2, sum)
                rowTotals <- apply(mat, 1, sum)

                for (rowNo in seq_len(nRows)) {

                    values <- mat[rowNo,]
                    rowTotal <- sum(values)

                    pcRow <- values / rowTotal

                    values <- as.list(values)
                    names(values) <- paste0(1:nCols, '[count]')
                    values[['.total[count]']] <- rowTotal

                    expValues <- exp[rowNo,]
                    expValues <- as.list(expValues)
                    names(expValues) <- paste0(1:nCols, '[expected]')
                    expValues[['.total[exp]']] <- sum(exp[rowNo,])

                    pcRow <- as.list(pcRow)
                    names(pcRow) <- paste0(1:nCols, '[pcRow]')
                    pcRow[['.total[pcRow]']] <- 1

                    pcCol <- as.list(mat[rowNo,] / colTotals)
                    names(pcCol) <- paste0(1:nCols, '[pcCol]')
                    pcCol[['.total[pcCol]']] <- unname(rowTotals[rowNo] / total)

                    pcTot <- as.list(mat[rowNo,] / total)
                    names(pcTot) <- paste0(1:nCols, '[pcTot]')
                    pcTot[['.total[pcTot]']] <- sum(mat[rowNo,] / total)

                    values <- c(values, expValues, pcRow, pcCol, pcTot)

                    freqs$setRow(rowNo=freqRowNo, values=values)
                    freqRowNo <- freqRowNo + 1
                }

                values <- apply(mat, 2, sum)
                rowTotal <- sum(values)
                values <- as.list(values)
                names(values) <- paste0(1:nCols, '[count]')
                values[['.total[count]']] <- rowTotal

                expValues <- apply(mat, 2, sum)
                expValues <- as.list(expValues)
                names(expValues) <- paste0(1:nCols, '[expected]')

                pcRow <- apply(mat, 2, sum) / rowTotal
                pcRow <- as.list(pcRow)
                names(pcRow) <- paste0(1:nCols, '[pcRow]')

                pcCol <- rep(1, nCols)
                pcCol <- as.list(pcCol)
                names(pcCol) <- paste0(1:nCols, '[pcCol]')

                pcTot <- apply(mat, 2, sum) / total
                pcTot <- as.list(pcTot)
                names(pcTot) <- paste0(1:nCols, '[pcTot]')

                expValues[['.total[exp]']] <- total
                pcRow[['.total[pcRow]']] <- 1
                pcCol[['.total[pcCol]']] <- 1
                pcTot[['.total[pcTot]']] <- 1

                values <- c(values, expValues, pcRow, pcCol, pcTot)

                freqs$setRow(rowNo=freqRowNo, values=values)
                freqRowNo <- freqRowNo + 1

                # populate chi squared table

                if (base::inherits(test, 'try-error')) {
                    values <- list(
                        `value[chiSq]`=NaN,
                        `df[chiSq]`='',
                        `p[chiSq]`='',
                        `value[chiSqCorr]`=NaN,
                        `df[chiSqCorr]`='',
                        `p[chiSqCorr]`='',
                        `value[zProp]`=NaN,
                        `df[zProp]`='',
                        `p[zProp]`='',
                        `value[likeRat]`=NaN,
                        `df[likeRat]`='',
                        `p[likeRat]`='',
                        `value[fisher]`='',
                        `p[fisher]`='',
                        `value[N]`=n)
                } else {

                    if (inherits(fish, 'htest')) {
                        fishP <- fish$p.value
                    } else {
                        fishP <- ''
                    }

                    if (is.null(zP)) {
                        zPstat <- NaN
                        zPpval <- ''
                    } else {
                        zPstat <- sqrt(dp$statistic) * sign(zP)
                        zPpval <- dp$p.value
                    }

                    values <- list(
                        `value[chiSq]`=unname(test$statistic),
                        `df[chiSq]`=unname(test$parameter),
                        `p[chiSq]`=unname(test$p.value),
                        `value[chiSqCorr]`=unname(corr$statistic),
                        `df[chiSqCorr]`=unname(corr$parameter),
                        `p[chiSqCorr]`=unname(corr$p.value),
                        `value[zProp]`=zPstat,
                        `df[zProp]`='', # needed to keep table entry order
                        `p[zProp]`=zPpval,
                        `value[likeRat]`=asso$chisq_tests['Likelihood Ratio', 'X^2'],
                        `df[likeRat]`=asso$chisq_tests['Likelihood Ratio', 'df'],
                        `p[likeRat]`=asso$chisq_tests['Likelihood Ratio', 'P(> X^2)'],
                        `value[fisher]`='',
                        `p[fisher]`=fishP,
                        `value[N]`=n)
                }

                chiSq$setRow(rowNo=othRowNo, values=values)

                hypothesisTested <- ''
                if (hypothesis == 'oneGreater')
                    hypothesisTested <- jmvcore::format("H\u2090: {} P({}) > P({})", variable, groups[1], groups[2])
                else if (hypothesis == 'twoGreater')
                    hypothesisTested <- jmvcore::format("H\u2090: {} P({}) < P({})", variable, groups[1], groups[2])
                else
                    hypothesisTested <- 'two-sided'

                if (is.null(zP))
                    chiSq$addFootnote(rowNo=othRowNo, 'value[zProp]', .('z test only available for 2x2 tables'))
                else if (hypothesis!="different")
                    chiSq$addFootnote(rowNo=othRowNo, 'p[zProp]', hypothesisTested)

                if (inherits(fish, 'htest') && all(dim(mat) == 2) && hypothesis != "different")
                    chiSq$addFootnote(rowNo=othRowNo, 'p[fisher]', hypothesisTested)

                values <- list(
                    `v[cont]`=asso$contingency,
                    `v[phi]`=ifelse(is.na(asso$phi), NaN, asso$phi),
                    `v[cra]`=asso$cramer)
                nom$setRow(rowNo=othRowNo, values=values)

                values <- list(
                    gamma=gamm$gamma,
                    se=gamm$sigma,
                    cil=gamm$CI[1],
                    ciu=gamm$CI[2])
                gamma$setRow(rowNo=othRowNo, values=values)

                if (self$options$taub) {
                    if (base::inherits(tau, 'try-error') || is.na(tau$estimate))
                        values <- list(taub=NaN, t='', p='')
                    else
                        values <- list(
                            taub=tau$estimate,
                            t=unname(tau$statistic),
                            p=tau$p.value)
                    taub$setRow(rowNo=othRowNo, values=values)
                }

                if (self$options$mh) {
                    if (base::inherits(mhchi2, 'try-error') || is.na(mhchi2) || mhchi2 == -1)
                        values <- list(chi2=NaN, df='', p='')
                    else
                        values <- list(chi2=mhchi2, df=1, p=1-pchisq(mhchi2,1))

                    mh$setRow(rowNo=othRowNo, values=values)

                    if (base::inherits(mhchi2, 'try-error') || is.na(mhchi2))
                        mh$addFootnote(rowNo=othRowNo, 'chi2', .('Variables must have at least two levels'))
                    else if (mhchi2 == -1)
                        mh$addFootnote(rowNo=othRowNo, 'chi2', .('At least one variable must have two levels'))
                }

                if ( ! is.null(lor)) {
                    ci <- confint(lor, level=ciWidth)
                    odds$setRow(rowNo=othRowNo, list(
                        `v[dp]`=dp$dp,
                        `cil[dp]`=dp$lower,
                        `ciu[dp]`=dp$upper,
                        `v[lo]`=unname(lor[[1]]),
                        `cil[lo]`=ci[1],
                        `ciu[lo]`=ci[2],
                        `v[o]`=exp(unname(lor[[1]])),
                        `cil[o]`=exp(ci[1]),
                        `ciu[o]`=exp(ci[2]),
                        `v[rr]`=rr$rr,
                        `cil[rr]`=rr$lower,
                        `ciu[rr]`=rr$upper))

                    footnote <- `if`(self$options$compare == 'rows', .('Rows compared'), .('Columns compared'))
                    odds$addFootnote(rowNo=othRowNo, 'v[dp]', footnote)
                    odds$addFootnote(rowNo=othRowNo, 'v[rr]', footnote)

                    if (any(mat == 0)){
                        odds$addFootnote(rowNo=othRowNo, 'v[lo]', .('Haldane-Anscombe correction applied'))
                        odds$addFootnote(rowNo=othRowNo, 'v[o]', .('Haldane-Anscombe correction applied'))
                    }
                } else {
                    odds$setRow(rowNo=othRowNo, list(
                        `v[dp]`=NaN, `cil[dp]`='', `ciu[dp]`='',
                        `v[lo]`=NaN, `cil[lo]`='', `ciu[lo]`='',
                        `v[o]`=NaN, `cil[o]`='', `ciu[o]`='',
                        `v[rr]`=NaN, `cil[rr]`='', `ciu[rr]`=''))
                    odds$addFootnote(rowNo=othRowNo, 'v[dp]', .('Available for 2x2 tables only'))
                    odds$addFootnote(rowNo=othRowNo, 'v[lo]', .('Available for 2x2 tables only'))
                    odds$addFootnote(rowNo=othRowNo, 'v[o]', .('Available for 2x2 tables only'))
                    odds$addFootnote(rowNo=othRowNo, 'v[rr]', .('Available for 2x2 tables only'))
                }

                othRowNo <- othRowNo + 1
            }
        },

        #### Plot functions ----
        .initBarPlot = function() {
            image <- self$results$get('barplot')

            width <- 450
            height <- 400

            layerNames <- self$options$layers
            if (length(layerNames) == 1)
                image$setSize(width * 2, height)
            else if (length(layerNames) >= 2)
                image$setSize(width * 2, height * 2)
        },
        .barPlot = function(image, ggtheme, theme, ...) {

            if (! self$options$barplot)
                return()

            rowVarName <- self$options$rows
            if (! is.null(rowVarName))
                rowVarName <- jmvcore::toB64(rowVarName)

            colVarName <- self$options$cols
            if (! is.null(colVarName))
                colVarName <- jmvcore::toB64(colVarName)

            countsName <- self$options$counts
            if (! is.null(countsName))
                countsName <- jmvcore::toB64(countsName)

            layerNames <- self$options$layers
            if (length(layerNames) > 0)
                layerNames <- jmvcore::toB64(layerNames)
            if (length(layerNames) > 2)
                layerNames <- layerNames[1:2] # max 2

            if (is.null(rowVarName) || is.null(colVarName))
                return()

            data <- private$.cleanData(B64 = TRUE)
            data <- na.omit(data)

            if (! is.null(countsName)){
                untable <- function (df, counts) df[rep(1:nrow(df), counts), ]
                data <- untable(data[, c(rowVarName, colVarName, layerNames)], counts=data[, countsName])
            }

            formula <- jmvcore::composeFormula(NULL, c(rowVarName, colVarName, layerNames))
            counts <- xtabs(formula, data)
            d <- dim(counts)

            expand <- list()
            for (i in c(rowVarName, colVarName, layerNames))
                expand[[i]] <- base::levels(data[[i]])
            tab <- expand.grid(expand)
            tab$Counts <- as.numeric(counts)

            if (self$options$yaxis == "ypc") { # percentages
                props <- counts

                if (self$options$yaxisPc == "column_pc") {
                    pctVarName <- colVarName
                } else if (self$options$yaxisPc == "row_pc") {
                    pctVarName <- rowVarName
                } else { # total
                    pctVarName <- NULL
                }

                if (length(layerNames) == 0) {
                    props <- proportions(counts, pctVarName)
                } else if (length(layerNames) == 1) {
                    for (i in seq.int(1, d[3], 1)) {
                        props[,,i] <- proportions(counts[,,i], pctVarName)
                    }
                } else { # 2 layers
                    for (i in seq.int(1, d[3], 1)) {
                        for (j in seq.int(1, d[4], 1)) {
                            props[,,i,j] <- proportions(counts[,,i,j], pctVarName)
                        }
                    }
                }

                tab$Percentages <- as.numeric(props) * 100
            }

            if (self$options$xaxis == "xcols") {
                xVarName <- ensym(colVarName)
                zVarName <- ensym(rowVarName)
            } else {
                xVarName <- ensym(rowVarName)
                zVarName <- ensym(colVarName)
            }

            position <- self$options$bartype

            if (self$options$yaxis == "ycounts") {
                p <- ggplot(data=tab, aes(y=Counts, x=!!xVarName, fill=!!zVarName)) +
                    geom_col(position=position, width = 0.7) +
                    labs(y = .("Counts"))
            } else {
                p <- ggplot(data=tab, aes(y=Percentages, x=!!xVarName, fill=!!zVarName)) +
                    geom_col(position=position, width = 0.7)

                if (self$options$yaxisPc == "total_pc") {
                    p <- p + labs(y = .("Percentages of total"))
                } else {
                    p <- p + labs(y = jmvcore::format(.("Percentages within {var}"), var=jmvcore::fromB64(pctVarName)))
                }
            }

            p <- p + labs(x=jmvcore::fromB64(xVarName), fill=jmvcore::fromB64(zVarName))

            if (length(layerNames) > 0) {
                if (length(layerNames) == 1)
                    layers <- as.formula(jmvcore::composeFormula(NULL, layerNames))
                else
                    layers <- as.formula(jmvcore::composeFormula(layerNames[1], layerNames[2]))

                p <- p + facet_grid(layers)
            }
            p <- p + ggtheme

            return(p)
        },

        #### Helper functions ----
        .cleanData = function(B64 = FALSE) {

            data <- self$data

            rowVarName <- self$options$rows
            colVarName <- self$options$cols
            layerNames <- self$options$layers
            countsName <- self$options$counts

            if ( ! is.null(rowVarName)) {
                rowVarNameNew <- ifelse(B64, jmvcore::toB64(rowVarName), rowVarName)
                data[[rowVarNameNew]] <- as.factor(data[[rowVarName]])
            }
            if ( ! is.null(colVarName)) {
                colVarNameNew <- ifelse(B64, jmvcore::toB64(colVarName), colVarName)
                data[[colVarNameNew]] <- as.factor(data[[colVarName]])
            }
            for (layerName in layerNames) {
                layerNameNew <- ifelse(B64, jmvcore::toB64(layerName), layerName)
                data[[layerNameNew]] <- as.factor(data[[layerName]])
            }
            if ( ! is.null(countsName)) {
                countsNameNew <- ifelse(B64, jmvcore::toB64(countsName), countsName)
                data[[countsNameNew]] <- jmvcore::toNumeric(data[[countsName]])
            }

            return(data)
        },
        .matrices=function(data) {

            matrices <- list()

            rowVarName <- self$options$rows
            colVarName <- self$options$cols
            layerNames <- self$options$layers
            countsName <- self$options$counts

            if (length(layerNames) == 0) {

                subData <- jmvcore::select(data, c(rowVarName, colVarName))

                if (is.null(countsName))
                    .COUNTS <- rep(1, nrow(subData))
                else
                    .COUNTS <- jmvcore::toNumeric(data[[countsName]])

                matrices <- list(ftable(xtabs(.COUNTS ~ ., data=subData)))

            } else {

                layerData <- jmvcore::select(data, layerNames)
                dataList <- do.call(split, list(data, layerData))

                tables <- lapply(dataList, function(x) {

                    xTemp <- jmvcore::select(x, c(rowVarName, colVarName))

                    if (is.null(countsName))
                        .COUNTS <- rep(1, nrow(xTemp))
                    else
                        .COUNTS <- jmvcore::toNumeric(x[[countsName]])

                    ftable(xtabs(.COUNTS ~ ., data=xTemp))
                })

                rows <- private$.grid(data=data, incRows=FALSE)

                expand <- list()

                for (layerName in layerNames)
                    expand[[layerName]] <- base::levels(data[[layerName]])

                tableNames <- rev(expand.grid(expand))

                matrices <- list()
                for (i in seq_along(rows[,1])) {

                    indices <- c()
                    for (j in seq_along(tableNames[,1])) {

                        row <- as.character(unlist((rows[i,])))
                        tableName <- as.character(unlist(tableNames[j,]))

                        if (all(row == tableName | row == '.total'))
                            indices <- c(indices, j)
                    }

                    matrices[[i]] <- Reduce("+", tables[indices])
                }

            }

            matrices
        },
        .grid=function(data, incRows=FALSE) {

            rowVarName <- self$options$rows
            layerNames <- self$options$layers

            expand <- list()

            if (incRows) {
                if (is.null(rowVarName))
                    expand[['.']] <- c('.', '. ', .('Total'))
                else
                    expand[[rowVarName]] <- c(base::levels(data[[rowVarName]]), '.total')
            }

            for (layerName in layerNames)
                expand[[layerName]] <- c(base::levels(data[[layerName]]), '.total')

            rows <- rev(expand.grid(expand))

            rows
        },
        .diffProp = function(mat, Ha) {

            dims <- dim(mat)

            if (dims[1] > 2 || dims[2] > 2)
                return(NULL)

            ciWidth <- self$options$ciWidth / 100

            if (self$options$compare == "columns")
                mat <- t(mat)

            a <- mat[1,1]
            b <- mat[1,2]
            c <- mat[2,1]
            d <- mat[2,2]

            p1 <- a / (a + b)
            p2 <- c / (c + d)

            dp <- p1 - p2
            prtest <- stats::prop.test(mat, conf.level=ciWidth, correct=FALSE, alternative=Ha)
            ci <-prtest$conf.int
            lower <- ci[1]
            upper <- ci[2]

            return(list(dp=dp, lower=lower, upper=upper,
                        p.value=prtest$p.value, statistic=prtest$statistic))

        },
        .relativeRisk = function(mat) {

            # https://en.wikipedia.org/wiki/Relative_risk#Tests

            dims <- dim(mat)

            if (dims[1] > 2 || dims[2] > 2)
                return(NULL)

            ciWidth <- self$options$ciWidth
            tail <- (100 - ciWidth) / 200
            z <- qnorm(tail, lower.tail = FALSE)

            if (self$options$compare == "columns")
                mat <- t(mat)

            a <- mat[1,1]
            b <- mat[1,2]
            c <- mat[2,1]
            d <- mat[2,2]

            p1 <- a / (a + b)
            p2 <- c / (c + d)

            m <- log(p1 / p2)
            s <- sqrt((b / (a*(a+b))) + (d / (c*(c+d))))
            lower <- exp(m - z*s)
            upper <- exp(m + z*s)

            rr <- p1 / p2

            return(list(rr=rr, lower=lower, upper=upper))

        },
        .sourcifyOption = function(option) {
            if (option$name %in% c('rows', 'cols', 'counts'))
                return('')
            super$.sourcifyOption(option)
        },
        .formula=function() {
            rhs <- list()
            if ( ! is.null(self$options$rows)) {
                rhs[[1]] <- self$options$rows
                if ( ! is.null(self$options$cols)) {
                    rhs[[2]] <- self$options$cols
                    rhs <- c(rhs, self$options$layers)
                }
            }
            jmvcore:::composeFormula(self$options$counts, list(rhs))
        }
    )
)
