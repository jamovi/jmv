
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
            subTitles <- c('Observed', 'Expected', '% within row', '% within column', '% of total')
            visible   <- c('TRUE', '(exp)', '(pcRow)', '(pcCol)', '(pcTot)')
            types     <- c('integer', 'number', 'number', 'number', 'number')
            formats   <- c('', '', 'pc', 'pc', 'pc')

            # iterate over the sub rows

            for (j in seq_along(subNames)) {
                subName <- subNames[[j]]
                if (j == 1)
                    v <- '(exp || pcRow || pcCol || pcTot)'
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

            freqs$addColumn(
                name='.total[count]',
                title='Total',
                type='integer')

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
                        value <- 'Total'
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

            } else {

                for (i in seq_len(nrow(rows))) {

                    for (name in dimnames(rows)[[2]]) {
                        value <- as.character(rows[i, name])
                        if (value == '.total')
                            value <- 'Total'
                        values[[name]] <- value
                    }

                    chiSq$addRow(rowKey=i, values=values)
                    nom$addRow(rowKey=i, values=values)
                    odds$addRow(rowKey=i, values=values)
                    gamma$addRow(rowKey=i, values=values)
                    taub$addRow(rowKey=i, values=values)
                }
            }

            ciText <- paste0(self$options$ciWidth, '% Confidence Intervals')
            odds$getColumn('cil[lo]')$setSuperTitle(ciText)
            odds$getColumn('ciu[lo]')$setSuperTitle(ciText)
            odds$getColumn('cil[o]')$setSuperTitle(ciText)
            odds$getColumn('ciu[o]')$setSuperTitle(ciText)
            odds$getColumn('cil[rr]')$setSuperTitle(ciText)
            odds$getColumn('ciu[rr]')$setSuperTitle(ciText)
            gamma$getColumn('cil')$setSuperTitle(ciText)
            gamma$getColumn('ciu')$setSuperTitle(ciText)

        },
        .run=function() {

            rowVarName <- self$options$rows
            colVarName <- self$options$cols
            countsName <- self$options$counts

            if (is.null(rowVarName) || is.null(colVarName))
                return()

            data <- private$.cleanData()

            if (nlevels(data[[rowVarName]]) < 2)
                jmvcore::reject("Row variable '{}' contains less than 2 levels", code='', rowVarName)
            if (nlevels(data[[colVarName]]) < 2)
                jmvcore::reject("Column variable '{}' contains less than 2 levels", code='', colVarName)

            if ( ! is.null(countsName)) {
                countCol <- data[[countsName]]
                if (any(countCol < 0, na.rm=TRUE))
                    jmvcore::reject('Counts may not be negative')
                if (any(is.infinite(countCol)))
                    jmvcore::reject('Counts may not be infinite')
            }

            freqs <- self$results$freqs
            chiSq <- self$results$chiSq
            nom   <- self$results$nom
            odds  <- self$results$odds
            gamma <- self$results$gamma
            taub  <- self$results$taub

            freqRowNo <- 1
            othRowNo <- 1

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

                    df <- as.data.frame(as.table(mat))
                    v1 <- rep(as.numeric(df[[1]]), df$Freq)
                    v2 <- rep(as.numeric(df[[2]]), df$Freq)
                    tau <- try(cor.test(v1, v2, method="kendall", conf.level=ciWidth))

                    lor <- NULL
                    fish <- NULL
                    if (all(dim(mat) == 2)) {
                        fish <- stats::fisher.test(mat, conf.level=ciWidth)
                        lor <- vcd::loddsratio(mat)
                        rr <- private$.relativeRisk(mat)
                    }

                }) # suppressWarnings

                total <- sum(mat)
                colTotals <- apply(mat, 2, sum)

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

                    pcRow <- as.list(pcRow)
                    names(pcRow) <- paste0(1:nCols, '[pcRow]')

                    pcCol <- as.list(mat[rowNo,] / colTotals)
                    names(pcCol) <- paste0(1:nCols, '[pcCol]')

                    pcTot <- as.list(mat[rowNo,] / total)
                    names(pcTot) <- paste0(1:nCols, '[pcTot]')

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
                        `value[likeRat]`=NaN,
                        `df[likeRat]`='',
                        `p[likeRat]`='',
                        `value[fisher]`=NaN,
                        `p[fisher]`='',
                        `value[N]`=n)
                } else {

                    if (is.null(fish)) {
                        fishE <- NaN
                        fishP <- ''
                    } else {
                        fishE <- fish$estimate
                        fishP <- fish$p.value
                    }

                    values <- list(
                        `value[chiSq]`=unname(test$statistic),
                        `df[chiSq]`=unname(test$parameter),
                        `p[chiSq]`=unname(test$p.value),
                        `value[chiSqCorr]`=unname(corr$statistic),
                        `df[chiSqCorr]`=unname(corr$parameter),
                        `p[chiSqCorr]`=unname(corr$p.value),
                        `value[likeRat]`=asso$chisq_tests['Likelihood Ratio', 'X^2'],
                        `df[likeRat]`=asso$chisq_tests['Likelihood Ratio', 'df'],
                        `p[likeRat]`=asso$chisq_tests['Likelihood Ratio', 'P(> X^2)'],
                        `value[fisher]`=fishE,
                        `p[fisher]`=fishP,
                        `value[N]`=n)
                }

                chiSq$setRow(rowNo=othRowNo, values=values)

                if (is.null(fish))
                    chiSq$addFootnote(rowNo=othRowNo, 'value[fisher]', 'Available for 2x2 tables only')

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

                if (base::inherits(tau, 'try-error') || is.na(tau$estimate))
                    values <- list(taub=NaN, t='', p='')
                else
                    values <- list(
                        taub=tau$estimate,
                        t=unname(tau$statistic),
                        p=tau$p.value)
                taub$setRow(rowNo=othRowNo, values=values)

                if ( ! is.null(lor)) {
                    ci <- confint(lor, level=ciWidth)
                    odds$setRow(rowNo=othRowNo, list(
                        `v[lo]`=unname(lor[[1]]),
                        `cil[lo]`=ci[1],
                        `ciu[lo]`=ci[2],
                        `v[o]`=exp(unname(lor[[1]])),
                        `cil[o]`=exp(ci[1]),
                        `ciu[o]`=exp(ci[2]),
                        `v[rr]`=rr$rr,
                        `cil[rr]`=rr$lower,
                        `ciu[rr]`=rr$upper))

                } else {
                    odds$setRow(rowNo=othRowNo, list(
                        `v[lo]`=NaN, `cil[lo]`='', `ciu[lo]`='',
                        `v[o]`=NaN, `cil[o]`='', `ciu[o]`='',
                        `v[rr]`=NaN, `cil[rr]`='', `ciu[rr]`=''))
                    odds$addFootnote(rowNo=othRowNo, 'v[lo]', 'Available for 2x2 tables only')
                    odds$addFootnote(rowNo=othRowNo, 'v[o]', 'Available for 2x2 tables only')
                    odds$addFootnote(rowNo=othRowNo, 'v[rr]', 'Available for 2x2 tables only')
                }

                othRowNo <- othRowNo + 1
            }

        },

        #### Helper functions ----
        .cleanData = function() {

            data <- self$data

            rowVarName <- self$options$rows
            colVarName <- self$options$cols
            layerNames <- self$options$layers
            countsName <- self$options$counts

            if ( ! is.null(rowVarName))
                data[[rowVarName]] <- as.factor(data[[rowVarName]])
            if ( ! is.null(colVarName))
                data[[colVarName]] <- as.factor(data[[colVarName]])
            for (layerName in layerNames)
                data[[layerName]] <- as.factor(data[[layerName]])
            if ( ! is.null(countsName))
                data[[countsName]] <- toNumeric(data[[countsName]])

            data
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
                    expand[[layerName]] <- c(base::levels(data[[layerName]]))

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
                    expand[['.']] <- c('.', '. ', 'Total')
                else
                    expand[[rowVarName]] <- c(base::levels(data[[rowVarName]]), '.total')
            }

            for (layerName in layerNames)
                expand[[layerName]] <- c(base::levels(data[[layerName]]), '.total')

            rows <- rev(expand.grid(expand))

            rows
        },
        .relativeRisk = function(mat) {

            # https://en.wikipedia.org/wiki/Relative_risk#Tests

            dims <- dim(mat)

            if (dims[1] > 2 || dims[2] > 2)
                return(NULL)

            ciWidth <- self$options$ciWidth
            tail <- (100 - ciWidth) / 200
            z <- qnorm(tail, lower.tail = FALSE)

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

        })
)
