
#' @rdname jamovi
#' @importFrom vcd assocstats loddsratio
#' @importFrom vcdExtra GKgamma
#' @export
contTablesClass <- R6::R6Class(
  "contTablesClass",
  inherit=contTablesBase,
  private=list(
    .init=function() {

        rowVarName <- self$options$get('rows')
        colVarName <- self$options$get('cols')
        layerNames <- self$options$get('layers')

        freqs <- self$results$get('freqs')
        chiSq <- self$results$get('chiSq')
        nom   <- self$results$get('nom')
        odds  <- self$results$get('odds')
        gamma <- self$results$get('gamma')
        taub  <- self$results$get('taub')

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
            levels <- base::levels(self$data[[colVarName]])
        }
        else {
            superTitle <- '.'
            levels <- c('.', '.')
        }

        hasSubRows <- sum(self$options$get('exp'),
                          self$options$get('pcRow'),
                          self$options$get('pcCol'),
                          self$options$get('pcTot')) > 0

        subNames  <- c('[count]', '[expected]', '[pcRow]', '[pcCol]', '[pcTot]')
        subTitles <- c('Count', 'Expected count', '% within row', '% within column', '% of total')
        visible   <- c('(obs)', '(exp)', '(pcRow)', '(pcCol)', '(pcTot)')
        types     <- c('integer', 'number', 'number', 'number', 'number')
        formats   <- c('', '', 'pc', 'pc', 'pc')

        # iterate over the sub rows

        for (j in seq_along(subNames)) {
            subName <- subNames[[j]]
            if (j == 1)
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

        freqs$addColumn(
            name='.total[count]',
            title='Total',
            type='integer')

        # populate the first column with levels of the row variable

        values <- list()
        for (i in seq_along(subNames))
            values[[paste0('type', subNames[i])]] <- subTitles[i]

        rows <- private$.grid(incRows=TRUE)

        for (i in seq_len(nrow(rows))) {
            for (name in dimnames(rows)[[2]]) {
                value <- as.character(rows[i, name])
                if (value == '.total')
                    value <- 'Total'
                values[[name]] <- value
            }
            key <- paste0(rows[i,], collapse='`')
            freqs$addRow(rowKey=key, values=values)

            if (hasSubRows == FALSE && rows[i, ncol(rows)] == '.total')
                freqs$addFormat(rowNo=i, 1, Cell.BEGIN_END_GROUP)
        }

        rows <- private$.grid(incRows=FALSE)
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

        ciText <- paste0(self$options$get('ciWidth'), '% Confidence Intervals')
        odds$getColumn('cil[lo]')$setSuperTitle(ciText)
        odds$getColumn('ciu[lo]')$setSuperTitle(ciText)
        odds$getColumn('cil[f]')$setSuperTitle(ciText)
        odds$getColumn('ciu[f]')$setSuperTitle(ciText)
        gamma$getColumn('cil')$setSuperTitle(ciText)
        gamma$getColumn('ciu')$setSuperTitle(ciText)

    },
    .grid=function(incRows=FALSE) {

        rowVarName <- self$options$get('rows')
        layerNames <- self$options$get('layers')

        expand <- list()

        if (incRows) {
            if (is.null(rowVarName))
                expand[['.']] <- c('.', '. ', 'Total')
            else
                expand[[rowVarName]] <- c(base::levels(self$data[[rowVarName]]), '.total')
        }

        for (layerName in layerNames)
            expand[[layerName]] <- c(base::levels(self$data[[layerName]]), '.total')

        rows <- rev(expand.grid(expand))

        rows
    },
    .matrices=function() {

        matrices <- list()

        rowVarName <- self$options$get('rows')
        colVarName <- self$options$get('cols')
        countsName <- self$options$get('counts')
        layerNames <- rev(unlist(self$options$get('layers')))

        data <- jmvcore::select(self$data, c(layerNames, rowVarName, colVarName))

        if (is.null(countsName))
            x <- rep(1, nrow(data))
        else
            x <- jmvcore::toNumeric(self$data[[countsName]])

        nRows  <- base::nlevels(data[[rowVarName]])
        nCols  <- base::nlevels(data[[colVarName]])
        nCells <- nRows * nCols

        while(TRUE) {

            data <- jmvcore::select(self$data, c(layerNames, rowVarName, colVarName))
            table <- ftable(xtabs(x ~ ., data=data))

            if (nrow(table) == 0)
                return(matrices)

            for (i in seq(1, nrow(table), by=nRows)) {
                indices <- seq(i, length.out=nRows)
                matrices[[length(matrices)+1]] <- table[indices,]
            }

            if (length(layerNames) == 0)
                break()
            layerNames <- layerNames[-1]
        }

        matrices
    },
    .run=function() {

        rowVarName <- self$options$get('rows')
        colVarName <- self$options$get('cols')

        if (is.null(rowVarName) || is.null(colVarName))
            return()

        if (nlevels(self$data[[rowVarName]]) < 2)
            jmvcore::reject("Row variable '{}' contains less than 2 levels", code='', rowVarName)
        if (nlevels(self$data[[colVarName]]) < 2)
            jmvcore::reject("Column variable '{}' contains less than 2 levels", code='', colVarName)

        countName <- self$options$get('counts')
        if ( ! is.null(countName)) {
            countCol <- jmvcore::toNumeric(self$data[[countName]])
            if (any(countCol < 0, na.rm=TRUE))
                jmvcore::reject('Counts may not be negative')
            if (any(is.infinite(countCol)))
                jmvcore::reject('Counts may not be infinite')
        }

        freqs <- self$results$get('freqs')
        chiSq <- self$results$get('chiSq')
        nom   <- self$results$get('nom')
        odds  <- self$results$get('odds')
        gamma <- self$results$get('gamma')
        taub  <- self$results$get('taub')

        freqRowNo <- 1
        othRowNo <- 1

        mats <- private$.matrices()

        nRows  <- base::nlevels(self$data[[rowVarName]])
        nCols  <- base::nlevels(self$data[[colVarName]])
        nCells <- nRows * nCols

        ciWidth <- self$options$get('ciWidth') / 100

        for (mat in mats) {

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
            v1 <- rep(as.numeric(df$Var1), df$Freq)
            v2 <- rep(as.numeric(df$Var2), df$Freq)
            tau <- try(cor.test(v1, v2, method="kendall", conf.level=ciWidth))

            lor <- NULL
            fish <- NULL
            if (all(dim(mat) == 2)) {
                fish <- stats::fisher.test(mat, conf.level=ciWidth)
                lor <- vcd::loddsratio(mat)
            }

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

            if (base::inherits(test, 'try-error'))
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
                    `value[N]`=n)
            else
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
                    `value[N]`=n)
            chiSq$setRow(rowNo=othRowNo, values=values)

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
                    `v[f]`=fish$estimate,
                    `cil[f]`=fish$conf.int[1],
                    `ciu[f]`=fish$conf.int[2]))

            } else {
                odds$setRow(rowNo=othRowNo, list(
                    `v[lo]`=NaN, `cil[lo]`='', `ciu[lo]`='',
                    `v[f]`=NaN, `cil[f]`='', `ciu[f]`=''))
                odds$addFootnote(rowNo=othRowNo, 'v[lo]', 'Available for 2x2 tables only')
                odds$addFootnote(rowNo=othRowNo, 'v[f]', 'Available for 2x2 tables only')
            }

            othRowNo <- othRowNo + 1
        }

    })
)
