
contTablesPairedClass <- R6::R6Class(
    "contTablesPairedClass",
    inherit = contTablesPairedBase,
    private = list(
        .run = function() {

            rowVarName <- self$options$rows
            colVarName <- self$options$cols
            countName <- self$options$counts

            data <- jmvcore::select(self$data, c(rowVarName, colVarName, countName))
            data <- jmvcore::naOmit(data)

            if (is.null(rowVarName) || is.null(colVarName))
                return()

            if (nlevels(data[[rowVarName]]) < 2)
                jmvcore::reject("Row variable '{}' contains fewer than 2 levels", code='', rowVarName)
            if (nlevels(data[[colVarName]]) < 2)
                jmvcore::reject("Column variable '{}' contains fewer than 2 levels", code='', colVarName)

            if ( ! is.null(countName)) {
                countCol <- jmvcore::toNumeric(data[[countName]])

                if (any(countCol < 0, na.rm=TRUE))
                    jmvcore::reject('Counts may not be negative')
                if (any(is.infinite(countCol)))
                    jmvcore::reject('Counts may not be infinite')
            }

            rowVar <- data[[rowVarName]]
            colVar <- data[[colVarName]]

            freqs <- self$results$get('freqs')
            test  <- self$results$get('test')

            if (! is.null(countName))
                result <- stats::xtabs(countCol ~ rowVar + colVar)
            else
                result <- base::table(rowVar, colVar)

            colTotals <- apply(result, 2, base::sum)
            freqRowNo <- 1

            for (rowNo in seq_len(nrow(result))) {

                counts <- result[rowNo,]

                if (length(counts) > 0) {
                    rowTotal <- sum(counts)
                    pcRow <- counts / rowTotal
                    pcCol <- counts / colTotals

                    names(counts) <- paste0(seq_len(length(counts)), '[count]')
                    names(pcRow)  <- paste0(seq_len(length(counts)), '[pcRow]')
                    names(pcCol)  <- paste0(seq_len(length(counts)), '[pcCol]')
                    names(rowTotal)  <- '.total[count]'

                    freqs$setRow(rowNo=rowNo, values=c(counts, pcRow, pcCol, rowTotal))
                    freqRowNo <- freqRowNo + 1
                }
            }

            nCols <- length(colTotals)

            rowTotal <- base::sum(colTotals)
            values <- as.list(colTotals)
            names(values) <- paste0(1:nCols, '[count]')
            values[['.total[count]']] <- rowTotal

            pcRow <- colTotals / rowTotal
            pcRow <- as.list(pcRow)
            names(pcRow) <- paste0(1:nCols, '[pcRow]')

            pcCol <- rep(1, nCols)
            pcCol <- as.list(pcCol)
            names(pcCol) <- paste0(1:nCols, '[pcCol]')

            names(rowTotal)  <- '.total[count]'

            values <- c(values, pcRow, pcCol, rowTotal)

            freqs$setRow(freqRowNo, values=values)

            wocor <- try(stats::mcnemar.test(result, correct=FALSE), silent=TRUE)
            wcor  <- try(stats::mcnemar.test(result, correct=TRUE), silent=TRUE)

            values <- list()

            if (base::inherits(wocor, 'try-error') || is.na(wocor$statistic)) {
                values[['value[mcn]']] <- NaN
                values[['df[mcn]']] <- ''
                values[['p[mcn]']]  <- ''
            } else {
                values[['value[mcn]']] <- wocor$statistic
                values[['df[mcn]']] <- wocor$parameter
                values[['p[mcn]']]  <- wocor$p.value
            }

            if (base::inherits(wcor, 'try-error') || is.na(wcor$statistic)) {
                values[['value[cor]']] <- NaN
                values[['df[cor]']] <- ''
                values[['p[cor]']]  <- ''
            } else {
                values[['value[cor]']] <- wcor$statistic
                values[['df[cor]']] <- wcor$parameter
                values[['p[cor]']]  <- wcor$p.value
            }

            test$setRow(rowNo=1, values=values)

            if (base::inherits(wocor, 'try-error')) {
                error <- jmvcore::extractErrorMessage(wocor)
                if (error == "'x' must be square with at least two rows and columns")
                    error <- 'McNemar requires a 2x2 table'
                else if (error == "all entries of 'x' must be nonnegative and finite")
                    error <- 'Counts must be non-negative and finite'
                test$addFootnote(rowNo=1, 'value[mcn]', error)
            }

            if (base::inherits(wcor, 'try-error')) {
                error <- jmvcore::extractErrorMessage(wcor)
                if (error == "'x' must be square with at least two rows and columns")
                    error <- 'McNemar requires a 2x2 table'
                else if (error == "all entries of 'x' must be nonnegative and finite")
                    error <- 'Counts must be non-negative and finite'
                test$addFootnote(rowNo=1, 'value[cor]', error)
            }
        },
        .init = function() {

            freqs <- self$results$get('freqs')
            rowVarName <- self$options$rows
            colVarName <- self$options$cols

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

            hasSubRows <- sum(self$options$pcRow,
                              self$options$pcCol) > 0

            subNames  <- c('[count]', '[pcRow]', '[pcCol]')
            subTitles <- c('Count', '% within row', '% within column')
            visible   <- c('TRUE', '(pcRow)', '(pcCol)')
            types     <- c('integer', 'number', 'number')
            formats   <- c('', 'pc', 'pc')

            # iterate over the sub rows

            for (j in seq_along(subNames)) {
                subName <- subNames[[j]]
                if (j == 1)
                    v <- '(pcRow || pcCol)'
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

                if (i == 1)
                    freqs$addFormat(rowNo=i, 1, Cell.BEGIN_GROUP)
                else if (i == nrow(rows) - 1)
                    freqs$addFormat(rowNo=i, 1, Cell.END_GROUP)
                else if (i == nrow(rows))
                    freqs$addFormat(rowNo=i, 1, Cell.BEGIN_END_GROUP)
            }

            test <- self$results$get('test')
            test$addRow(rowKey=1, values=list())

        },
        .grid=function(incRows=FALSE) {

            rowVarName <- self$options$get('rows')

            expand <- list()

            if (incRows) {
                if (is.null(rowVarName))
                    expand[['.']] <- c('.', '. ', 'Total')
                else
                    expand[[rowVarName]] <- c(base::levels(self$data[[rowVarName]]), '.total')
            }

            rows <- rev(expand.grid(expand))

            rows
        })
)
