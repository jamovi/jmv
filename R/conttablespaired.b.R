
ContTablesPairedClass <- R6::R6Class(
    "ContTablesPairedClass",
    inherit = jmvcore::Analysis,
    private = list(
        .run = function() {

            tables <- self$results$get('tables')
            tests  <- self$results$get('tests')

            for (i in seq_along(self$options$pairs)) {

                pair <- self$options$pairs[[i]]
                if (is.null(pair[[1]]) || is.null(pair[[2]]))
                    next()

                table <- tables$get(pair)
                item1 <- pair[[1]]
                item2 <- pair[[2]]

                data <- jmvcore::select(self$data, pair)
                data <- jmvcore::naOmit(data)

                data1 <- data[[1]]
                if (ncol(data) == 1)  # variable paired with itself
                    data2 <- data[[1]]
                else
                    data2 <- data[[2]]

                if (self$options$areCounts) {

                    if (jmvcore::canBeNumeric(data1))
                        data1 <- jmvcore::toNumeric(data1)
                    else
                        data1 <- suppressWarnings(as.numeric(as.character(data1)))

                    if (jmvcore::canBeNumeric(data2))
                        data2 <- jmvcore::toNumeric(data2)
                    else
                        data2 <- suppressWarnings(as.numeric(as.character(data2)))

                    data  <- cbind(data1, data2)

                    result <- as.table(as.matrix(data))

                } else {
                    result <- base::table(data1, data2)
                }

                colTotals <- apply(result, 2, base::sum)

                for (rowNo in seq_len(nrow(result))) {

                    counts <- result[rowNo,]

                    if (length(counts) > 0) {
                        rowTotal <- sum(counts)
                        pcRow <- counts / rowTotal
                        pcCol <- counts / colTotals

                        names(counts) <- paste0(seq_len(length(counts)), '[count]')
                        names(pcRow)  <- paste0(seq_len(length(counts)), '[pcRow]')
                        names(pcCol)  <- paste0(seq_len(length(counts)), '[pcCol]')
                        table$setRow(rowNo=rowNo, values=c(counts, pcRow, pcCol))
                    }
                }

                wocor <- try(mcnemar.test(result, correct=FALSE), silent=TRUE)
                wcor  <- try(mcnemar.test(result, correct=TRUE), silent=TRUE)

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

                tests$setRow(rowNo=i, values=values)

                if (base::inherits(wocor, 'try-error')) {
                    error <- jmvcore::extractErrorMessage(wocor)
                    if (error == "'x' must be square with at least two rows and columns")
                        error <- 'McNemar requires a 2x2 table'
                    else if (error == "all entries of 'x' must be nonnegative and finite")
                        error <- 'Counts must be non-negative and finite'
                    tests$addFootnote(rowNo=i, 'value[mcn]', error)
                }

                if (base::inherits(wcor, 'try-error')) {
                    error <- jmvcore::extractErrorMessage(wcor)
                    if (error == "'x' must be square with at least two rows and columns")
                        error <- 'McNemar requires a 2x2 table'
                    else if (error == "all entries of 'x' must be nonnegative and finite")
                        error <- 'Counts must be non-negative and finite'
                    tests$addFootnote(rowNo=i, 'value[cor]', error)
                }
            }
        },
        .init = function() {

            tables <- self$results$get('tables')
            pairs <- self$options$pairs

            if (length(pairs) == 0) {
                pair <- list(NULL, NULL)
                pairs <- list(pair)
                tables$addItem(pair)
            }

            for (pair in pairs) {
                table <- tables$get(pair)
                item1 <- pair[[1]]
                item2 <- pair[[2]]

                if (is.null(item1)) {
                    item1 <- '…'
                    item1levels <- c('1', '2')
                } else if (self$options$areCounts) {
                    item1levels <- c('1', '2')
                } else {
                    item1levels <- base::levels(self$data[[item1]])
                    if (length(item1levels) == 0)
                        item1levels <- c('1', '2')
                }

                if (is.null(item2)) {
                    item2 <- '…'
                    item2levels <- c('1', '2')
                } else if (self$options$areCounts) {
                    item2levels <- c('1', '2')
                } else {
                    item2levels <- base::levels(self$data[[item2]])
                    if (length(item2levels) == 0)
                        item2levels <- c('1', '2')
                }

                table$setTitle(paste(item1, '-', item2))

                table$addColumn(
                    name='row',
                    title=item1,
                    type='text')

                subNames  <- c('[count]', '[pcRow]', '[pcCol]')
                subTitles <- c('Count', '% within row', '% within column')
                visible   <- c('(obs)', '(pcRow)', '(pcCol)')
                types     <- c('integer', 'number', 'number')
                formats   <- c('', 'pc', 'pc')

                # iterate over the sub rows

                for (j in seq_along(subNames)) {
                    subName <- subNames[[j]]
                    if (j == 1)
                        v <- '(pcRow || pcCol)'
                    else
                        v <- visible[j]

                    table$addColumn(
                        name=paste0('type', subName),
                        title='',
                        type='text',
                        visible=v)
                }

                for (i in seq_along(item2levels)) {
                    level <- item2levels[[i]]

                    for (j in seq_along(subNames)) {
                        subName <- subNames[[j]]
                        table$addColumn(
                            name=paste0(i, subName),
                            title=level,
                            superTitle=item2,
                            type=types[j],
                            format=formats[j],
                            visible=visible[j])
                    }
                }

                values <- list()
                for (i in seq_along(subNames))
                    values[[paste0('type', subNames[i])]] <- subTitles[i]

                for (level in item1levels)
                    table$addRow(rowKey=level, values=c(values, list(row=level)))
            }
        })
)
