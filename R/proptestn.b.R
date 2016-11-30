
PropTestNClass <- R6::R6Class(
    "PropTestNClass",
    inherit = PropTestNBase,
    private = list(
        .run = function() {

            table <- self$results$get('props')
            tests <- self$results$get('tests')
            table$clearRows()
            private$.setup()

            rowNo <- 1

            for (var in self$options$vars) {
                result <- private$.counts(var)
                total  <- result$total

                if (length(result$counts) > 0) {

                    for (count in result$counts) {

                        if ( ! is.na(count)) {
                            values = list(count=count, prop=count / total)
                        } else {
                            values = list(count=NaN, prop='')
                        }

                        table$setRow(rowNo=rowNo, values=values)
                        rowNo <- rowNo + 1
                    }

                } else {
                    rowNo <- rowNo + 2
                }

                chisq <- try(stats::chisq.test(result$counts))

                if ( ! base::inherits(chisq, 'try-error')) {
                    values <- list(
                        chi=chisq$statistic,
                        df=chisq$parameter,
                        p=chisq$p.value)

                } else {
                    values <- list(
                        chi=NaN,
                        df='',
                        p='')
                }

                tests$setRow(rowKey=var, values=values)
            }

        },
        .init = function() {
            private$.setup()
        },
        .counts=function(var) {

            initing <- nrow(self$data) == 0
            varData <- jmvcore::naOmit(self$data[[var]])

            if (self$options$areCounts) {

                if (jmvcore::canBeNumeric(varData))
                    counts <- jmvcore::toNumeric(varData)
                else
                    counts <- suppressWarnings(as.numeric(as.character(varData)))

            } else {
                counts <- as.vector(table(varData))
            }

            total <- base::sum(counts, na.rm=TRUE)
            list(counts=counts, total=total)
        },
        .setup=function() {

            table <- self$results$get('props')
            tests <- self$results$get('tests')
            initing <- nrow(self$data) == 0

            for (var in self$options$vars) {

                varData <- jmvcore::naOmit(self$data[[var]])

                if (self$options$areCounts) {
                    if (initing)
                        levels <- paste(1:2)
                    else if (length(varData) > 0)
                        levels <- paste(1:length(varData))
                    else
                        levels <- c('…', '… ')
                } else {
                    levels <- base::levels(varData)
                    if (length(levels) == 0) {
                        if (initing)
                            levels <- c('…', '… ')
                        else if (length(varData) == 0)
                            levels <- c('…', '… ')
                        else
                            levels <- paste(sort(unique(varData)))
                    }
                }

                for (level in levels) {
                    key <- paste0(var, '`', level)
                    table$addRow(rowKey=key, values=list(var=var, level=level))
                }

                table$addFormat(rowKey=paste0(var, '`', levels[1]), 'var', jmvcore::Cell.BEGIN_GROUP)
                table$addFormat(rowKey=paste0(var, '`', levels[length(levels)]), 'var', jmvcore::Cell.END_GROUP)
            }
        })
)
