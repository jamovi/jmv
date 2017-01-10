
#' @rdname jamovi
#' @export
PropTest2Class <- R6::R6Class(
    "PropTest2Class",
    inherit = PropTest2Base,
    private = list(
        .run = function() {

            table <- self$results$get('table')
            table$clearRows()
            private$.setup()

            rowNo <- 1

            hyp <- self$options$hypothesis
            if (hyp == 'notequal')
                hyp <- 'two.sided'

            for (var in self$options$vars) {
                result <- private$.counts(var)
                total  <- result$total

                if (length(result$counts) == 0) {
                    rowNo <- rowNo + 2
                    next()
                }

                for (count in result$counts) {

                    if ( ! is.na(count)) {
                        prop <- count / total
                        r <- try(binom.test(
                            x=count,
                            n=total,
                            p=self$options$testValue,
                            alternative=hyp,
                            conf.level=self$options$ciWidth/100))

                        if ( ! base::inherits(r, 'try-error')) {
                            p <- r$p.value
                            cil <- r$conf.int[1]
                            ciu <- r$conf.int[2]
                        } else {
                            p <- 'NaN'
                            cil <- ''
                            ciu <- ''
                        }

                        values = list(count=count, total=total, prop=prop, p=p, cil=cil, ciu=ciu)
                    } else {
                        values = list(count=NaN, total='', prop='', p='', cil='', ciu='')
                    }

                    table$setRow(rowNo=rowNo, values=values)
                    rowNo <- rowNo + 1
                }
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

            table <- self$results$get('table')
            initing <- nrow(self$data) == 0

            hyp <- self$options$hypothesis
            if (hyp == 'greater')
                op <- '>'
            else if (hyp == 'less')
                op <- '<'
            else
                op <- '\u2260'

            table$setNote('hyp', jmvcore::format('H\u2090 is proportion {} {}', op, self$options$testValue))
            table$getColumn('cil')$setSuperTitle(paste0(self$options$ciWidth, '% Confidence Interval'))
            table$getColumn('ciu')$setSuperTitle(paste0(self$options$ciWidth, '% Confidence Interval'))

            for (var in self$options$vars) {

                varData <- jmvcore::naOmit(self$data[[var]])

                if (self$options$areCounts) {
                    if (initing)
                        levels <- paste(1:2)
                    else if (length(varData) > 0)
                        levels <- paste(1:length(varData))
                    else
                        levels <- c('\u2026', '\u2026 ')
                } else {
                    levels <- base::levels(varData)
                    if (length(levels) == 0) {
                        if (initing)
                            levels <- c('\u2026', '\u2026 ')
                        else if (length(varData) == 0)
                            levels <- c('\u2026', '\u2026 ')
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
