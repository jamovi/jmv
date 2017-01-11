
#' @rdname jamovi
#' @export
propTestNClass <- R6::R6Class(
    "propTestNClass",
    inherit = propTestNBase,
    private = list(
        .run = function() {

            if (is.null(self$options$var))
                return()

            var <- self$data[[self$options$var]]

            if ( ! is.null(self$options$counts)) {
                countsData <- self$data[[self$options$counts]]
                if (jmvcore::canBeNumeric(countsData))
                    countsData <- jmvcore::toNumeric(countsData)
                else
                    countsData <- suppressWarnings(as.numeric(as.character(countsData)))

                data <- data.frame(var=var, counts=countsData)
                counts <- xtabs(counts ~ var, data=data)

            } else {

                counts <- table(var)
            }

            total <- sum(counts)

            table <- self$results$get('props')

            for (key in table$rowKeys) {
                if (key %in% names(counts)) {
                    count <- counts[[key]]
                    table$setRow(rowKey=key, values=list(count=count, prop=count/total))
                }
            }

            tests <- self$results$get('tests')
            result <- try(chisq.test(counts))

            if ( ! base::inherits(result, 'try-error')) {
                tests$setRow(rowNo=1, values=list(
                    chi=result$statistic,
                    df=result$parameter,
                    p=result$p.value))
            } else {
                tests$setRow(rowNo=1, values=list(
                    chi=NaN, df='', p=''))
            }

        })
)
