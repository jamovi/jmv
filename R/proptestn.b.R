
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

            ratio <- self$options$ratio
            if (is.null(ratio))
                expProps <- rep(1/length(counts), length(counts))
            else
                expProps <- ratio / sum(ratio)

            total <- sum(counts)

            table <- self$results$props

            keys <- table$rowKeys
            for (i in seq_along(keys)) {
                key <- keys[[i]]
                if (key %in% names(counts)) {
                    count <- counts[[key]]
                    expProp <- expProps[i]
                    values <- list(
                        `count[obs]`=count,
                        `prop[obs]`=count / total,
                        `count[exp]`=expProp * total,
                        `prop[exp]`=expProp)
                    table$setRow(rowKey=key, values=values)
                }
            }

            tests <- self$results$tests

            result <- try(chisq.test(counts, p=expProps))

            if ( ! base::inherits(result, 'try-error')) {
                tests$setRow(rowNo=1, values=list(
                    chi=result$statistic,
                    df=result$parameter,
                    p=result$p.value))
            } else {
                tests$setRow(rowNo=1, values=list(
                    chi=NaN, df='', p=''))
            }

        },
        .sourcifyOption = function(option) {
            if (option$name %in% c('var', 'counts'))
                return('')
            super$.sourcifyOption(option)
        },
        .formula=function() {
            jmvcore:::composeFormula(self$options$counts, self$options$var)
        })
)
