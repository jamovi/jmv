
descriptivesClass <- R6::R6Class(
    "descriptivesClass",
    inherit=descriptivesBase,
    private=list(
        .run=function() {

            dataset <- self$data
            vars <- self$options$get('vars')

            desc <- self$results$get("descriptives")
            freq <- self$results$get("frequencies")
            freqPlots <- self$results$get('freqPlots')

            for (i in seq_along(vars)) {

                name   <- vars[[i]]
                column <- dataset[[name]]

                total <- length(column)
                column <- jmvcore::naOmit(column)
                n <- length(column)

                desc$setCell(rowNo=i, "n", n)
                desc$setCell(rowNo=i, "missing", total - n)

                if (jmvcore::canBeNumeric(column) && n > 0) {

                    numColumn <- jmvcore::toNumeric(column)

                    desc$setCell(rowNo=i, "mean", mean(numColumn))
                    desc$setCell(rowNo=i, "median", median(numColumn))

                    mode <- as.numeric(names(table(numColumn)[table(numColumn)==max(table(numColumn))]))
                    desc$setCell(rowNo=i, "mode", mode[1])

                    desc$setCell(rowNo=i, "sum", sum(numColumn))
                    desc$setCell(rowNo=i, "sd", sd(numColumn))
                    desc$setCell(rowNo=i, "variance", var(numColumn))
                    desc$setCell(rowNo=i, "range", max(numColumn)-min(numColumn))
                    desc$setCell(rowNo=i, "min", min(numColumn))
                    desc$setCell(rowNo=i, "max", max(numColumn))
                    desc$setCell(rowNo=i, "se", sqrt(var(numColumn)/length(numColumn)))

                    deviation <- numColumn-mean(numColumn)
                    desc$setCell(rowNo=i, "skew", sum(deviation^3)/(length(numColumn)*sd(numColumn)^3))
                    desc$setCell(rowNo=i, "kurt", sum(deviation^4)/(length(numColumn)*var(numColumn)^2))

                    desc$setCell(rowNo=i, "quart1", quantile(numColumn, c(.25)))
                    desc$setCell(rowNo=i, "quart2", quantile(numColumn, c(.5)))
                    desc$setCell(rowNo=i, "quart3", quantile(numColumn, c(.75)))

                } else {

                    mode <- NULL

                    desc$setRow(rowNo=i, values=list(
                        mean='', median='', mode='', sum='', sd='', variance='',
                        range='', min='', max='', se='', skew='', kurt='',
                        quart1='', quart2='', quart3=''))
                }

                if (length(mode) > 1)
                    desc$addFootnote(rowNo=i, "mode", "More than one mode exists, only the first is reported")

                if (is.factor(column)) {

                    cumCount <- 0
                    levels <- base::levels(column)

                    freqTable <- freq$get(name)
                    freqPlot  <- freqPlots$get(name)

                    data <- NULL
                    if (length(levels) > 0) {
                        x <- factor(levels, levels=levels)
                        data <- data.frame(x=x, y=0)
                    }
                    else {
                        data <- data.frame(x=character(), y=numeric())
                    }

                    for (j in seq_along(levels)) {

                        count <- sum(column == levels[j])
                        cumCount <- cumCount + count

                        freqTable$setCell(rowNo=j, "counts", count)
                        freqTable$setCell(rowNo=j, "percentage", 100*count/length(column))
                        freqTable$setCell(rowNo=j, "cumpercentage", 100*cumCount/length(column))

                        data[j,'y'] <- count
                    }

                    freqPlot$setState(list(type='bars', data=data))

                } else {

                    if (n > 0) {
                        hist <- graphics::hist(column, plot=FALSE)
                        data <- data.frame(
                            x=hist$mids,
                            y=hist$density)

                    } else {

                        data <- data.frame(x=character(), y=numeric())
                    }

                    freqPlot <- freqPlots$get(name)
                    freqPlot$setState(list(type='hist', data=data))
                }
            }
        },
        .plotFreq=function(image, ggtheme, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            data <- image$state$data
            type <- image$state$type

            if (type == 'hist') {
                ylab <- 'density'
                axis.text.y <- element_blank()
                axis.ticks.y <- element_blank()
            }
            else {
                ylab <- 'counts'
                axis.text.y <- NULL
                axis.ticks.y <- NULL
            }

            plot <- ggplot(data=data, aes(x=x, y=y)) +
                geom_bar(stat="identity", color=theme$color[2], fill=theme$fill[2]) +
                labs(list(x=NULL, y=ylab)) +
                ggtheme +
                theme(axis.text.y=axis.text.y,
                      axis.ticks.y=axis.ticks.y)

            suppressWarnings(print(plot))

            TRUE
        }
    )
)
