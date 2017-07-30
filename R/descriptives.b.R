
descriptivesClass <- R6::R6Class(
    "descriptivesClass",
    inherit=descriptivesBase,
    private=list(
        #### Member variables ----
        colArgs = list(
            name = c("n", "missing", "mean", "median", "mode", "sum", "sd", "variance", "range",
                     "min", "max", "se", "skew", "kurt", "quart1", "quart2", "quart3"),
            title = c("N", "Missing", "Mean", "Median", "Mode", "Sum", "Standard deviation", "Variance",
                      "Range", "Minimum", "Maximum", "Standard error", "Skewness", "Kurtosis",
                      "25th percentile", "50th percentile", "75th percentile"),
            type = c(rep("integer", 2), rep("number", 15)),
            visible = c("(n)", "(missing)", "(mean)", "(median)", "(mode)", "(sum)", "(sd)", "(variance)", "(range)",
                        "(min)", "(max)", "(se)", "(skew)", "(kurt)", "(quart)", "(quart)", "(quart)")
        ),
        levels = NULL,

        #### Init + run functions ----
        .init = function() {

            private$.initDescriptivesTable()

            if (self$options$freq)
                private$.initFrequencyTables()

            private$.initPlots()

        },
        .run=function() {

            if (length(self$options$vars) > 0) {

                data <- self$data
                results <- private$.compute(data)

                private$.populateDescriptivesTable(results)

                if (self$options$freq)
                    private$.populateFrequencyTables(results)

                private$.preparePlots(data)

            }
        },

        #### Compute results ----
        .compute = function(data) {

            vars <- self$options$vars
            splitBy <- self$options$splitBy

            desc <- list()
            freq <- list()

            for (var in vars) {

                column <- data[[var]]

                if (is.factor(column)) {

                    if (length(splitBy) > 0) {

                        cols <- c(var, splitBy[-1], splitBy[1])
                        freq[[var]] <- table(data[cols])

                    } else {

                        freq[[var]] <- table(column)

                    }
                }

                column <- jmvcore::toNumeric(column)

                if (length(splitBy) > 0) {

                    groups <- data[splitBy]
                    desc[[var]] <- tapply(column, groups, private$.computeDesc, drop = FALSE)

                } else {

                    desc[[var]] <- private$.computeDesc(column)

                }
            }

            return(list(desc=desc, freq=freq))
        },

        #### Init tables/plots functions ----
        .initDescriptivesTable = function() {

            table <- self$results$descriptives
            vars <- self$options$vars
            splitBy <- self$options$splitBy
            data <- self$data

            levels <- list()
            for (i in seq_along(splitBy))
                levels[[i]] <- levels(data[[splitBy[i]]])

            private$levels <- levels

            expandGrid <- function(...) expand.grid(..., stringsAsFactors = FALSE)
            grid <- rev(do.call(expandGrid, rev(levels)))

            colArgs <- private$colArgs

            for (i in seq_along(colArgs$name)) {

                name <- colArgs$name[i]
                title <- colArgs$title[i]
                type <- colArgs$type[i]
                visible <- colArgs$visible[i]

                if (length(splitBy) > 0) {

                    for (j in 1:nrow(grid)) {

                        post <- paste0("[", name, paste0(grid[j,], collapse = ""), "]")
                        table$addColumn(name=paste0("stat", post), title="", type="text", value=title, visible=visible, combineBelow=TRUE)

                        if (j == 1)
                            table$addFormat(rowNo=1, col=paste0("stat", post), Cell.BEGIN_GROUP)

                        for (k in 1:ncol(grid)) {
                            table$addColumn(name=paste0("var", k,  post), title=splitBy[k], type="text", value=grid[j,k], visible=visible, combineBelow=TRUE)
                        }

                        for (k in seq_along(vars)) {

                            subName <- paste0(vars[k], post)
                            table$addColumn(name=subName, title=vars[k], type=type, visible=visible)
                        }
                    }

                } else {

                    post <- paste0("[", name, "]")
                    table$addColumn(name=paste0("stat", post), title="", type="text", value=title, visible=visible, combineBelow=TRUE)

                    for (k in seq_along(vars)) {

                        subName <- paste0(vars[k], post)
                        table$addColumn(name=subName, title=vars[k], type=type, visible=visible)
                    }
                }
            }
        },
        .initFrequencyTables = function() {

            tables <- self$results$frequencies
            vars <- self$options$vars
            splitBy <- self$options$splitBy

            for (i in seq_along(vars)) {

                var <- vars[i]
                column <- self$data[[var]]

                if (is.factor(column)) {

                    table <- tables$get(var)
                    levels <- base::levels(column)

                    if (length(splitBy) == 0) {

                        table$addColumn(name='levels', title='Levels', type='text')
                        table$addColumn(name='counts', title='Counts', type='integer')
                        # table$addColumn(name='perc', title='% of Total', type='number')

                        for (k in seq_along(levels))
                            table$addRow(levels[k], values = list(levels = levels[k]))

                    } else {

                        expandGrid <- function(...) expand.grid(..., stringsAsFactors = FALSE)
                        levels <- base::levels(column)
                        grid <- rev(do.call(expandGrid, rev(c(list(levels), private$levels[-1]))))
                        cols <- c(var, splitBy[-1])

                        for (col in cols)
                            table$addColumn(name=col, title=col, type='text', combineBelow=TRUE)

                        for (lev in private$levels[[1]])
                            table$addColumn(name=paste0(lev), title=lev, type='integer', superTitle=splitBy[1])

                        prev <- NULL
                        for (j in 1:nrow(grid)) {

                            row <- list()
                            for (k in seq_along(cols))
                                row[[cols[k]]] <- grid[j,k]

                            table$addRow(rowKey=j, values=row)

                            if (length(splitBy) > 1 && (j == 1 || grid[j,1] != prev))
                                table$addFormat(rowNo=j, col=1, Cell.BEGIN_GROUP)

                            prev <- grid[j,1]
                        }
                    }
                }
            }
        },
        .initPlots = function() {

            plots <- self$results$plots

            data <- self$data
            vars <- self$options$vars
            splitBy <- self$options$splitBy

            for (var in vars) {

                group <- plots$get(var)
                column <- data[[var]]

                if (is.factor(column)) {

                    if (self$options$bar) {

                        names <- na.omit(c(var, splitBy[1:3]))
                        df <- data[names]
                        levels <- lapply(df, levels)

                        size <- private$.plotSize(levels, 'bar')

                        image <- jmvcore::Image$new(options=self$options,
                                                    name="bar",
                                                    renderFun=".barPlot",
                                                    width=size[1],
                                                    height=size[2],
                                                    clearWith=list("splitBy", "bar"))

                        group$add(image)

                    }

                } else {

                    names <- na.omit(splitBy[1:3])
                    df <- data[names]
                    levels <- lapply(df, levels)

                    if (self$options$hist || self$options$dens) {

                        size <- private$.plotSize(levels, 'hist')

                        image <- jmvcore::Image$new(options=self$options,
                                                    name="hist",
                                                    renderFun=".histogram",
                                                    width=size[1],
                                                    height=size[2],
                                                    clearWith=list("splitBy", "hist", "dens"))

                        group$add(image)

                    }

                    if (self$options$box || self$options$violin || self$options$dot) {

                        size <- private$.plotSize(levels, 'box')

                        image <- jmvcore::Image$new(options=self$options,
                                                    name="box",
                                                    renderFun=".boxPlot",
                                                    width=size[1],
                                                    height=size[2],
                                                    clearWith=list("splitBy", "box", "violin", "dot", "dotType"))

                        group$add(image)
                    }
                }
            }
        },

        #### Populate tables ----
        .populateDescriptivesTable = function(results) {

            table <- self$results$descriptives
            vars <- self$options$vars
            splitBy <- self$options$splitBy

            expandGrid <- function(...) expand.grid(..., stringsAsFactors = FALSE)
            grid <- rev(do.call(expandGrid, rev(private$levels)))

            colNames <- private$colArgs$name
            desc <- results$desc

            values <- list()
            for (i in seq_along(vars)) {

                r <- desc[[vars[i]]]

                if (length(splitBy) > 0) {

                    for (j in 1:nrow(grid)) {

                        indices <- grid[j,]
                        stats <- do.call("[", c(list(r), indices))[[1]]

                        for (k in seq_along(colNames)) {

                            name <- colNames[k]
                            post <- paste0("[", name, paste0(grid[j,], collapse = ""), "]")
                            subName <- paste0(vars[i], post)

                            values[[subName]] <- stats[[name]][1]

                        }
                    }

                } else {

                    for (k in seq_along(colNames)) {

                        name <- colNames[k]
                        post <- paste0("[", name, "]")
                        subName <- paste0(vars[i], post)

                        values[[subName]] <- r[[name]][1]

                    }
                }
            }

            table$setRow(rowNo=1, values=values)
        },
        .populateFrequencyTables = function(results) {

            tables <- self$results$frequencies
            vars <- self$options$vars
            splitBy <- self$options$splitBy

            freqs <- results$freq

            for (i in seq_along(vars)) {

                var <- vars[i]
                column <- self$data[[var]]

                if (is.factor(column)) {

                    table <- tables$get(var)
                    levels <- base::levels(column)
                    freq <- freqs[[var]]

                    if (length(splitBy) == 0) {

                        for (k in seq_along(levels)) {
                            counts <- as.numeric(freq[levels[k]])
                            table$setRow(rowNo=k, values=list(counts=counts))
                        }

                    } else {

                        expandGrid <- function(...) expand.grid(..., stringsAsFactors = FALSE)
                        grid <- rev(do.call(expandGrid, rev(c(list(levels), private$levels[-1]))))
                        cols <- c(var, splitBy[-1])

                        for (j in 1:nrow(grid)) {

                            row <- list()
                            for (lev in private$levels[[1]]) {
                                indices <- c(grid[j,], lev)
                                counts <- do.call("[", c(list(freq), indices))[[1]]
                                row[[lev]] <- as.numeric(counts)
                            }

                            table$setRow(rowKey=j, values=row)
                        }
                    }
                }
            }
        },

        #### Plot functions ----
        .preparePlots = function(data) {

            plots <- self$results$plots
            vars <- self$options$vars
            splitBy <- self$options$splitBy

            for (i in seq_along(vars)) {

                var <- vars[i]
                group <- plots$get(var)
                column <- data[[var]]

                if (is.factor(column)) {

                    levels <- base::levels(column)
                    bar  <- group$get('bar')

                    if (self$options$bar) {

                        if (length(levels) > 0) {

                            columns <- na.omit(c(var, splitBy[1:3]))
                            groups <- data[columns]

                            if (length(splitBy) >= 3) {
                                names <- list("x"="x", "s1"="s1", "s2"="s2", "s3"="s3", "y"="y")
                                labels <- list("x"=var, "s1"=splitBy[1], "s2"=splitBy[2], "s3"=splitBy[3])
                            } else if (length(splitBy) == 2) {
                                names <- list("x"="x", "s1"="s1", "s2"="s2", "s3"=NULL, "y"="y")
                                labels <- list("x"=var, "s1"=splitBy[1], "s2"=splitBy[2], "s3"=NULL)
                            } else if (length(splitBy) == 1) {
                                names <- list("x"="x", "s1"="s1", "s2"=NULL, "s3"=NULL, "y"="y")
                                labels <- list("x"=var, "s1"=splitBy[1], "s2"=NULL, "s3"=NULL)
                            } else {
                                names <- list("x"="x", "s1"=NULL, "s2"=NULL, "s3"=NULL, "y"="y")
                                labels <- list("x"=var, "s1"=NULL, "s2"=NULL, "s3"=NULL)
                            }

                            plotData <- as.data.frame(table(groups))

                        } else {

                            plotData <- data.frame(x=character(), y=numeric())
                            names <- list("x"="x", "s1"=NULL, "s2"=NULL, "s3"=NULL, "y"="y")
                            labels <- list("x"=var, "s1"=NULL, "s2"=NULL, "s3"=NULL)

                        }

                        colnames(plotData) <- as.character(unlist(names))

                        bar$setState(list(data=plotData, names=names, labels=labels))
                    }

                } else {

                    hist  <- group$get('hist')
                    box  <- group$get('box')

                    if (self$options$hist || self$options$dens || self$options$box || self$options$violin || self$options$dot) {

                        if (length(na.omit(column)) > 0) {

                            columns <- na.omit(c(var, splitBy[1:3]))
                            plotData <- naOmit(data[columns])

                            if (length(splitBy) >= 3) {
                                names <- list("x"="x", "s1"="s1", "s2"="s2", "s3"="s3")
                                labels <- list("x"=var, "s1"=splitBy[1], "s2"=splitBy[2], "s3"=splitBy[3])
                            } else if (length(splitBy) == 2) {
                                names <- list("x"="x", "s1"="s1", "s2"="s2", "s3"=NULL)
                                labels <- list("x"=var, "s1"=splitBy[1], "s2"=splitBy[2], "s3"=NULL)
                            } else if (length(splitBy) == 1) {
                                names <- list("x"="x", "s1"="s1", "s2"=NULL, "s3"=NULL)
                                labels <- list("x"=var, "s1"=splitBy[1], "s2"=NULL, "s3"=NULL)
                            } else {
                                names <- list("x"="x", "s1"=NULL, "s2"=NULL, "s3"=NULL)
                                labels <- list("x"=var, "s1"=NULL, "s2"=NULL, "s3"=NULL)
                            }

                        } else {

                            plotData <- data.frame(x=character())
                            names <- list("x"="x", "s1"=NULL, "s2"=NULL, "s3"=NULL)
                            labels <- list("x"=var, "s1"=NULL, "s2"=NULL, "s3"=NULL)
                        }

                        colnames(plotData) <- as.character(unlist(names))

                        if (self$options$hist || self$options$dens)
                            hist$setState(list(data=plotData, names=names, labels=labels))

                        if (self$options$box || self$options$violin || self$options$dot)
                            box$setState(list(data=plotData, names=names, labels=labels))
                    }
                }
            }
        },
        .histogram = function(image, ggtheme, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            data <- image$state$data
            names <- image$state$names
            labels <- image$state$labels
            splitBy <- self$options$splitBy

            fill <- theme$fill[2]
            color <- theme$color[1]
            if (length(splitBy) == 2) {
                formula <- as.formula(paste(". ~", names$s2))
            } else if (length(splitBy) > 2) {
                formula <- as.formula(paste(names$s3, "~", names$s2))
            }

            if (self$options$hist && self$options$dens)
                alpha <- 0.4
            else
                alpha <- 1

            themeSpec <- NULL

            if (is.null(splitBy)) {

                plot <- ggplot(data=data, aes_string(x=names$x)) +
                    labs(list(x=labels$x, y='density'))

                if (self$options$hist)
                    plot <- plot + geom_histogram(aes(y=..density..), position="identity",
                                                  stat="bin", binwidth = 0.5, color=color, fill=fill)

                if (self$options$dens)
                    plot <- plot + geom_density(color=color, fill=fill, alpha=alpha)

                themeSpec <- theme(axis.text.y=element_blank(),
                                   axis.ticks.y=element_blank())

            } else {

                plot <- ggplot(data=data, aes_string(x=names$x, y=names$s1, fill=names$s1)) +
                    labs(list(x=labels$x, y=labels$s1)) +
                    scale_y_discrete(expand = c(0.05, 0)) +
                    scale_x_continuous(expand = c(0.01, 0))

                if (self$options$hist)
                    plot <- plot + ggjoy::geom_joy(stat="binline", bins=20, scale=0.9)

                if (self$options$dens)
                    plot <- plot + ggjoy::geom_joy(scale=0.9, alpha=alpha)

                themeSpec <- theme(legend.position = 'none')
            }

            if (length(splitBy) > 1)
                plot <- plot + facet_grid(formula)

            plot <- plot + ggtheme + themeSpec

            suppressWarnings(
                suppressMessages(
                    print(plot)
                )
            )

            TRUE
        },
        .barPlot = function(image, ggtheme, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            data <- image$state$data
            names <- image$state$names
            labels <- image$state$labels
            splitBy <- self$options$splitBy

            fill <- theme$fill[2]
            color <- theme$color[1]
            if (length(splitBy) == 2) {
                formula <- as.formula(paste(". ~", names$s2))
            } else if (length(splitBy) > 2) {
                formula <- as.formula(paste(names$s3, "~", names$s2))
            }

            themeSpec <- NULL

            if (is.null(splitBy)) {

                plot <- ggplot(data=data, aes_string(x=names$x, y=names$y)) +
                    geom_bar(stat="identity", position="dodge", width = 0.7, fill=fill, color=color) +
                    labs(list(x=labels$x, y='counts'))

                # if (self$options$barCounts)
                #     plot <- plot + geom_text(aes_string(label=names$y), vjust=-0.3)

            } else {

                plot <- ggplot(data=data, aes_string(x=names$x, y=names$y, fill=names$s1)) +
                    geom_bar(stat="identity", position=position_dodge(0.85), width = 0.7, color='#333333') +
                    labs(list(x=labels$x, y='counts', fill=labels$s1))

                # if (self$options$barCounts)
                #     plot <- plot + geom_text(aes_string(label=names$y), position=position_dodge(.75), vjust=-0.3)

            }

            if (length(splitBy) > 1)
                plot <- plot + facet_grid(formula)

            plot <- plot + ggtheme + themeSpec

            suppressWarnings(
                suppressMessages(
                    print(plot)
                )
            )

            TRUE
        },
        .boxPlot = function(image, ggtheme, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            data <- image$state$data
            type <- image$state$type
            names <- image$state$names
            labels <- image$state$labels
            splitBy <- self$options$splitBy

            fill <- theme$fill[2]
            color <- theme$color[2]
            if (length(splitBy) > 2) {
                formula <- as.formula(paste(". ~", names$s3))
            }

            themeSpec <- NULL

            if (is.null(splitBy) || length(splitBy) == 1) {

                data[["placeHolder"]] <- rep('var1', nrow(data))

                if (is.null(splitBy))
                    x <- "placeHolder"
                else
                    x <- names$s1

                plot <- ggplot(data=data, aes_string(x=x, y=names$x)) +
                    labs(list(x=labels$s1, y=labels$x))

                if (self$options$violin)
                    plot <- plot + ggplot2::geom_violin(fill=theme$fill[1], color=theme$color[1], alpha=0.5)

                if (self$options$dot) {
                    if (self$options$dotType == 'jitter')
                        plot <- plot + ggplot2::geom_jitter(color=theme$color[1], width=0.1, alpha=0.4)
                    else if (self$options$dotType == 'stack')
                        plot <- plot + ggplot2::geom_dotplot(binaxis = "y", stackdir = "center",
                                                       color=theme$color[1], alpha=0.4,
                                                       stackratio=0.9, dotsize=0.7)
                }

                if (self$options$box)
                    plot <- plot + ggplot2::geom_boxplot(color=theme$color[1], width=0.3, alpha=0.9,
                                                   fill=theme$fill[2], outlier.colour=theme$color[1])

                if (is.null(splitBy))
                    themeSpec <- theme(axis.text.x=element_blank(),
                                       axis.ticks.x=element_blank(),
                                       axis.title.x=element_blank())

            } else {

                plot <- ggplot(data=data, aes_string(x=names$s1, y=names$x, fill=names$s2)) +
                    labs(list(x=labels$s1, y=labels$x, fill=labels$s2, color=labels$s2))

                if (self$options$violin)
                    plot <- plot + ggplot2::geom_violin(color=theme$color[1], position=position_dodge(0.9), alpha=0.3)

                if (self$options$dot) {
                    if (self$options$dotType == 'jitter')
                        plot <- plot + ggplot2::geom_jitter(aes_string(color=names$s2), alpha=0.7,
                                                            position = position_jitterdodge(jitter.width=0.1,
                                                                                            dodge.width = 0.9))
                    else if (self$options$dotType == 'stack')
                        plot <- plot + ggplot2::geom_dotplot(binaxis = "y", stackdir = "center",
                                                             color=theme$color[1], alpha=0.4,
                                                             stackratio=0.9, dotsize=0.7,
                                                             position=position_dodge(0.9))
                }

                if (self$options$box)
                    plot <- plot + ggplot2::geom_boxplot(color=theme$color[1], width=0.3, alpha=0.8,
                                                         outlier.colour=theme$color[1],
                                                         position=position_dodge(0.9))
            }

            if (length(splitBy) > 2)
                plot <- plot + facet_grid(formula)

            plot <- plot + ggtheme + themeSpec

            suppressWarnings(
                suppressMessages(
                    print(plot)
                )
            )

            TRUE
        },

        #### Helper functions ----
        .computeDesc = function(column) {

            stats <- list()

            total <- length(column)
            column <- jmvcore::naOmit(column)
            n <- length(column)
            stats[['n']] <- n
            stats[['missing']] <- total - n

            if (jmvcore::canBeNumeric(column) && n > 0) {

                stats[['mean']] <- mean(column)
                stats[['median']] <- median(column)
                stats[['mode']] <- as.numeric(names(table(column)[table(column)==max(table(column))]))
                stats[['sum']] <- sum(column)
                stats[['sd']] <- sd(column)
                stats[['variance']] <- var(column)
                stats[['range']] <- max(column)-min(column)
                stats[['min']] <- min(column)
                stats[['max']] <- max(column)
                stats[['se']] <- sqrt(var(column)/length(column))

                deviation <- column-mean(column)
                stats[['skew']] <- sum(deviation^3)/(length(column)*sd(column)^3)
                stats[['kurt']] <- sum(deviation^4)/(length(column)*var(column)^2)

                stats[['quart1']] <- as.numeric(quantile(column, c(.25)))
                stats[['quart2']] <- as.numeric(quantile(column, c(.5)))
                stats[['quart3']] <- as.numeric(quantile(column, c(.75)))

            } else if (jmvcore::canBeNumeric(column)) {

                l <- list(mean=NaN, median=NaN, mode=NaN, sum=NaN, sd=NaN, variance=NaN,
                          range=NaN, min=NaN, max=NaN, se=NaN, skew=NaN, kurt=NaN,
                          quart1=NaN, quart2=NaN, quart3=NaN)

                stats <- append(stats, l)

            } else {

                l <- list(mean='', median='', mode='', sum='', sd='', variance='',
                          range='', min='', max='', se='', skew='', kurt='',
                          quart1='', quart2='', quart3='')

                stats <- append(stats, l)

            }

            return(stats)
        },
        .plotSize = function(levels, plot) {

            nLevels <- as.numeric(sapply(levels, length))
            nLevels <- ifelse(is.na(nLevels[1:4]), 1, nLevels[1:4])
            nCharLevels <- as.numeric(sapply(lapply(levels, nchar), max))
            nCharLevels <- ifelse(is.na(nCharLevels[1:4]), 0, nCharLevels[1:4])
            nCharNames <- as.numeric(nchar(names(levels)))
            nCharNames <- ifelse(is.na(nCharNames[1:4]), 0, nCharNames[1:4])

            if (plot == "bar") {

                xAxis <- 30 + 20
                yAxis <- 30 + 20
                width <- max(300, 50 * nLevels[1] * nLevels[2] * nLevels[3])
                height <- 300 * nLevels[4]
                legend <- max(25 + 21 + 3.5 + 8.3 * nCharLevels[2] + 28, 25 + 10 * nCharNames[2] + 28)

                width <- yAxis + width + ifelse(nLevels[2] > 1, legend, 0)
                height <- xAxis + height

            } else if (plot == "box") {

                xAxis <- 30 + 20
                yAxis <- 30 + 20
                width <- max(300, 70 * nLevels[1] * nLevels[2] * nLevels[3])
                height <- 300
                legend <- max(25 + 21 + 3.5 + 8.3 * nCharLevels[2] + 28, 25 + 10 * nCharNames[2] + 28)

                width <- yAxis + width + ifelse(nLevels[2] > 1, legend, 0)
                height <- xAxis + height

            } else {

                xAxis <- 30 + 20
                yAxis <- 45 + 11 + 8.3 * nCharLevels[1]
                width <- 300 * nLevels[2]
                height <- max(300, 50 * nLevels[1] * nLevels[3])

                width <- yAxis + width
                height <- xAxis + height

            }

            return(c(width, height))
        }
    )
)
