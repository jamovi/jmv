
descriptivesClass <- R6::R6Class(
    "descriptivesClass",
    inherit=descriptivesBase,
    private=list(
        #### Member variables ----
        colArgs = list(
            name = c("n", "missing", "mean", "se", "median", "mode", "sum", "sd", "variance", "range",
                     "min", "max", "skew", "seSkew", "kurt", "seKurt", "sww", "sw", "quart1", "quart2", "quart3"),
            title = c("N", "Missing", "Mean", "Std. error mean", "Median", "Mode", "Sum", "Standard deviation", "Variance",
                      "Range", "Minimum", "Maximum", "Skewness", "Std. error skewness",
                      "Kurtosis", "Std. error kurtosis", "Shapiro-Wilk W", "Shapiro-Wilk p", "25th percentile", "50th percentile", "75th percentile"),
            type = c(rep("integer", 2), rep("number", 19)),
            format = c(rep("", 17), "zto,pvalue", rep("", 3)),
            visible = c("(n)", "(missing)", "(mean)", "(se)", "(median)", "(mode)", "(sum)", "(sd)", "(variance)", "(range)",
                        "(min)", "(max)", "(skew)", "(skew)", "(kurt)", "(kurt)", "(sw)", "(sw)", "(quart)", "(quart)", "(quart)")
        ),
        levels = NULL,

        #### Init + run functions ----
        .init = function() {

            private$.addQuantiles()
            private$.addPercentiles()
            private$.initDescriptivesTable()

            if (self$options$freq)
                private$.initFrequencyTables()

            private$.initPlots()

        },
        .clear = function(vChanges, ...) {

            private$.clearDescriptivesTable(vChanges)

        },
        .run=function() {

            data <- self$data
            splitBy <- self$options$splitBy

            if ( ! is.null(splitBy)) {
                for (item in splitBy) {
                    if ( ! is.factor(data[[item]]))
                        reject('Unable to split by a continuous variable')
                }
            }

            private$.errorCheck(data)

            if (length(self$options$vars) > 0) {

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

                if (private$.treatAsFactor(column)) {

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

            levels <- rep(list(NULL), length(splitBy))
            for (i in seq_along(splitBy)) {
                lvls <- levels(data[[splitBy[i]]])
                if (length(lvls) == 0) {
                    # error
                    splitBy <- NULL
                    levels <- list()
                    break()
                }
                levels[[i]] <- lvls
            }

            private$levels <- levels

            expandGrid <- function(...) expand.grid(..., stringsAsFactors = FALSE)
            grid <- rev(do.call(expandGrid, rev(levels)))

            colArgs <- private$colArgs

            for (i in seq_along(colArgs$name)) {

                name <- colArgs$name[i]
                title <- colArgs$title[i]
                format <- colArgs$format[i]
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
                            table$addColumn(name=subName, title=vars[k], type=type, format=format, visible=visible)
                        }
                    }

                } else {

                    post <- paste0("[", name, "]")
                    table$addColumn(name=paste0("stat", post), title="", type="text", value=title, visible=visible, combineBelow=TRUE)

                    for (k in seq_along(vars)) {

                        subName <- paste0(vars[k], post)
                        table$addColumn(name=subName, title=vars[k], type=type, format=format, visible=visible)
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

                if (private$.treatAsFactor(column)) {

                    table <- tables$get(var)
                    levels <- base::levels(column)
                    if (is.null(levels)) {
                        levels <- levels(factor(naOmit(column)))
                        table$setVisible(TRUE)
                    }

                    if (length(splitBy) == 0) {

                        table$addColumn(name='levels', title='Levels', type='text')
                        table$addColumn(name='counts', title='Counts', type='integer')
                        table$addColumn(name='pc', title='% of Total', type='number', format='pc')
                        table$addColumn(name='cumpc', title='Cumulative %', type='number', format='pc')

                        for (k in seq_along(levels))
                            table$addRow(levels[k], values = list(levels = levels[k]))

                    } else {

                        expandGrid <- function(...) expand.grid(..., stringsAsFactors = FALSE)
                        grid <- rev(do.call(expandGrid, rev(c(list(levels), private$levels[-1]))))
                        cols <- c(var, splitBy[-1])

                        for (col in cols)
                            table$addColumn(name=col, title=col, type='text', combineBelow=TRUE)

                        if (length(private$levels) >= 1) {
                            for (lev in private$levels[[1]])
                                table$addColumn(name=paste0(lev), title=lev, type='integer', superTitle=splitBy[1])
                        }

                        prev <- NULL
                        for (j in seq_len(nrow(grid))) {

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

            varsCannotBeNumeric <- NULL
            for (var in vars) {

                if ((self$options$hist || self$options$dens || self$options$box ||
                    self$options$violin || self$options$dot || self$options$qq) &&
                    ! jmvcore::canBeNumeric(data[[var]])) {

                    varsCannotBeNumeric <- c(varsCannotBeNumeric, var)

                }
            }

            if ( ! is.null(varsCannotBeNumeric)) {

                html <- jmvcore::Html$new(
                    options = self$options,
                    name = 'warningMessage'
                )

                if (length(varsCannotBeNumeric) == 1) {
                    content <- jmvcore::format(
                        "<p class=\"warning\">
                            The variable {} cannot be treated as numeric. Therefore plots that expect
                            numeric data will not be created for this variable.
                        </p>", listItems(varsCannotBeNumeric)
                    )
                } else {
                    content <- jmvcore::format(
                        "<p class=\"warning\">
                            The variables {} cannot be treated as numeric. Therefore plots that expect
                            numeric data will not be created for these variables.
                        </p>", listItems(varsCannotBeNumeric)
                    )

                }

                html$setContent(content)
                plots$add(html)
            }

            for (var in vars) {

                group <- jmvcore::Group$new(options = self$options, name = var, title = var)
                column <- data[[var]]

                if (self$options$bar) {

                    names <- na.omit(c(var, splitBy[1:3]))
                    df <- data[names]
                    levels <- lapply(df, levels)

                    size <- private$.plotSize(levels, 'bar')

                    image <- jmvcore::Image$new(
                        options = self$options,
                        name = "bar",
                        renderFun = ".barPlot",
                        width = size[1],
                        height=size[2],
                        clearWith=list("splitBy", "bar")
                    )

                    group$add(image)
                }

                if (jmvcore::canBeNumeric(column)) {

                    if (is.null(splitBy))
                        names <- NULL
                    else
                        names <- na.omit(splitBy[1:3])

                    df <- data[names]
                    levels <- lapply(df, levels)

                    if (self$options$hist || self$options$dens) {

                        size <- private$.plotSize(levels, 'hist')

                        image <- jmvcore::Image$new(
                            options = self$options,
                            name = "hist",
                            renderFun = ".histogram",
                            width = size[1],
                            height = size[2],
                            clearWith = list("splitBy", "hist", "dens")
                        )

                        group$add(image)
                    }

                    if (self$options$box || self$options$violin || self$options$dot) {

                        size <- private$.plotSize(levels, 'box')

                        image <- jmvcore::Image$new(
                            options = self$options,
                            name = "box",
                            renderFun = ".boxPlot",
                            width = size[1],
                            height = size[2],
                            clearWith = list("splitBy", "box", "violin", "dot", "dotType")
                        )

                        group$add(image)
                    }

                    if (self$options$qq) {

                        size <- private$.plotSize(levels, 'qq')

                        image <- jmvcore::Image$new(
                            options = self$options,
                            name = "qq",
                            renderFun = ".qq",
                            requiresData = TRUE,
                            width = size[1],
                            height = size[2],
                            clearWith = list("splitBy")
                        )

                        group$add(image)
                    }

                }

                plots$add(group)
            }
        },

        #### Clear tables ----
        .clearDescriptivesTable = function(vChanges) {

            table <- self$results$descriptives
            vars <- vChanges
            splitBy <- self$options$splitBy

            expandGrid <- function(...) expand.grid(..., stringsAsFactors = FALSE)
            grid <- rev(do.call(expandGrid, rev(private$levels)))

            colNames <- private$colArgs$name

            values <- rep(NA, length(vars) * ifelse(length(splitBy) > 0, nrow(grid), 1) * length(colNames))
            names <- rep('', length(vars) * ifelse(length(splitBy) > 0, nrow(grid), 1) * length(colNames))
            iter <- 1

            for (i in seq_along(vars)) {

                if (length(splitBy) > 0) {

                    for (j in 1:nrow(grid)) {
                        for (k in seq_along(colNames)) {

                            name <- colNames[k]
                            post <- paste0("[", name, paste0(grid[j,], collapse = ""), "]")
                            subName <- paste0(vars[i], post)

                            names[iter] <- subName
                            iter <- iter + 1
                        }
                    }

                } else {

                    for (k in seq_along(colNames)) {

                        name <- colNames[k]
                        post <- paste0("[", name, "]")
                        subName <- paste0(vars[i], post)

                        names[iter] <- subName
                        iter <- iter + 1
                    }
                }
            }

            names(values) <- names
            table$setRow(rowNo=1, values=values)
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

            values <- list(); footnotes <- list()
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

                        if (length(stats[['mode']]) > 1) {
                            post <- paste0("[mode", paste0(grid[j,], collapse = ""), "]")
                            subName <- paste0(vars[i], post)
                            footnotes <- c(footnotes, subName)
                        }
                    }

                } else {

                    for (k in seq_along(colNames)) {

                        name <- colNames[k]
                        post <- paste0("[", name, "]")
                        subName <- paste0(vars[i], post)

                        values[[subName]] <- r[[name]][1]
                    }

                    if (length(r[['mode']]) > 1)
                        footnotes <- c(footnotes, paste0(vars[i], '[mode]'))
                }
            }

            table$setRow(rowNo=1, values=values)

            for (i in seq_along(footnotes))
                table$addFootnote(rowNo=1, footnotes[[i]], 'More than one mode exists, only the first is reported')


        },
        .populateFrequencyTables = function(results) {

            tables <- self$results$frequencies
            vars <- self$options$vars
            splitBy <- self$options$splitBy

            freqs <- results$freq

            for (i in seq_along(vars)) {

                var <- vars[i]
                column <- self$data[[var]]

                if (private$.treatAsFactor(column)) {

                    table <- tables$get(var)

                    if ( !  table$visible)
                        next()

                    levels <- base::levels(column)
                    if (is.null(levels)) {
                        levels <- levels(factor(naOmit(column)))
                        table$setVisible(TRUE)
                    }
                    freq <- freqs[[var]]

                    if (length(splitBy) == 0) {
                        n <- sum(freq)
                        cumsum <- 0
                        for (k in seq_along(levels)) {
                            counts <- as.numeric(freq[levels[k]])
                            cumsum <- cumsum + counts
                            pc <- counts / n
                            cumpc <- cumsum / n
                            if (is.na(pc)) pc <- 0
                            if (is.na(cumpc)) cumpc <- 0
                            table$setRow(rowNo=k, values=list(
                                counts=counts,
                                pc=pc,
                                cumpc=cumpc))
                        }

                    } else {

                        expandGrid <- function(...) expand.grid(..., stringsAsFactors = FALSE)
                        grid <- rev(do.call(expandGrid, rev(c(list(levels), private$levels[-1]))))
                        cols <- c(var, splitBy[-1])

                        for (j in seq_len(nrow(grid))) {

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

                if (self$options$bar) {

                    levels <- base::levels(column)
                    bar  <- group$get('bar')

                    if ( ! is.factor(column)) {

                        values <- data[[var]]

                        nSplits <- length(splitBy)
                        if (nSplits > 3)  # limit to one for now
                            nSplits <- 3

                        by <- splitBy[seq_len(nSplits)]
                        by <- as.list(data[by])
                        names(by) <- c('s1', 's2', 's3')[seq_len(nSplits)]

                        meanfun <- function(x) mean(x, na.rm=TRUE)
                        sefun <- function(x) sd(x, na.rm=TRUE)/sqrt(sum( ! is.na(x)))

                        if (length(by) > 0) {

                            plotData <- aggregate(x=values, by=by, FUN=meanfun)
                            names(plotData)[length(plotData)] <- 'y'

                            ses <- aggregate(x=values, by=by, FUN=sefun)$x

                            plotData <- cbind(x='', plotData)
                            plotData <- cbind(plotData, sel=plotData$y-ses)
                            plotData <- cbind(plotData, seu=plotData$y+ses)

                        } else {
                            m <- meanfun(values)
                            ses <- sefun(values)
                            plotData <- data.frame(x='', y=m, sel=m-ses, seu=m+ses)
                        }

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

                    } else if (length(levels) > 0) {

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

                        colnames(plotData) <- as.character(unlist(names))

                    } else {

                        plotData <- data.frame(x=character(), y=numeric())
                        names <- list("x"="x", "s1"=NULL, "s2"=NULL, "s3"=NULL, "y"="y")
                        labels <- list("x"=var, "s1"=NULL, "s2"=NULL, "s3"=NULL)
                    }


                    type <- `if`(is.factor(column), 'categorical', 'continuous')

                    bar$setState(list(data=plotData, names=names, labels=labels, type=type))

                }

                if (jmvcore::canBeNumeric(column)) {

                    hist  <- group$get('hist')
                    box   <- group$get('box')
                    qq    <- group$get('qq')

                    if (self$options$qq)
                        qq$setState(var)

                    if (self$options$hist || self$options$dens || self$options$box || self$options$violin || self$options$dot) {

                        if (length(na.omit(column)) > 0) {

                            columns <- na.omit(c(var, splitBy[1:3]))
                            plotData <- naOmit(data[columns])
                            plotData[[var]] <- jmvcore::toNumeric(plotData[[var]])

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
        .qq = function(image, ggtheme, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            var <- image$state
            data <- self$data
            splitBy <- self$options$splitBy

            if (length(splitBy) > 3)
                splitBy <- splitBy[1:3]

            nSplits <- length(splitBy)
            splitNames <- paste0('s', seq_len(nSplits))

            grid <- list()
            for (i in seq_along(splitBy))
                grid[[ splitNames[i] ]] <- data[[ splitBy[[i]] ]]
            grid <- as.data.frame(grid)

            y <- jmvcore::toNumeric(data[[var]])

            if (nSplits > 0) {
                # split into groups
                pieces <- split(y, grid)
                # scale groups individually
                pieces <- lapply(pieces, function(x) as.vector(scale(x)))
                # join back together
                y <- unsplit(pieces, grid)
                data <- cbind(grid, y=y)
            } else {
                y <- as.vector(scale(y))
                data <- data.frame(y=y)
            }

            data <- na.omit(data)

            plot <- ggplot(data=data) +
                geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
                stat_qq(aes(sample=y), size=2, colour=theme$color[1]) +
                xlab("Theoretical Quantiles") +
                ylab("Standardized Residuals") +
                ggtheme

            if (nSplits == 0) {
                facetFmla <- NULL
            } else if (nSplits == 1) {
                facetFmla <- . ~ s1
            } else if (nSplits == 2) {
                facetFmla <- s1 ~ s2
            } else {
                facetFmla <- s3 ~ s2 * s1
            }

            if ( ! is.null(facetFmla))
                plot <- plot + facet_grid(as.formula(facetFmla), drop=FALSE)

            return(plot)
        },
        .histogram = function(image, ggtheme, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            data <- image$state$data
            names <- image$state$names
            labels <- image$state$labels
            splitBy <- self$options$splitBy

            if (self$options$hist && self$options$dens)
                alpha <- 0.4
            else
                alpha <- 1

            themeSpec <- NULL
            nBins <- 18
            nSplits <- length(splitBy)

            if (nSplits == 0) {

                fill <- theme$fill[2]
                color <- theme$color[1]

                min <- min(data[names$x])
                max <- max(data[names$x])

                range <- max - min

                nUniques <- length(unique(data[[names$x]]))
                if (nUniques > nBins)
                    binWidth <- range / nBins
                else
                    binWidth <- range / (nUniques - 1)

                plot <- ggplot(data=data, aes_string(x=names$x)) +
                    labs(x=labels$x, y='density')

                if (self$options$hist)
                    plot <- plot + geom_histogram(aes(y=..density..), position="identity",
                                                  stat="bin", binwidth = binWidth, color=color, fill=fill)

                if (self$options$dens)
                    plot <- plot + geom_density(color=color, fill=fill, alpha=alpha)

                themeSpec <- theme(axis.text.y=element_blank(),
                                   axis.ticks.y=element_blank())

            } else {  # if (nSplits > 0) {

                data$s1rev <- factor(data$s1, rev(levels(data$s1)))

                plot <- ggplot(data=data, aes_string(x='x', y='s1rev', fill='s1')) +
                    labs(x=labels$x, y=labels$s1) +
                    scale_y_discrete(expand = c(0.05, 0)) +
                    scale_x_continuous(expand = c(0.01, 0))

                if (self$options$hist)
                    plot <- plot + ggridges::geom_density_ridges(stat="binline", bins=nBins, scale=0.9)

                if (self$options$dens)
                    plot <- plot + ggridges::geom_density_ridges(scale=0.9, alpha=alpha)

                if (nSplits == 2) {
                    plot <- plot + facet_grid(cols=vars(s2))
                } else if (nSplits > 2) {
                    plot <- plot + facet_grid(cols=vars(s2), rows=vars(s3))
                }

                themeSpec <- theme(legend.position = 'none')
            }

            plot <- plot + ggtheme + themeSpec

            return(plot)
        },
        .barPlot = function(image, ggtheme, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            data <- image$state$data
            names <- image$state$names
            labels <- image$state$labels
            splitBy <- self$options$splitBy
            type <- `if`(identical(image$state$type, 'continuous'), 'continuous', 'categorical')

            fill <- theme$fill[2]
            color <- theme$color[1]

            if (length(splitBy) == 2) {
                formula <- as.formula(paste(". ~", names$s2))
            } else if (length(splitBy) > 2) {
                formula <- as.formula(paste(names$s3, "~", names$s2))
            }

            if (is.null(splitBy)) {

                if (type == 'categorical') {

                    plot <- ggplot(data=data, aes_string(x=names$x, y=names$y)) +
                        geom_bar(stat="identity", position="dodge", width = 0.7, fill=fill, color=color) +
                        labs(x=labels$x, y='counts')

                } else {

                    plot <- ggplot(data=data, aes_string(x=names$x, y=names$y)) +
                        geom_col(position="dodge", width = 0.7, fill=fill, color=color) +
                        geom_errorbar(aes_string(x=names$x, ymin='sel', ymax='seu'), width=.1) +
                        labs(x=labels$x, y='')
                }

            } else {

                pd <- position_dodge(0.85)

                if (type == 'categorical') {

                    plot <- ggplot(data=data, aes_string(x=names$x, y=names$y, fill=names$s1)) +
                        geom_bar(stat="identity", position=pd, width = 0.7, color='#333333') +
                        labs(x=labels$x, y='counts', fill=labels$s1)

                } else {

                    plot <- ggplot(data=data, aes_string(x=names$x, y=names$y, fill=names$s1)) +
                        geom_col(position=pd, width = 0.7, color='#333333') +
                        geom_errorbar(position=pd, aes_string(ymin='sel', ymax='seu'), width=.1) +
                        labs(x=labels$x, y='', fill=labels$s1)
                }

                if (length(splitBy) > 1)
                    plot <- plot + facet_grid(formula)
            }

            plot <- plot + ggtheme

            return(plot)
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
                    labs(x=labels$s1, y=labels$x)

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

                if (self$options$box) {
                    # hide outliers it plotting the data
                    outlier.shape <- `if`(self$options$dot, NA, 19)

                    plot <- plot + ggplot2::geom_boxplot(color=theme$color[1], width=0.3, alpha=0.9,
                                                   fill=theme$fill[2], outlier.colour=theme$color[1],
                                                   outlier.shape=outlier.shape)
                }

                if (is.null(splitBy))
                    themeSpec <- theme(axis.text.x=element_blank(),
                                       axis.ticks.x=element_blank(),
                                       axis.title.x=element_blank())

            } else {

                plot <- ggplot(data=data, aes_string(x=names$s2, y=names$x, fill=names$s1)) +
                    labs(x=labels$s2, y=labels$x, fill=labels$s1, color=labels$s1)

                if (self$options$violin)
                    plot <- plot + ggplot2::geom_violin(color=theme$color[1], position=position_dodge(0.9), alpha=0.3)

                if (self$options$dot) {
                    if (self$options$dotType == 'jitter')
                        plot <- plot + ggplot2::geom_jitter(aes_string(color=names$s1), alpha=0.7,
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

            return(plot)
        },

        #### Helper functions ----
        .errorCheck = function(data) {

            splitBy <- self$options$splitBy

            for (var in splitBy) {
                if (length(levels(data[[var]])) == 0)
                    jmvcore::reject(jmvcore::format('The \'split by\' variable \'{}\' contains no data.', var), code='')
            }
        },
        .treatAsFactor = function(column) {

            if (is.factor(column))
                return(TRUE)

            nUniques <- length(unique(column))
            if (nUniques > 0 && nUniques <= 10)
                return(TRUE)
            else
                return(FALSE)
        },
        .addQuantiles = function() {

            pcNEqGr <- self$options$pcNEqGr

            if (self$options$quart && pcNEqGr == 4)
                return()

            colArgs <- private$colArgs
            pcEq <- (1:pcNEqGr / pcNEqGr)[-pcNEqGr]

            private$colArgs$name <- c(colArgs$name, paste0('quant', 1:(pcNEqGr-1)))
            private$colArgs$title <- c(colArgs$title, paste0(round(pcEq * 100, 2), 'th percentile'))
            private$colArgs$type <- c(colArgs$type, rep('number', pcNEqGr - 1))
            private$colArgs$visible <- c(colArgs$visible, rep("(pcEqGr)", pcNEqGr - 1))

        },
        .addPercentiles = function() {

            pcValues <- as.numeric(unlist(strsplit(self$options$pcValues,",")))
            pcValues[pcValues < 0 | pcValues > 1] <- NA 
            pcValues <- pcValues[!is.na(pcValues)]
            npcValues <- length(pcValues)

            if (! self$options$pcVal || npcValues == 0)
                return()       

            colArgs <- private$colArgs

            private$colArgs$name <- c(colArgs$name, paste0('perc', 1:npcValues))
            private$colArgs$title <- c(colArgs$title, paste0(round(pcValues * 100, 2), 'th percentile'))
            private$colArgs$type <- c(colArgs$type, rep('number', npcValues))
            private$colArgs$visible <- c(colArgs$visible, rep("(pcValues)", npcValues))

        },
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
                skew <- private$.skewness(column)
                kurt <- private$.kurtosis(column)
                norm <- jmvcore::tryNaN(shapiro.test(column)$p.value)
                normw <- jmvcore::tryNaN(shapiro.test(column)$statistic)
                stats[['skew']] <- skew$skew
                stats[['seSkew']] <- skew$seSkew
                stats[['kurt']] <- kurt$kurt
                stats[['seKurt']] <- kurt$seKurt
                stats[['sww']] <- normw
                stats[['sw']] <- norm

                stats[['quart1']] <- as.numeric(quantile(column, c(.25)))
                stats[['quart2']] <- as.numeric(quantile(column, c(.5)))
                stats[['quart3']] <- as.numeric(quantile(column, c(.75)))

                pcNEqGr <- self$options$pcNEqGr

                if ( ! self$options$quart || pcNEqGr != 4) {
                    pcEq <- (1:pcNEqGr / pcNEqGr)[-pcNEqGr]
                    quants <- as.numeric(quantile(column, pcEq))

                    for (i in 1:(pcNEqGr-1))
                        stats[[paste0('quant', i)]] <- quants[i]
                }

                pcValues <- as.numeric(unlist(strsplit(self$options$pcValues,",")))
                pcValues[pcValues < 0 | pcValues > 1] <- NA 
                pcValues <- pcValues[!is.na(pcValues)]
                npcValues <- length(pcValues)

                if (self$options$pcVal && npcValues > 0) {
                    quants <- as.numeric(quantile(column, pcValues))
                    for (i in 1:npcValues)
                        stats[[paste0('perc', i)]] <- quants[i]
                }

            } else if (jmvcore::canBeNumeric(column)) {

                l <- list(mean=NaN, median=NaN, mode=NaN, sum=NaN, sd=NaN, variance=NaN,
                          range=NaN, min=NaN, max=NaN, se=NaN, skew=NaN, seSkew=NaN,
                          kurt=NaN, seKurt=NaN, sw=NaN, quart1=NaN, quart2=NaN, quart3=NaN)

                pcNEqGr <- self$options$pcNEqGr
                if ( ! (self$options$quart && pcNEqGr == 4)) {
                    for (i in 1:(pcNEqGr-1))
                        l[[paste0('quant', i)]] <- NaN
                }

                pcValues <- as.numeric(unlist(strsplit(self$options$pcValues,",")))
                pcValues[pcValues < 0 | pcValues > 1] <- NA 
                pcValues <- pcValues[!is.na(pcValues)]
                npcValues <- length(pcValues)
                if (! self$options$pcVal || npcValues == 0) {
                    for (i in 1:npcValues)
                        l[[paste0('perc', i)]] <- NaN
                }

                stats <- append(stats, l)

            } else {

                l <- list(mean='', median='', mode='', sum='', sd='', variance='',
                          range='', min='', max='', se='', skew='', seSkew='',
                          kurt='', seKurt='', sw='', quart1='', quart2='', quart3='')

                pcNEqGr <- self$options$pcNEqGr
                if ( ! (self$options$quart && pcNEqGr == 4)) {
                    for (i in 1:(pcNEqGr-1))
                        l[[paste0('quant', i)]] <- ''
                }

                pcValues <- as.numeric(unlist(strsplit(self$options$pcValues,",")))
                pcValues[pcValues < 0 | pcValues > 1] <- NA 
                pcValues <- pcValues[!is.na(pcValues)]
                npcValues <- length(pcValues)
                if (! self$options$pcVal || npcValues == 0) {
                    for (i in 1:npcValues)
                        l[[paste0('perc', i)]] <- ''
                }

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
                legend <- max(25 + 21 + 3.5 + 8.3 * nCharLevels[1] + 28, 25 + 10 * nCharNames[1] + 28)

                width <- yAxis + width + ifelse(nLevels[1] > 1, legend, 0)
                height <- xAxis + height

            } else if (plot == "qq") {

                xAxis <- 30 + 20
                yAxis <- 45 + 11 + 8.3 * nCharLevels[1]

                if (nLevels[1] == 1) {
                    width <- 300
                    height <- 300
                } else if (nLevels[2] == 1) {
                    width <- 200 * nLevels[1]
                    height <- 200
                } else if (nLevels[3] == 1) {
                    width <- 200 * nLevels[2]
                    height <- 200 * nLevels[1]
                } else {
                    width <- 200 * nLevels[1] * nLevels[2]
                    height <- 200 * nLevels[3]
                }

                width <- yAxis + width
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
        },
        .kurtosis = function(x) {

            # https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-whats-with-the-different-formulas-for-kurtosis/

            n <- length(x)
            s2 <- sum((x - mean(x))^2)
            s4 <- sum((x - mean(x))^4)
            v <- s2 / (n-1)

            e1 <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))
            e2 <- s4 / (v^2)
            e3 <- (-3 * (n - 1)^2) / ((n - 2) * (n - 3))
            kurtosis <- e1 * e2 + e3

            varSkew <- 6 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3))
            varKurt <- 4 * (n^2 - 1) * varSkew / ((n - 3) * (n + 5))
            seKurt <- sqrt(varKurt)

            return(list(kurt=kurtosis, seKurt=seKurt))
        },
        .skewness = function(x) {

            n <- length(x)
            x <- x - mean(x)

            e1 <- sqrt(n * (n - 1))/(n - 2)
            e2 <- sqrt(n) * sum(x^3)/(sum(x^2)^(3/2))
            skewness <- e1 * e2

            varSkew <- 6 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3))
            seSkew <- sqrt(varSkew)

            return(list(skew=skewness, seSkew=seSkew))
        },
        .sourcifyOption = function(option) {
            if (option$name == 'vars' && length(self$options$splitBy) > 0)
                return('')
            if (option$name == 'splitBy')
                return('')
            super$.sourcifyOption(option)
        },
        .formula=function() {
            if (length(self$options$splitBy) == 0)
                return('')
            jmvcore:::composeFormula(self$options$vars, list(self$options$splitBy))
        }
    )
)
