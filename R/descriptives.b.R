
#' @importFrom magrittr %>%
#' @importFrom jmvcore .
descriptivesClass <- R6::R6Class(
    "descriptivesClass",
    inherit=descriptivesBase,
    #### Active bindings ----
    active = list(
        weights = function() {
            if (is.null(private$.weights))
                private$.weights <- private$.computeWeights()

            return(private$.weights)
        },
        isWeighted = function() {
            if (is.null(private$.isWeighted))
                private$.isWeighted <- ! is.null(attr(self$data, "jmv-weights-name"))

            return(private$.isWeighted)
        },
        anyPlots = function() {
            if (is.null(private$.anyPlots)) {
                private$.anyPlots <- self$options$hist || self$options$dens || self$options$box ||
                    self$options$violin || self$options$dot || self$options$qq || self$options$bar
            }

            return(private$.anyPlots)
        },
        anyNonWeightSupportedStats = function() {
            if (is.null(private$.anyNonWeightSupportedStats)) {
                private$.anyNonWeightSupportedStats <- self$options$se || self$options$ci ||
                    self$options$mode || self$options$skew || self$options$kurt || self$options$sw ||
                    self$options$pc || self$options$pcEqGr || self$options$iqr
            }

            return(private$.anyNonWeightSupportedStats)
        }
    ),
    private=list(
        #### Member variables ----
        colArgs = NA,
        .weights = NULL,
        .isWeighted = NULL,
        .anyPlots = NULL,
        .anyNonWeightSupportedStats = NULL,
        .levels = NULL,
        .splitByGrid = NULL,

        #### Init + run functions ----
        .init = function() {
            private$colArgs <- list(
                name = c(
                    "n", "missing", "mean", "se", "ciLower", "ciUpper", "median",
                    "mode", "sum", "sd", "variance", "iqr", "range", "min", "max",
                    "skew", "seSkew", "kurt", "seKurt", "sww", "sw"
                ),
                title = c(
                    .("N"), .("Missing"), .("Mean"), .("Std. error mean"), .("lower bound"),
                    .("upper bound"), .("Median"), .("Mode"), .("Sum"), .("Standard deviation"),
                    .("Variance"), .("IQR"), .("Range"), .("Minimum"), .("Maximum"), .("Skewness"),
                    .("Std. error skewness"), .("Kurtosis"), .("Std. error kurtosis"),
                    .("Shapiro-Wilk W"), .("Shapiro-Wilk p")
                ),
                titleT = c(
                    .("N"), .("Missing"), .("Mean"), .("SE"), .("Lower"), .("Upper"), .("Median"),
                    .("Mode"), .("Sum"), .("SD"), .("Variance"), .("IQR"), .("Range"), .("Minimum"),
                    .("Maximum"), .("Skewness"), .("SE"), .("Kurtosis"), .("SE"), .("W"), .("p")
                ),
                superTitle = c(
                    rep("", 4), rep("ci", 2), rep("", 9), rep(.("Skewness"), 2),
                    rep(.("Kurtosis"), 2), rep(.("Shapiro-Wilk"), 2)
                ),
                type = rep("number", 21),
                format = c(rep("", 20), "zto,pvalue"),
                visible = c(
                    "(n)", "(missing)", "(mean)", "(se)", "(ci)", "(ci)",
                    "(median)", "(mode)", "(sum)", "(sd)", "(variance)", "(iqr)",
                    "(range)", "(min)", "(max)", "(skew)", "(skew)", "(kurt)",
                    "(kurt)", "(sw)", "(sw)"
                ),
                supportsWeights = c(
                    rep(TRUE, 3), rep(FALSE, 3), TRUE, FALSE, rep(TRUE, 7), rep(FALSE, 6)
                )
            )

            private$.addQuantiles()

            private$.errorCheck()

            private$.initDescriptivesTable()
            private$.initDescriptivesTTable()
            private$.initFrequencyTables()
            private$.initExtremeTables()
            private$.initPlots()

            private$.errorCheck()
        },
        .clear = function(vChanges, ...) {
            private$.clearDescriptivesTable(vChanges)
        },
        .run=function() {
            private$.errorCheck()

            if (length(self$options$vars) > 0) {
                results <- private$.compute()
                private$.populateDescriptivesTable(results)
                private$.populateDescriptivesTTable(results)
                private$.populateFrequencyTables(results)
                private$.populateExtremeTables(results)
                private$.preparePlots()
            }
        },

        #### Compute results ----
        .compute = function() {
            data <- self$data
            vars <- self$options$vars
            splitBy <- self$options$splitBy

            desc <- list()
            freq <- list()
            extreme <- list()
            for (var in vars) {
                column <- data[[var]]
                if (is.factor(column)) {
                    freq[[var]] <- private$.computeFreq(var, splitBy, self$weights)
                }

                extreme[[var]] <- private$.computeExtreme(
                    data.frame(rows=rownames(self$data), values=column)
                )

                column <- jmvcore::toNumeric(column)

                if (length(splitBy) > 0) {
                    df <- data.frame(column=column)
                    if (! is.null(self$weights))
                        df$weights <- self$weights

                    desc[[var]] <- lapply(
                        split(df,  data[splitBy], drop = FALSE),
                        function(subset) {
                            private$.computeDesc(
                                column=subset$column, weights=subset$weights
                            )
                        }
                    )
                } else {
                    desc[[var]] <- private$.computeDesc(column=column, weights=self$weights)
                }
            }

            return(list(desc=desc, freq=freq, extreme=extreme))
        },
        .computeDesc = function(column, weights=NULL) {
            if (! is.null(weights))
                return(private$.computeDescWeighted(column, weights))

            return(private$.computeDescUnweighted(column))
        },
        .computeDescUnweighted = function(column) {
            stats <- list()

            total <- length(column)
            column <- jmvcore::naOmit(column)
            n <- length(column)
            stats[['n']] <- n
            stats[['missing']] <- total - n

            if (jmvcore::canBeNumeric(column) && n > 0) {
                stats[['mean']] <- mean(column)
                stats[['median']] <- median(column)
                stats[['mode']] <- as.numeric(
                    names(table(column)[ table(column) == max(table(column)) ])
                )
                stats[['sum']] <- sum(column)
                stats[['sd']] <- sd(column)
                stats[['variance']] <- var(column)
                stats[['range']] <- max(column)-min(column)
                stats[['min']] <- min(column)
                stats[['max']] <- max(column)
                stats[['se']] <- sqrt(var(column) / length(column))

                # Calculate CI of the mean based on a t distribution
                tCriticalValue <- 1 - ((1 - self$options$ciWidth/100) / 2)
                ciDiff <- qt(tCriticalValue, df=stats[['n']] - 1) * stats[['se']]
                stats[['ciLower']] <- stats[['mean']] - ciDiff
                stats[['ciUpper']] <- stats[['mean']] + ciDiff

                stats[['iqr']] <- diff(as.numeric(quantile(column, c(.25,.75))))

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

                if ( self$options$pcEqGr ) {
                    pcNEqGr <- self$options$pcNEqGr

                    pcEq <- (1:pcNEqGr / pcNEqGr)[-pcNEqGr]
                    quants <- as.numeric(quantile(column, pcEq))

                    for (i in 1:(pcNEqGr-1))
                        stats[[paste0('quant', i)]] <- quants[i]
                }

                if ( self$options$pc ) {
                    pcValues <- private$.getPcValues()
                    npcValues <- length(pcValues)

                    if ( npcValues > 0 ) {
                        quants <- as.numeric(quantile(column, pcValues))
                        for (i in 1:npcValues)
                            stats[[paste0('perc', i)]] <- quants[i]
                    }
                }
            } else {
                # Because these descriptives can only be calculated for numeric
                # values, the values are set to an empty string for non-numeric
                # columns. If the column is numeric, but has no values, the
                # values are set to NaN to indicate that the value could not
                # be calculated.
                val <- ifelse(jmvcore::canBeNumeric(column), NaN, "")

                l <- list(
                    mean=val, median=val, mode=val, sum=val, sd=val,
                    variance=val, range=val, min=val, max=val, se=val,
                    ciLower=val, ciUpper=val, skew=val, seSkew=val, kurt=val,
                    seKurt=val, sww=val, sw=val, iqr=val
                )

                if ( self$options$pcEqGr ) {
                    pcNEqGr <- self$options$pcNEqGr
                    for (i in 1:(pcNEqGr-1))
                        l[[paste0('quant', i)]] <- val
                }

                if ( self$options$pc ) {
                    pcValues <- private$.getPcValues()
                    npcValues <- length(pcValues)
                    if ( npcValues > 0 ) {
                        for (i in 1:npcValues)
                            l[[paste0('perc', i)]] <- val
                    }
                }

                stats <- append(stats, l)
            }

            return(stats)
        },
        .computeDescWeighted = function(column, weights) {
            stats <- list()

            df <- data.frame(column=column, weights=weights)

            total <- nrow(df)
            df <- jmvcore::naOmit(df)
            column <- df$column
            weights <- df$weights

            n <- sum(weights)
            stats[['n']] <- n
            stats[['missing']] <- total - nrow(df)

            if (jmvcore::canBeNumeric(column) && n > 0) {
                column <- jmvcore::toNumeric(column)

                stats[['mean']] <- matrixStats::weightedMean(column, weights)
                stats[['median']] <- matrixStats::weightedMedian(column, weights)
                stats[['sum']] <- sum(column * weights)
                stats[['sd']] <- matrixStats::weightedSd(column, weights)
                stats[['variance']] <- matrixStats::weightedVar(column, weights)
                stats[['range']] <- max(column) - min(column)
                stats[['min']] <- min(column)
                stats[['max']] <- max(column)

                stats[['mode']] <- NaN
                stats[['se']] <- NaN
                stats[['ciLower']] <- NaN
                stats[['ciUpper']] <- NaN

                stats[['iqr']] <- diff(as.numeric(Hmisc::wtd.quantile(column, weights=weights, probs=c(.25,.75))))
                stats[['skew']] <- NaN
                stats[['seSkew']] <- NaN
                stats[['kurt']] <- NaN
                stats[['seKurt']] <- NaN
                stats[['sww']] <- NaN
                stats[['sw']] <- NaN

                if ( self$options$pcEqGr ) {
                    pcNEqGr <- self$options$pcNEqGr

                    pcEq <- (1:pcNEqGr / pcNEqGr)[-pcNEqGr]
                    quants <- as.numeric(Hmisc::wtd.quantile(column, weights=weights, probs=pcEq))

                    for (i in 1:(pcNEqGr-1))
                        stats[[paste0('quant', i)]] <- quants[i]
                }

                if ( self$options$pc ) {
                    pcValues <- private$.getPcValues()
                    npcValues <- length(pcValues)

                    if ( npcValues > 0 ) {
                        quants <- as.numeric(Hmisc::wtd.quantile(column, weights=weights, probs=pcValues))
                        for (i in 1:npcValues)
                            stats[[paste0('perc', i)]] <- quants[i]
                    }
                }
            } else {
                # Because these descriptives can only be calculated for numeric
                # values, the values are set to an empty string for non-numeric
                # columns. If the column is numeric, but has no values, the
                # values are set to NaN to indicate that the value could not
                # be calculated.
                val <- ifelse(jmvcore::canBeNumeric(column), NaN, "")

                l <- list(
                    mean=val, median=val, mode=val, sum=val, sd=val,
                    variance=val, range=val, min=val, max=val, se=val,
                    ciLower=val, ciUpper=val, skew=val, seSkew=val, kurt=val,
                    seKurt=val, sww=val, sw=val, iqr=val
                )

                if ( self$options$pcEqGr ) {
                    pcNEqGr <- self$options$pcNEqGr
                    for (i in 1:(pcNEqGr-1))
                        l[[paste0('quant', i)]] <- val
                }

                if ( self$options$pc ) {
                    pcValues <- private$.getPcValues()
                    npcValues <- length(pcValues)
                    if ( npcValues > 0 ) {
                        for (i in 1:npcValues)
                            l[[paste0('perc', i)]] <- val
                    }
                }

                stats <- append(stats, l)
            }

            return(stats)
        },
        .computeFreq = function(var, splitBy, weights=NULL) {
            df <- jmvcore::select(self$data, c(var, splitBy))

            if (!is.null(weights)) {
                # Check for and exclude missing weights
                complete_cases <- complete.cases(weights)
                if (any( ! complete_cases)) {
                    weights <- weights[complete_cases]
                    df <- df[complete_cases, ]
                }

                return(tapply(weights, df, sum, default = 0))
            }

            return(table(df))
        },
        .computeExtreme = function(df) {
            extremeN = self$options$extremeN

            if (! jmvcore::canBeNumeric(df$values))
                return(NULL)

            df$values = jmvcore::toNumeric(df$values)

            lowest = head(df[order(df$values),], extremeN)
            highest <- head(df[order(-df$values),], extremeN)

            return(list(highest=highest, lowest=lowest))
        },
        .computeWeights = function() {
            weights <- attr(self$data, "jmv-weights")

            if (is.null(weights))
                return()

            if (any(na.omit(weights) < 0)) {
                jmvcore::reject(
                    .("'{var}' contains negative values. Negative weights are not permitted."),
                    var=weights
                )
            }

            return(weights)
        },

        #### Init tables/plots functions ----
        .initDescriptivesTable = function() {
            table <- self$results$descriptives

            if (self$options$desc != "columns") {
                table$setVisible(FALSE)
                return()
            }

            vars <- self$options$vars
            splitBy <- self$options$splitBy
            data <- self$data

            grid <- private$.getSplitByGrid()
            colArgs <- private$colArgs
            ciOptionVisible <- FALSE

            for (i in seq_along(colArgs$name)) {
                if (private$.skipOption(colArgs$visible[i]))
                    next

                name <- colArgs$name[i]
                title <- colArgs$title[i]
                format <- colArgs$format[i]
                type <- colArgs$type[i]
                visible <- colArgs$visible[i]
                supportsWeights <- colArgs$supportsWeights[i]

                if (name == "ciLower" || name == "ciUpper") {
                    title <- jmvcore::format(
                        .("{ciWidth}% CI mean {title}"), ciWidth=self$options$ciWidth, title=title
                    )
                    ciOptionVisible <- TRUE
                }

                if (length(splitBy) > 0) {
                    for (j in seq_len(nrow(grid))) {
                        post <- paste0(
                            "[", name, paste0(grid[j,], collapse = ""), "]"
                        )
                        table$addColumn(
                            name=paste0("stat", post),
                            title="",
                            type="text",
                            value=title,
                            visible=visible,
                            combineBelow=TRUE
                        )

                        if (j == 1) {
                            table$addFormat(
                                rowNo=1, col=paste0("stat", post), Cell.BEGIN_GROUP
                            )
                        }

                        # Add weights not supported footnote for unsupported stats
                        if (self$isWeighted && ! supportsWeights) {
                            table$addFootnote(
                                paste0("stat", post),
                                .("Does not support weighted data yet."),
                                1
                            )
                        }

                        for (k in 1:ncol(grid)) {
                            table$addColumn(
                                name=paste0("var", k,  post),
                                title=splitBy[k],
                                type="text",
                                value=grid[j,k],
                                visible=visible,
                                combineBelow=TRUE
                            )
                        }

                        for (k in seq_along(vars)) {
                            subName <- paste0(vars[k], post)
                            table$addColumn(
                                name=subName,
                                title=vars[k],
                                type=type,
                                format=format,
                                visible=visible
                            )
                        }
                    }
                } else {
                    post <- paste0("[", name, "]")
                    table$addColumn(
                        name=paste0("stat", post),
                        title="",
                        type="text",
                        value=title,
                        visible=visible,
                        combineBelow=TRUE
                    )

                    # Add weights not supported footnote for unsupported stats
                    if (self$isWeighted && ! supportsWeights) {
                        table$addFootnote(
                            paste0("stat", post),
                            .("Does not support weighted data yet."),
                            1
                        )
                    }

                    for (k in seq_along(vars)) {
                        subName <- paste0(vars[k], post)
                        table$addColumn(
                            name=subName,
                            title=vars[k],
                            type=type,
                            format=format,
                            visible=visible
                        )
                    }
                }

                if (ciOptionVisible) {
                    table$setNote(
                        "ci",
                        .("The CI of the mean assumes sample means follow a t-distribution with N - 1 degrees of freedom")
                    )
                }
            }
        },
        .initDescriptivesTTable = function() {
            table <- self$results$descriptivesT

            if (self$options$desc != "rows") {
                table$setVisible(FALSE)
                return()
            }

            splitBy <- self$options$splitBy
            colArgs <- private$colArgs

            table$addColumn(
                name="vars", title="", type="text", combineBelow=TRUE
            )
            for (i in seq_along(splitBy)) {
                table$addColumn(
                    name=splitBy[i],
                    title=splitBy[i],
                    type="text",
                    combineBelow=TRUE
                )
            }

            ciOptionVisible <- FALSE

            for (i in seq_along(colArgs$name)) {
                if (private$.skipOption(colArgs$visible[i]))
                    next

                if (colArgs$superTitle[i] == "ci") {
                    superTitle <- jmvcore::format(
                        .('{ciWidth}% Confidence Interval'), ciWidth=self$options$ciWidth
                    )
                    ciOptionVisible <- TRUE
                } else {
                    superTitle <- colArgs$superTitle[i]
                }

                table$addColumn(
                    name=colArgs$name[i],
                    title=colArgs$titleT[i],
                    type=colArgs$type[i],
                    format=colArgs$format[i],
                    visible=colArgs$visible[i],
                    superTitle=superTitle
                )
            }

            if (ciOptionVisible) {
                table$setNote(
                    "ci",
                    .("The CI of the mean assumes sample means follow a t-distribution with N - 1 degrees of freedom")
                )
            }

            if (self$isWeighted && self$anyNonWeightSupportedStats) {
                table$setNote(
                    "weighted",
                    .("Not all statistics support weighted data yet.")
                )
            }

            vars <- self$options$vars
            grid <- private$.getSplitByGrid()

            iter <- 1
            for (i in seq_along(vars)) {
                if (length(splitBy) > 0) {
                    for (j in seq_len(nrow(grid))) {
                        values <- list("vars"=vars[i])
                        for (k in seq_along(splitBy))
                            values[[splitBy[k]]] <- grid[j, k]

                        table$addRow(rowKey=iter, values=values)
                        if (j == 1)
                            table$addFormat(rowNo=iter, col=1, Cell.BEGIN_GROUP)

                        iter <- iter + 1
                    }
                } else {
                    table$addRow(rowKey=i, values=list(vars = vars[i]))
                }
            }
        },
        .initFrequencyTables = function() {
            if ( ! self$options$freq)
                return()

            tables <- self$results$frequencies
            vars <- self$options$vars
            splitBy <- self$options$splitBy

            for (i in seq_along(vars)) {
                var <- vars[i]
                column <- self$data[[var]]

                if (! is.factor(column))
                    next()

                tableVars <- c(var, splitBy)
                allLevels <- lapply(jmvcore::select(self$data, tableVars), levels)
                grid <- rev(expand.grid(rev(allLevels)))

                table <- tables$get(var)

                for (var in tableVars)
                    table$addColumn(name=var, title=var, type="text", combineBelow=TRUE)
                table$addColumn(name='counts', title=.('Counts'), type='number')
                table$addColumn(name='pc', title=.('% of Total'), type='number', format='pc')
                table$addColumn(name='cumpc', title=.('Cumulative %'), type='number', format='pc')

                for (row in seq_len(nrow(grid))) {
                    rowValues <- list()
                    for (col in tableVars)
                        rowValues[[col]] <- as.character(grid[row, col])
                    table$addRow(rowKey=row, values=rowValues)
                }
            }
        },
        .initExtremeTables = function() {
            if ( ! self$options$extreme)
                return()

            extremeN <- self$options$extremeN
            tables <- self$results$extremeValues
            vars <- self$options$vars

            for (i in seq_along(vars)) {
                var <- vars[i]
                table <- tables[[i]]

                if (! jmvcore::canBeNumeric(self$data[[var]])) {
                    table$setVisible(FALSE)
                    next()
                }

                table$addFormat(rowNo=extremeN+1, col=1, Cell.BEGIN_GROUP)

                iter <- 1
                for (n in seq_len(extremeN)) {
                    table$setRow(rowNo=iter, values=list(type="Highest", place=n))
                    iter <- iter + 1
                }

                for (n in seq_len(extremeN)) {
                    table$setRow(rowNo=iter, values=list(type="Lowest", place=n))
                    iter <- iter + 1
                }
            }
        },
        .initPlots = function() {
            plots <- self$results$plots

            if (self$anyPlots && self$isWeighted) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'warningMessage',
                    type = jmvcore::NoticeType$WARNING
                )

                warningMessage <- .("Plots are not yet supported for weighted descriptives.")

                notice$setContent(warningMessage)
                plots$setHeader(notice)

                return()
            }

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
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'warningMessage',
                    type = jmvcore::NoticeType$WARNING)

                if (length(varsCannotBeNumeric) == 1) {
                    warningMessage <- jmvcore::format(
                        .("The variable {var} cannot be treated as numeric. Plots that expect numeric data will not be created for this variable."),
                        var=listItems(self, varsCannotBeNumeric)
                    )
                } else {
                    warningMessage <- jmvcore::format(
                        .("The variables {vars} cannot be treated as numeric. Plots that expect numeric data will not be created for these variables."),
                        vars=listItems(self, varsCannotBeNumeric)
                    )
                }

                notice$setContent(warningMessage)
                plots$setHeader(notice)
            }

            for (var in vars) {
                group <- plots$get(var)
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
                            requiresData = TRUE,
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
                            requiresData = TRUE,
                            width = size[1],
                            height = size[2],
                            clearWith = list("splitBy", "box", "violin", "dot", "dotType", "boxMean", "boxLabelOutliers")
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
            }
        },

        #### Clear tables ----
        .clearDescriptivesTable = function(vChanges) {
            if (self$options$desc != "columns")
                return()

            table <- self$results$descriptives
            vars <- vChanges
            splitBy <- self$options$splitBy

            grid <- private$.getSplitByGrid()
            colNames <- private$colArgs$name

            values <- rep(
                NA,
                length(vars) * ifelse(length(splitBy) > 0, nrow(grid), 1) * length(colNames)
            )
            names <- rep(
                '',
                length(vars) * ifelse(length(splitBy) > 0, nrow(grid), 1) * length(colNames)
            )
            iter <- 1
            for (i in seq_along(vars)) {
                if (length(splitBy) > 0) {
                    for (j in seq_len(nrow(grid))) {
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
            if (self$options$desc != "columns")
                return()

            table <- self$results$descriptives
            vars <- self$options$vars
            splitBy <- self$options$splitBy

            grid <- private$.getSplitByGrid()
            colNames <- private$colArgs$name
            desc <- results$desc

            values <- list(); footnotes <- list()
            for (i in seq_along(vars)) {

                r <- desc[[vars[i]]]
                if (length(splitBy) > 0) {
                    for (j in seq_len(nrow(grid))) {
                        indices <- grid[j,]
                        stats <- do.call("[", c(list(r), paste0(indices, collapse = ".")))[[1]]

                        for (k in seq_along(colNames)) {
                            if (private$.skipOption(private$colArgs$visible[k]))
                                next

                            name <- colNames[k]
                            post <- paste0("[", name, paste0(grid[j,], collapse = ""), "]")
                            subName <- paste0(vars[i], post)

                            values[[subName]] <- stats[[name]][1]
                        }

                        if (self$options$mode && length(stats[['mode']]) > 1) {
                            post <- paste0("[mode", paste0(grid[j,], collapse = ""), "]")
                            subName <- paste0(vars[i], post)
                            footnotes <- c(footnotes, subName)
                        }
                    }
                } else {
                    for (k in seq_along(colNames)) {
                        if (private$.skipOption(private$colArgs$visible[k]))
                            next

                        name <- colNames[k]
                        post <- paste0("[", name, "]")
                        subName <- paste0(vars[i], post)

                        values[[subName]] <- r[[name]][1]
                    }

                    if (self$options$mode && length(r[['mode']]) > 1)
                        footnotes <- c(footnotes, paste0(vars[i], '[mode]'))
                }
            }
            table$setRow(rowNo=1, values=values)

            for (i in seq_along(footnotes)) {
                table$addFootnote(
                    rowNo=1,
                    footnotes[[i]],
                    .('More than one mode exists, only the first is reported')
                )
            }
        },
        .populateDescriptivesTTable = function(results) {
            if (self$options$desc != "rows")
                return()

            table <- self$results$descriptivesT
            vars <- self$options$vars
            splitBy <- self$options$splitBy

            grid <- private$.getSplitByGrid()
            colNames <- private$colArgs$name
            desc <- results$desc

            iter <- 1
            for (i in seq_along(vars)) {
                r <- desc[[vars[i]]]
                if (length(splitBy) > 0) {
                    for (j in seq_len(nrow(grid))) {
                        stats <- do.call("[", c(list(r), paste0(grid[j,], collapse = ".")))[[1]]
                        values <- list()
                        for (k in seq_along(colNames)) {
                            if (private$.skipOption(private$colArgs$visible[k]))
                                next

                            values[[ colNames[k] ]] <- stats[[ colNames[k] ]][1]
                        }
                        table$setRow(rowNo=iter, values=values)
                        if (self$options$mode && length(stats[['mode']]) > 1) {
                            table$addFootnote(
                                rowNo=iter,
                                'mode',
                                .('More than one mode exists, only the first is reported')
                            )
                        }

                        iter <- iter + 1
                    }
                } else {
                    values <- list()
                    for (k in seq_along(colNames)) {
                        if (private$.skipOption(private$colArgs$visible[k]))
                            next

                        values[[ colNames[k] ]] <- r[[ colNames[k] ]][1]
                    }
                    table$setRow(rowNo=i, values=values)
                    if (self$options$mode && length(r[['mode']]) > 1) {
                        table$addFootnote(
                            rowNo=i,
                            'mode',
                            .('More than one mode exists, only the first is reported')
                        )
                    }
                }
            }
        },
        .populateFrequencyTables = function(results) {
            if ( ! self$options$freq)
                return()

            tables <- self$results$frequencies
            vars <- self$options$vars
            splitBy <- self$options$splitBy

            freqs <- results$freq

            for (i in seq_along(vars)) {
                var <- vars[i]
                column <- self$data[[var]]

                if (! is.factor(column))
                    next()

                table <- tables$get(var)
                freq <- freqs[[var]]

                tableVars <- c(var, splitBy)
                allLevels <- lapply(jmvcore::select(self$data, tableVars), levels)
                grid <- rev(expand.grid(rev(allLevels)))

                n <- sum(freq)
                cumsum <- 0

                for (row in seq_len(nrow(grid))) {
                    counts <- as.numeric(do.call("[", c(list(freq), grid[row, ])))
                    cumsum <- cumsum + counts
                    pc <- counts / n
                    cumpc <- cumsum / n
                    if (is.na(pc)) pc <- 0
                    if (is.na(cumpc)) cumpc <- 0

                    table$setRow(rowNo=row, value=list(counts=counts, pc=pc, cumpc=cumpc))
                }
            }
        },
        .populateExtremeTables = function(results) {
            if ( ! self$options$extreme)
                return()

            extremeN <- self$options$extremeN
            tables <- self$results$extremeValues
            vars <- self$options$vars

            for (i in seq_along(vars)) {
                r <- results$extreme[[ vars[i] ]]

                if (is.null(r))
                    next()

                table <- tables[[i]]

                for (n in 1:nrow(r$highest)) {
                    table$setRow(
                        rowNo=n,
                        values=list(
                            row=r$highest[n,"rows"],
                            value=r$highest[n,"values"]
                        )
                    )
                }

                for (n in 1:nrow(r$lowest)) {
                    table$setRow(
                        rowNo=extremeN + n,
                        values=list(
                            row=r$lowest[n,"rows"],
                            value=r$lowest[n,"values"]
                        )
                    )
                }

                note <- .('Number of requested extreme values is higher than the number of rows in the data.')
                if (extremeN > nrow(r$highest))
                    table$setNote("insufficientData", note)
            }
        },

        #### Plot functions ----
        .preparePlots = function() {
            if (self$anyPlots && self$isWeighted)
                return()

            data <- self$data
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

                    if ( ! is.factor(column) && ! self$isWeighted) {
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
                            names <- list("x"="y", "s1"="s1", "s2"="s2", "s3"="s3", "y"="y")
                            labels <- list("x"=var, "s1"=splitBy[1], "s2"=splitBy[2], "s3"=splitBy[3])
                        } else if (length(splitBy) == 2) {
                            names <- list("x"="y", "s1"="s1", "s2"="s2", "s3"=NULL, "y"="y")
                            labels <- list("x"=var, "s1"=splitBy[1], "s2"=splitBy[2], "s3"=NULL)
                        } else if (length(splitBy) == 1) {
                            names <- list("x"="y", "s1"="s1", "s2"=NULL, "s3"=NULL, "y"="y")
                            labels <- list("x"=var, "s1"=splitBy[1], "s2"=NULL, "s3"=NULL)
                        } else {
                            names <- list("x"="y", "s1"=NULL, "s2"=NULL, "s3"=NULL, "y"="y")
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

                    if (
                        self$options$hist ||
                        self$options$dens ||
                        self$options$box ||
                        self$options$violin ||
                        self$options$dot
                    ) {
                        if (length(na.omit(column)) > 0) {
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
                            names <- list("x"="x", "s1"=NULL, "s2"=NULL, "s3"=NULL)
                            labels <- list("x"=var, "s1"=NULL, "s2"=NULL, "s3"=NULL)
                        }

                        if (self$options$hist || self$options$dens)
                            hist$setState(list(var=var, names=names, labels=labels))

                        if (self$options$box || self$options$violin || self$options$dot)
                            box$setState(list(var=var, names=names, labels=labels))
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
                xlab(.("Theoretical Quantiles")) +
                ylab(.("Standardized Residuals")) +
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

            names <- image$state$names
            labels <- image$state$labels
            splitBy <- self$options$splitBy
            var <- image$state$var

            data <- self$data
            column <- data[[var]]
            if (length(na.omit(column)) > 0) {
                columns <- na.omit(c(var, splitBy[1:3]))
                data <- naOmit(data[columns])
                data[[var]] <- jmvcore::toNumeric(data[[var]])
            } else {
                data <- data.frame(x=numeric())
            }
            colnames(data) <- as.character(unlist(names))

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

                min <- min(data[[names$x]])
                if (is.na(min))
                    min <- 0

                max <- max(data[[names$x]])
                if (is.na(max))
                    max <- 0

                range <- max - min

                nUniques <- length(unique(data[[names$x]]))
                if (nUniques > nBins)
                    binWidth <- range / nBins
                else
                    binWidth <- range / (nUniques - 1)

                plot <- ggplot(data=data, aes_string(x=names$x)) +
                    labs(x=labels$x, y='density')

                if (self$options$hist)
                    plot <- plot + geom_histogram(
                        aes(y=..density..),
                        position="identity",
                        stat="bin",
                        binwidth=binWidth,
                        color=color,
                        fill=fill
                    )

                if (self$options$dens)
                    plot <- plot + geom_density(color=color, fill=fill, alpha=alpha)

                themeSpec <- theme(axis.text.y=element_blank(),
                                   axis.ticks.y=element_blank())

            } else {
                if (nSplits == 1)
                    fill <- "s1"
                else if (nSplits == 2)
                    fill <- "s2"
                else
                    fill <- "s3"

                data$fillrev <- factor(data[[fill]], rev(levels(data[[fill]])))


                plot <- ggplot(data=data, aes(x=x, y=fillrev, fill=!!sym(fill), height=after_stat(density))) +
                    labs(x=labels$x, y=labels[[fill]]) +
                    scale_y_discrete(expand = c(0.05, 0)) +
                    scale_x_continuous(expand = c(0.01, 0))

                if (self$options$hist)
                    plot <- plot + ggridges::geom_density_ridges(stat="binline", bins=nBins, scale=0.9)

                if (self$options$dens)
                    plot <- plot + ggridges::geom_density_ridges(stat="density", scale=0.9, alpha=alpha)

                if (nSplits == 2) {
                    plot <- plot + facet_grid(cols=vars(s1))
                } else if (nSplits > 2) {
                    plot <- plot + facet_grid(cols=vars(s1), rows=vars(s2))
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
            type <- `if`(
                identical(image$state$type, 'continuous'),
                'continuous',
                'categorical'
            )

            fill <- theme$fill[2]
            color <- theme$color[1]
            pd <- position_dodge(0.85)

            plotSpecificTheme <- NULL

            if (type == 'categorical') {
                if (is.null(splitBy)) {
                    plot <-
                        ggplot(data=data, aes_string(x=names$x, y=names$y)) +
                        geom_bar(
                            stat="identity",
                            position="dodge",
                            width = 0.7,
                            fill=fill,
                            color=color
                        ) +
                        labs(x=labels$x, y='counts')
                } else {
                    plot <-
                        ggplot(
                            data=data,
                            aes_string(x=names$x, y=names$y, fill=names$s1)
                        ) +
                        geom_bar(
                            stat="identity",
                            position=pd,
                            width=0.7,
                            color='#333333'
                        ) +
                        labs(x=labels$x, y='counts', fill=labels$s1)

                    if (length(splitBy) == 2) {
                        plot <- plot +
                            facet_grid(as.formula(paste(". ~", names$s2)))
                    } else if (length(splitBy) > 2) {
                        plot <- plot +
                            facet_grid(as.formula(paste(names$s3, "~", names$s2)))
                    }
                }
            } else {
                if (length(splitBy) <= 1) {
                    if (is.null(names$s1))
                        names$s1 <- "x"

                    plot <- ggplot(data=data, aes_string(x=names$s1, y=names$x)) +
                        geom_col(
                            position="dodge", width = 0.7, fill=fill, color=color
                        ) +
                        geom_errorbar(
                            aes_string(y=names$x, ymin='sel', ymax='seu'), width=.1
                        ) +
                        labs(x=labels$s1, y=labels$x)

                    if (is.null(splitBy)) {
                        plotSpecificTheme <- theme(
                            axis.text.x = element_blank(),
                            axis.ticks.x = element_blank()
                        )
                    }
                } else {
                    plot <-
                        ggplot(
                            data=data,
                            aes_string(x=names$s1, y=names$x, fill=names$s2)
                        ) +
                        geom_col(position=pd, width = 0.7, color='#333333') +
                        geom_errorbar(
                            position=pd,
                            aes_string(ymin='sel', ymax='seu'),
                            width=.1
                        ) +
                        labs(x=labels$s1, y=labels$x, fill=labels$s2)

                    if (length(splitBy) > 2) {
                        plot <- plot +
                            facet_grid(as.formula(paste(". ~", names$s3)))
                    }
                }
            }
            plot <- plot + ggtheme + plotSpecificTheme

            return(plot)
        },
        .boxPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            type <- image$state$type
            names <- image$state$names
            labels <- image$state$labels
            splitBy <- self$options$splitBy
            var <- image$state$var

            data <- self$data
            column <- data[[var]]
            if (length(na.omit(column)) > 0) {
                columns <- na.omit(c(var, splitBy[1:3]))
                data <- naOmit(data[columns])
                data[[var]] <- jmvcore::toNumeric(data[[var]])
            } else {
                data <- data.frame(x=numeric())
            }
            colnames(data) <- as.character(unlist(names))

            fill <- theme$fill[2]
            color <- theme$color[2]
            themeSpec <- NULL

            # hide outliers if plotting the data
            outlierShape <- `if`(self$options$dot, NA, 19)

            if (is.null(splitBy) || length(splitBy) == 1) {
                data[["placeHolder"]] <- rep('var1', nrow(data))

                if (is.null(splitBy))
                    x <- "placeHolder"
                else
                    x <- names$s1

                if (self$options$box && self$options$boxLabelOutliers) {
                    data$.ROWNAMES <- rownames(data)
                    data <- data %>%
                        dplyr::group_by_at(x) %>%
                        dplyr::mutate(outlier=private$.isOutlier(x))
                }

                plot <- ggplot(data=data, aes_string(x=x, y=names$x)) +
                    labs(x=labels$s1, y=labels$x)

                if (self$options$violin) {
                    plot <- plot +
                        geom_violin(
                            fill=theme$fill[1], color=theme$color[1], alpha=0.5
                        )
                }

                if (self$options$dot) {
                    if (self$options$dotType == 'jitter') {
                        plot <- plot +
                            ggplot2::geom_jitter(
                                color=theme$color[1], width=0.1, alpha=0.4
                            )
                    } else if (self$options$dotType == 'stack') {
                        plot <- plot +
                            ggplot2::geom_dotplot(
                                binaxis="y",
                                stackdir="center",
                                color=theme$color[1],
                                alpha=0.4,
                                stackratio=0.9,
                                dotsize=0.7
                            )
                    }
                }

                if (self$options$box) {
                    plot <- plot +
                        ggplot2::geom_boxplot(
                            color=theme$color[1],
                            width=0.3,
                            alpha=0.9,
                            fill=theme$fill[2],
                            outlier.colour=theme$color[1],
                            outlier.shape=outlierShape
                        )

                    if (self$options$boxLabelOutliers) {
                        plot <- plot +
                            ggrepel::geom_label_repel(
                                data=. %>% dplyr::filter(outlier),
                                aes(label=.ROWNAMES),
                                position = position_dodge(0.8)
                            )
                    }
                }

                if (self$options$boxMean) {
                    plot <- plot +
                        stat_summary(
                            fun.y=mean,
                            geom="point",
                            shape=15,
                            size=3.5,
                            color=theme$color[1]
                        )
                }

                if (is.null(splitBy))
                    themeSpec <- theme(axis.text.x=element_blank(),
                                       axis.ticks.x=element_blank(),
                                       axis.title.x=element_blank())
            } else {
                if (length(splitBy) > 2) {
                    x <- names$s2
                    xLabel <- labels$s2
                    split <- names$s3
                    splitLabel <- labels$s3
                } else {
                    x <- names$s1
                    xLabel <- labels$s1
                    split <- names$s2
                    splitLabel <- labels$s2
                }

                plot <-
                    ggplot(
                        data=data,
                        aes_string(x=x, y=names$x, fill=split)
                    ) +
                    labs(
                        x=xLabel, y=labels$x, fill=splitLabel, color=splitLabel
                    )

                if (self$options$violin) {
                    plot <- plot +
                        ggplot2::geom_violin(
                            color=theme$color[1],
                            position=position_dodge(0.9),
                            alpha=0.3
                        )
                }

                if (self$options$dot) {
                    if (self$options$dotType == 'jitter') {
                        plot <-
                            plot +
                            ggplot2::geom_jitter(
                                aes_string(color=split),
                                alpha=0.7,
                                position=position_jitterdodge(
                                    jitter.width=0.1, dodge.width = 0.9
                                )
                            )
                    } else if (self$options$dotType == 'stack') {
                        plot <-
                            plot +
                            ggplot2::geom_dotplot(
                                binaxis = "y",
                                stackdir = "center",
                                color=theme$color[1],
                                alpha=0.4,
                                stackratio=0.9,
                                dotsize=0.7,
                                position=position_dodge(0.9)
                            )
                    }
                }

                if (self$options$box) {
                    plot <- plot +
                        ggplot2::geom_boxplot(
                            color=theme$color[1],
                            width=0.3,
                            alpha=0.8,
                            outlier.shape=outlierShape,
                            outlier.colour=theme$color[1],
                            position=position_dodge(0.9)
                        )
                }

                if (self$options$boxMean) {
                    plot <- plot +
                        stat_summary(
                            fun.y=mean,
                            geom="point",
                            shape=15,
                            size=3.5,
                            color=theme$color[1],
                            position=position_dodge(0.9),
                            show.legend = FALSE
                        )
                }
            }

            if (length(splitBy) > 2) {
                formula <- as.formula(paste(". ~", names$s1))
                plot <- plot + facet_grid(formula)
            }

            plot <- plot + ggtheme + themeSpec

            return(plot)
        },

        #### Helper functions ----
        .errorCheck = function() {
            data <- self$data
            splitBy <- self$options$splitBy

            if ( ! is.null(splitBy)) {
                for (item in splitBy) {
                    if ( ! is.factor(data[[item]])) {
                        jmvcore::reject(
                            .('Unable to split by a continuous variable'),
                            code=exceptions$valueError
                        )
                    } else if (length(levels(data[[item]])) == 0) {
                        jmvcore::reject(
                            .("The 'split by' variable '{var}' contains no data."),
                            code=exceptions$valueError,
                            var=item
                        )
                    }
                }
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
        .getPcValues = function() {
            if ( self$options$pcEqGr ) {
                pcNEqGr <- self$options$pcNEqGr
                pcEq <- (1:pcNEqGr / pcNEqGr)[-pcNEqGr]
                pcEq <- round(pcEq, 4)
            } else {
                pcEq <- NULL
            }

            pcValues<-self$options$pcValues
            if ( is.character(pcValues) )
                pcValues <- as.numeric(unlist(strsplit(pcValues,",")))
            pcValues <- pcValues / 100
            pcValues[pcValues < 0 | pcValues > 1] <- NA
            pcValues <- pcValues[!is.na(pcValues)]
            pcValues <- pcValues[ ! (pcValues %in% pcEq) ]

            return(pcValues)
        },
        .getLevels = function() {
            if (is.null(private$.levels)) {
                splitBy <- self$options$splitBy
                levels <- rep(list(NULL), length(splitBy))
                for (i in seq_along(splitBy)) {
                    lvls <- levels(self$data[[splitBy[i]]])
                    if (length(lvls) == 0) {
                        # error
                        splitBy <- NULL
                        levels <- list()
                        break()
                    }
                    levels[[i]] <- lvls
                }
                private$.levels <- levels
            }

            return(private$.levels)
        },
        .getSplitByGrid = function() {
            if (is.null(private$.splitByGrid)) {
                expandGrid <- function(...) expand.grid(..., stringsAsFactors = FALSE)
                private$.splitByGrid <- rev(do.call(expandGrid, rev(private$.getLevels())))
            }
            return(private$.splitByGrid)
        },
        .addQuantiles = function() {
            if ( self$options$pcEqGr ) {
                pcNEqGr <- self$options$pcNEqGr

                colArgs <- private$colArgs
                pcEq <- (1:pcNEqGr / pcNEqGr)[-pcNEqGr]

                private$colArgs$name <- c(colArgs$name, paste0('quant', 1:(pcNEqGr-1)))
                private$colArgs$title <- c(colArgs$title, paste0(round(pcEq * 100, 2), .('th percentile')))
                private$colArgs$titleT <- c(colArgs$titleT, paste0(round(pcEq * 100, 2), 'th'))
                private$colArgs$superTitle <- c(colArgs$superTitle, rep(.("Percentiles"), pcNEqGr-1))
                private$colArgs$type <- c(colArgs$type, rep('number', pcNEqGr - 1))
                private$colArgs$visible <- c(colArgs$visible, rep("(pcEqGr)", pcNEqGr - 1))
                private$colArgs$supportsWeights <- c(colArgs$supportsWeights, rep(TRUE, pcNEqGr - 1))
            }

            if ( self$options$pc ){
                pcValues <- private$.getPcValues()
                npcValues <- length(pcValues)

                if (npcValues > 0){
                    colArgs <- private$colArgs

                    private$colArgs$name <- c(colArgs$name, paste0('perc', 1:npcValues))
                    private$colArgs$title <- c(colArgs$title, paste0(round(pcValues * 100, 2), .('th percentile')))
                    private$colArgs$titleT <- c(colArgs$titleT, paste0(round(pcValues * 100, 2), 'th'))
                    private$colArgs$superTitle <- c(colArgs$superTitle, rep(.("Percentiles"), npcValues))
                    private$colArgs$type <- c(colArgs$type, rep('number', npcValues))
                    private$colArgs$visible <- c(colArgs$visible, rep("(pc)", npcValues))
                    private$colArgs$supportsWeights <- c(colArgs$supportsWeights, rep(TRUE, npcValues))
                }
            }
        },
        .skipOption = function(visible) {
            return(! self$options[[ gsub("[()]", "", visible) ]])
        },
        .plotSize = function(levels, plot) {
            nLevels <- as.numeric(sapply(levels, length))
            nLevels <- ifelse(is.na(nLevels[1:4]), 1, nLevels[1:4])
            nCharLevels <- as.numeric(sapply(lapply(levels, nchar), max))
            nCharLevels <- ifelse(is.na(nCharLevels[1:4]), 0, nCharLevels[1:4])
            nCharNames <- as.numeric(nchar(names(levels)))
            nCharNames <- ifelse(is.na(nCharNames[1:4]), 0, nCharNames[1:4])
            nSplits <- length(self$options$splitBy)

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

                legendVar <- min(max(nSplits, 1), 3)
                legend <- max(25 + 21 + 3.5 + 8.3 * nCharLevels[legendVar] + 28, 25 + 10 * nCharNames[legendVar] + 28)

                width <- yAxis + width + ifelse(nLevels[legendVar] > 1, legend, 0)
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
                yAxis <- 45 + 11
                width <- 300
                height <- 300

                if (nSplits == 1) {
                    yAxis <- yAxis + 8.3 * nCharLevels[1]
                    height <- max(height, 50 * nLevels[1])
                } else if (nSplits == 2) {
                    yAxis <- yAxis + 8.3 * nCharLevels[2]
                    width <- width * nLevels[1]
                    height <- max(height, 50 * nLevels[2])
                } else if (nSplits > 2) {
                    yAxis <- yAxis + 8.3 * nCharLevels[3]
                    width <- width * nLevels[1]
                    height <- max(height, 50 * nLevels[2] * nLevels[3])
                }

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
        },
        .isOutlier = function(x) {
            return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
        }
    )
)
