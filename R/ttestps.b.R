
#' @importFrom jmvcore .
ttestPSClass <- R6::R6Class(
    "ttestPSClass",
    inherit=ttestPSBase,
    private=list(
        .run=function() {

            data <- self$data

            ttestTable <- self$results$get('ttest')
            descTable <- self$results$get('desc')
            normTable <- self$results$get('norm')
            plots     <- self$results$get('plots')

            if (self$options$get('miss') == 'listwise')
                data <- naOmit(data)

            if (self$options$get("hypothesis") == "oneGreater")
                Ha <- "greater"
            else if (self$options$get("hypothesis") == "twoGreater")
                Ha <- "less"
            else
                Ha <- "two.sided"

            confInt <- self$options$get("ciWidth") / 100
            confIntES <- 1 - self$options$ciWidthES / 100

            pairs <- self$options$get('pairs')

            for (i in seq_along(pairs)) {

                pair <- pairs[[i]]

                name1 <- pair$i1
                name2 <- pair$i2

                if (is.null(name1) || is.null(name2))
                    next()

                data[[name1]] <- jmvcore::toNumeric(data[[name1]])
                data[[name2]] <- jmvcore::toNumeric(data[[name2]])

                if (self$options$get('miss') == "perAnalysis") {
                    pairsData <- naOmit(data[c(name1, name2)])
                    column1 <- pairsData[[name1]]
                    column2 <- pairsData[[name2]]
                } else {
                    column1 <- data[[name1]]
                    column2 <- data[[name2]]
                }

                var1 <- tryNaN(var(column1))
                var2 <- tryNaN(var(column2))
                n    <- length(column1)
                m1   <- tryNaN(mean(column1))
                m2   <- tryNaN(mean(column2))
                med1 <- tryNaN(median(column1))
                med2 <- tryNaN(median(column2))
                se1  <- sqrt(var1/n)
                se2  <- sqrt(var2/n)
                sd1  <- tryNaN(stats::sd(column1))
                sd2  <- tryNaN(stats::sd(column2))
                nTies <- sum((column1 - column2) == 0, na.rm=TRUE)

                pooledSD <- tryNaN(stats::sd(column1-column2))
                sediff <- pooledSD/sqrt(n)
                d <- (m1-m2)/pooledSD #Cohen's d
                dCI <- psych::d.ci(d, n1=n, alpha=confIntES)

                if (is.factor(column1) || is.factor(column2)) {
                    stud <- createError(.('One or both variables are not numeric'))
                    wilc <- createError(.('One or both variables are not numeric'))
                }
                else {
                    stud <- try(t.test(column1, column2, paired=TRUE, conf.level=confInt, alternative=Ha), silent=TRUE)
                    wilc <- try(suppressWarnings(wilcox.test(column1, column2, alternative=Ha, exact=FALSE, paired=TRUE, conf.int=TRUE, conf.level=confInt)), silent=TRUE)
                }

                if ( ! isError(stud)) {

                    ttestTable$setRow(rowKey=pair, list(
                        'stat[stud]'=stud$statistic,
                        'df[stud]'=stud$parameter,
                        'p[stud]'=stud$p.value,
                        'md[stud]'=stud$estimate,
                        'sed[stud]'=sediff,
                        'cil[stud]'=stud$conf.int[1],
                        'ciu[stud]'=stud$conf.int[2],
                        'es[stud]'=d,
                        "ciles[stud]"=dCI[1],
                        "ciues[stud]"=dCI[3]))

                } else {
                    ttestTable$setRow(rowKey=pair, list(
                        'stat[stud]'=NaN,
                        'df[stud]'='',
                        'p[stud]'='',
                        'md[stud]'='',
                        'sed[stud]'='',
                        'cil[stud]'='',
                        'ciu[stud]'='',
                        'es[stud]'='',
                        "ciles[stud]"='',
                        "ciues[stud]"=''))

                    message <- extractErrorMessage(stud)
                    if (message == "not enough 'x' observations")
                        message <- .('One or both variables do not contain enough observations')
                    else if (message == 'missing value where TRUE/FALSE needed')
                        message <- .('One or both variables contain infinite values')

                    ttestTable$addFootnote(rowKey=pair, 'stat[stud]', message)
                }

                if ( ! isError(wilc)) {

                    totalRankSum <- ((n-nTies) * ((n-nTies) + 1)) / 2
                    biSerial <- (2 * (wilc$statistic / totalRankSum)) - 1

                    ttestTable$setRow(rowKey=pair, list(
                        'stat[wilc]'=wilc$statistic,
                        'df[wilc]'=wilc$parameter,
                        'p[wilc]'=wilc$p.value,
                        'md[wilc]'=wilc$estimate,
                        'sed[wilc]'=sediff,
                        'cil[wilc]'=wilc$conf.int[1],
                        'ciu[wilc]'=wilc$conf.int[2],
                        'es[wilc]'=biSerial,
                        "ciles[wilc]"='',
                        "ciues[wilc]"=''))

                    if (nTies > 0) {
                        message <- jmvcore::format(.('{n} pair(s) of values were tied'), n=nTies)
                        ttestTable$addFootnote(rowKey=pair, 'stat[wilc]', message)
                    }

                } else {

                    ttestTable$setRow(rowKey=pair, list(
                        'stat[wilc]'=NaN,
                        'df[wilc]'='',
                        'p[wilc]'='',
                        'md[wilc]'='',
                        'sed[wilc]'='',
                        'cil[wilc]'='',
                        'ciu[wilc]'='',
                        'es[wilc]'='',
                        "ciles[wilc]"='',
                        "ciues[wilc]"=''))

                    message <- extractErrorMessage(wilc)
                    if (message == "not enough 'x' observations")
                        message <- .('One or both variables do not contain enough observations')
                    else if (message == 'missing value where TRUE/FALSE needed')
                        message <- .('One or both variables contain infinite values')
                    else if (message == 'cannot compute confidence interval when all observations are tied')
                        message <- .('All observations are tied')
                    else if (message == "'y' must be numeric")
                        message <- .('One or both variables contain no observations')

                    ttestTable$addFootnote(rowKey=pair, 'stat[wilc]', message)
                }


                ## Normality test table

                w <- NaN
                p <- ''

                if (n < 3) {
                    normTable$addFootnote(rowKey=pair, 'w', .("Too few observations (N < 3) to compute statistic"))
                }
                else if (n > 5000) {
                    normTable$addFootnote(rowKey=pair, 'w', .("Too many observations (N > 5000) to compute statistic"))
                }
                else {
                    res <- try(shapiro.test(column1-column2), silent=TRUE)
                    if ( ! base::inherits(res, 'try-error')) {
                        w <- res$statistic
                        p <- res$p.value
                    }
                }

                normTable$setRow(rowKey=pair, list(
                    var1=name1, var2=name2, w=w, p=p))

                row1Key <- paste0(pair$i1, i)
                row2Key <- paste0(pair$i2, i)

                descTable$setRow(rowKey=row1Key, list(
                    "name"=name1,
                    "num"=n,
                    "m"=m1,
                    "med"=med1,
                    "sd"=sd1,
                    "se"=se1))

                descTable$setRow(rowKey=row2Key, list(
                    "name"=name2,
                    "num"=n,
                    "m"=m2,
                    "med"=med2,
                    "sd"=sd2,
                    "se"=se2))

                descTable$addFormat(col='name', rowKey=row1Key, Cell.BEGIN_GROUP)
                descTable$addFormat(col='name', rowKey=row2Key, Cell.END_GROUP)

                if (self$options$get('bf')) {

                    if (is.factor(column1) || is.factor(column2)) {
                        res <- createError(.('One or both variables are not numeric'))
                    } else if (any(is.infinite(column1)) || any(is.infinite(column2))) {
                        res <- createError(.('One or both variables contain infinite values'))
                    } else {

                        if (self$options$get('hypothesis') == 'oneGreater') {
                            nullInterval <- c(0, Inf)
                        } else if (self$options$get('hypothesis') == 'twoGreater') {
                            nullInterval <- c(-Inf, 0)
                        } else {
                            nullInterval <- NULL
                        }

                        rscale <- self$options$get('bfPrior')

                        res <- try(BayesFactor::ttestBF(x=column1, y=column2, paired=TRUE, nullInterval=nullInterval, rscale=rscale), silent=TRUE)
                    }

                    if (isError(res)) {

                        ttestTable$setRow(rowKey=pair, list(
                            "stat[bf]"=NaN,
                            "err[bf]"=''))

                        message <- extractErrorMessage(res)
                        if (message == 'not enough observations')
                            message = .('One or both variables do not contain enough observations')
                        ttestTable$addFootnote(rowKey=pair, 'stat[bf]', message)

                    } else {

                        extracted <- BayesFactor::extractBF(res)
                        error <- extracted$error[1]
                        bf    <- extracted$bf[1]
                        if (is.na(error))
                            error <- NaN
                        if ( ! is.numeric(bf))
                            bf <- NaN

                        ttestTable$setRow(rowKey=pair, list(
                            "stat[bf]"=extracted$bf[1],
                            "err[bf]"=error))

                        if (! is.na(bf) && bf < 1)
                            ttestTable$addFormat(col='stat[bf]', rowKey=pair, Cell.NEGATIVE)
                    }
                }

                if (self$options$qq) {

                    image <- plots$get(key=pair)$qq
                    image$setState(pair)
                }

                if (self$options$plots) {

                    image <- plots$get(key=pair)$desc

                    ciWidth <- self$options$get('ciWidth')
                    tail <- qnorm(1 - (100 - ciWidth) / 200)

                    means <- c(tryNaN(mean(column1)), tryNaN(mean(column2)))
                    means[is.nan(means)] <- NA

                    cies  <- c(tail * tryNaN(sd(column1)) / sqrt(length(column1)), tail * tryNaN(sd(column2)) / sqrt(length(column2)))
                    medians <- c(tryNaN(median(column1)), tryNaN(median(column2)))

                    meanPlotData <- data.frame(group=c(name1, name2))
                    meanPlotData <- cbind(meanPlotData, stat=means)
                    meanPlotData <- cbind(meanPlotData, cie=cies)
                    meanPlotData <- cbind(meanPlotData, type='mean')

                    medianPlotData <- data.frame(group=c(name1, name2))
                    medianPlotData <- cbind(medianPlotData, stat=medians)
                    medianPlotData <- cbind(medianPlotData, cie=NA)
                    medianPlotData <- cbind(medianPlotData, type='median')

                    plotData <- rbind(meanPlotData, medianPlotData)
                    plotData$group <- factor(plotData$group, levels=unique(plotData$group))

                    if (all(is.na(plotData$stat)))
                        image$setState(NULL)
                    else
                        image$setState(plotData)
                }

            }
        },
        .init=function() {

            hypothesis <- self$options$get('hypothesis')
            ttestTable <- self$results$get('ttest')

            ciTitleString <- .('{ciWidth}% Confidence Interval')

            ciTitle <- jmvcore::format(ciTitleString, ciWidth=self$options$ciWidth)
            ttestTable$getColumn('ciu[stud]')$setSuperTitle(ciTitle)
            ttestTable$getColumn('cil[stud]')$setSuperTitle(ciTitle)
            ttestTable$getColumn('ciu[bf]')$setSuperTitle(ciTitle)
            ttestTable$getColumn('cil[bf]')$setSuperTitle(ciTitle)
            ttestTable$getColumn('ciu[wilc]')$setSuperTitle(ciTitle)
            ttestTable$getColumn('cil[wilc]')$setSuperTitle(ciTitle)

            ciTitleES <- jmvcore::format(ciTitleString, ciWidth=self$options$ciWidthES)
            ttestTable$getColumn('ciues[stud]')$setSuperTitle(ciTitleES)
            ttestTable$getColumn('ciles[stud]')$setSuperTitle(ciTitleES)
            ttestTable$getColumn('ciues[bf]')$setSuperTitle(ciTitleES)
            ttestTable$getColumn('ciles[bf]')$setSuperTitle(ciTitleES)
            ttestTable$getColumn('ciues[wilc]')$setSuperTitle(ciTitleES)
            ttestTable$getColumn('ciles[wilc]')$setSuperTitle(ciTitleES)

            if (hypothesis == 'oneGreater') {
                ttestTable$setNote(
                    "hyp",
                    jmvcore::format("H\u2090 \u03BC\u2009<sub>{}</sub> > 0", .("Measure 1 - Measure 2"))
                )
            } else if (hypothesis == 'twoGreater') {
                ttestTable$setNote(
                    "hyp",
                    jmvcore::format("H\u2090 \u03BC\u2009<sub>{}</sub> < 0", .("Measure 1 - Measure 2"))
                )
            } else {
                ttestTable$setNote(
                    "hyp",
                    jmvcore::format("H\u2090 \u03BC\u2009<sub>{}</sub> \u2260 0", .("Measure 1 - Measure 2"))
                )
            }

            pairs <- self$options$pairs
            descTable <- self$results$desc
            plots <- self$results$plots

            for (i in seq_along(pairs)) {

                pair <- pairs[[i]]

                ttestTable$setRow(rowKey=pair, list(
                    `var1[stud]`=pair$i1,
                    `var2[stud]`=pair$i2,
                    `var1[bf]`=pair$i1,
                    `var2[bf]`=pair$i2,
                    `var1[wilc]`=pair$i1,
                    `var2[wilc]`=pair$i2))

                row1Key <- paste0(pair$i1, i)
                row2Key <- paste0(pair$i2, i)
                descTable$addRow(row1Key)
                descTable$addRow(row2Key)

                plots$get(pair)$setTitle(paste0(pair, collapse=' - '))
            }
        },
        .desc=function(image, ggtheme, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            groupName <- self$options$get('group')
            ciw <- self$options$get('ciWidth')

            pd <- position_dodge(0.2)

            labels <- c(jmvcore::format(.('Mean ({ciWidth}% CI)'), ciWidth=ciw), .('Median'))

            plot <- ggplot(data=image$state, aes(x=group, y=stat, shape=type)) +
                geom_errorbar(aes(x=group, ymin=stat-cie, ymax=stat+cie, shape=type, width=.2), size=.8, colour=theme$color[1], position=pd) +
                geom_point(aes(x=group, y=stat, colour=type, shape=type), fill=theme$fill[1], size=3, colour=theme$color[1], position=pd) +
                labs(x=groupName, y=NULL) +
                scale_shape_manual(name='', values=c(mean=21, median=22), labels=labels) +
                ggtheme + theme(plot.title = ggplot2::element_text(margin=ggplot2::margin(b = 5.5 * 1.2)),
                              plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))

            return(plot)
        },
        .qq=function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            pair <- image$state
            y1 <- toNumeric(self$data[[ pair[[1]] ]])
            y2 <- toNumeric(self$data[[ pair[[2]] ]])
            y  <- y2 - y1
            y  <- scale(y)

            data <- data.frame(y=y)

            plot <- ggplot(data=data) +
                geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
                stat_qq(aes(sample=y), size=2, colour=theme$color[1]) +
                xlab(.("Theoretical Quantiles")) +
                ylab(.("Standardized Residuals")) +
                ggtheme

            return(plot)
        }
    )
)
