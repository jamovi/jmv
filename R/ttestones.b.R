
#' @importFrom jmvcore .
ttestOneSClass <- R6::R6Class(
    "ttestOneSClass",
    inherit=ttestOneSBase,
    private=list(
        .run=function() {

            variables <- self$options$get("vars")

            dataset <- select(self$data, variables)
            naHandling <- self$options$get("miss")

            ttest <- self$results$get("ttest")
            desc <- self$results$get("descriptives")
            normality <- self$results$get("normality")

            testValue <- self$options$get("testValue")
            cl <- self$options$get("ciWidth")/100
            confIntES <- 1 - self$options$ciWidthES / 100

            ## Listwise NA cleanup
            if (naHandling == "listwise")
                dataset <- naOmit(dataset)

            ## Hypothesis options checking
            if (self$options$get("hypothesis") == "gt") {
                Ha <- "greater"
                # Footnote message TBC
            } else if (self$options$get("hypothesis") == "lt") {
                Ha <- "less"
                # Footnote message TBC
            } else {
                Ha <- "two.sided"
            }

            plotData <- data.frame(stat=numeric(), cie=numeric(), type=character(), var=character())

            for (i in seq_along(variables)) {

                name <- variables[[i]]
                column <- jmvcore::toNumeric(dataset[[name]])
                column <- naOmit(column)

                n <- length(column)

                m <- tryNaN(mean(column))
                med <- tryNaN(median(column))
                stdDev <- tryNaN(sd(column))
                se <- stdDev/sqrt(n)
                d <- (m-testValue)/stdDev #Cohen's d
                dCI <- psych::d.ci(d, n1=n, alpha=confIntES)

                if (is.na(m))
                    m <- NaN
                if (is.na(med))
                    med <- NaN
                if (is.na(stdDev))
                    stdDev <- NaN
                if (is.na(se))
                    se <- NaN

                if (self$options$get("students")) {

                    if (is.factor(column))
                        res <- createError(.('Variable is not numeric'))
                    else if (any(is.infinite(column)))
                        res <- createError(.('Variable contains infinite values'))
                    else
                        res <- try(t.test(column, mu=testValue, paired=FALSE, conf.level=cl,
                                          alternative=Ha), silent=TRUE)

                    if ( ! isError(res)) {

                        ttest$setRow(rowNo=i, list(
                            "stat[stud]"=res$statistic,
                            "df[stud]"=res$parameter,
                            "p[stud]"=res$p.value,
                            "md[stud]"=res$estimate - testValue,
                            "cil[stud]"=res$conf.int[1] - testValue,
                            "ciu[stud]"=res$conf.int[2] - testValue,
                            "es[stud]"=d,
                            "ciles[stud]"=dCI[1],
                            "ciues[stud]"=dCI[3]))

                    } else {

                        ttest$setRow(rowNo=i, list(
                            "stat[stud]"=NaN,
                            "df[stud]"='',
                            "p[stud]"='',
                            "md[stud]"='',
                            "cil[stud]"='',
                            "ciu[stud]"='',
                            "es[stud]"='',
                            "ciles[stud]"='',
                            "ciues[stud]"=''))

                        message <- extractErrorMessage(res)
                        if (message == "not enough 'x' observations")
                            message <- .('Variable does not contain enough observations')
                        else if (message == 'data are essentially constant')
                            message <- .('All observations are tied')
                        ttest$addFootnote(rowNo=i, 'stat[stud]', message)
                    }
                }

                if (self$options$wilcoxon || self$options$mann) {

                    if (is.factor(column))
                        res <- createError(.('Variable is not numeric'))
                    else if (length(column) == 0)
                        res <- createError(.('Variable does not contain enough observations'))
                    else
                        res <- try(suppressWarnings(wilcox.test(column, mu=testValue,
                                                                alternative=Ha,
                                                                paired=FALSE,
                                                                conf.int=TRUE,
                                                                conf.level=cl)), silent=TRUE)

                    if ( ! isError(res)) {

                        nTies <- sum(column == 0)
                        totalRankSum <- ((n-nTies) * ((n-nTies) + 1)) / 2
                        biSerial <- (2 * (res$statistic / totalRankSum)) - 1

                        ttest$setRow(rowNo=i, list(
                            "stat[wilc]"=res$statistic,
                            "p[wilc]"=res$p.value,
                            "md[wilc]"=res$estimate - testValue,
                            "cil[wilc]"=res$conf.int[1] - testValue,
                            "ciu[wilc]"=res$conf.int[2] - testValue,
                            "es[wilc]"=biSerial,
                            "ciles[wilc]"='',
                            "ciues[wilc]"=''))

                    } else {

                        ttest$setRow(rowNo=i, list(
                            "stat[wilc]"=NaN,
                            "p[wilc]"='',
                            "md[wilc]"='',
                            "cil[wilc]"='',
                            "ciu[wilc]"='',
                            "es[wilc]"='',
                            "ciles[wilc]"='',
                            "ciues[wilc]"=''))

                        message <- extractErrorMessage(res)
                        if (message == 'cannot compute confidence interval when all observations are tied')
                            message <- .('All observations are tied')
                        ttest$addFootnote(rowNo=i, 'stat[wilc]', message)
                    }
                }

                # Normality test table
                if (n < 3) {

                    normality$addFootnote(rowNo=i, "w",
                                          .("Too few observations (N < 3) to compute statistic"))
                    res <- list(statistic=NaN, p.value='')

                } else if (n > 5000) {

                    normality$addFootnote(rowNo=i, "w",
                                          .("Too many observations (N > 5000) to compute statistic"))
                    res <- list(statistic=NaN, p.value='')

                }
                else {

                    res <- try(shapiro.test(column - testValue), silent=TRUE)
                    if (isError(res))
                        res <- list(statistic=NaN, p.value='')
                }

                normality$setRow(rowNo=i, list(
                    w=res$statistic,
                    p=res$p.value))

                ## Descriptives table
                desc$setRow(rowNo=i, list(num=n, mean=m, median=med, sd=stdDev, se=se))

                if (self$options$get('plots')) {

                    image <- self$results$get('plots')

                    ciWidth <- self$options$get('ciWidth')
                    tail <- qnorm(1 - (100 - ciWidth) / 200)

                    plotData <- rbind(plotData, list(
                        stat=m,
                        cie=tail * tryNaN(sd(column)) / sqrt(length(column)),
                        type='mean',
                        var=name), stringsAsFactors=FALSE)
                    plotData <- rbind(plotData, list(stat=med, cie=NA, type='median', var=name),
                                      stringsAsFactors=FALSE)

                    if (all(is.na(plotData$stat)))
                        image$setState(NULL)
                    else
                        image$setState(plotData)
                }

                if (self$options$get('bf')) {

                    if (is.factor(column)) {
                        res <- createError(.('Variable is not numeric'))
                    } else if (any(is.infinite(column))) {
                        res <- createError(.('Variable contains infinite values'))
                    } else {

                        if (self$options$get('hypothesis') == 'gt') {
                            nullInterval <- c(0, Inf)
                        } else if (self$options$get('hypothesis') == 'lt') {
                            nullInterval <- c(-Inf, 0)
                        } else {
                            nullInterval <- NULL
                        }

                        data <- column - testValue
                        rscale <- self$options$get('bfPrior')

                        res <- try(BayesFactor::ttestBF(x=data, nullInterval=nullInterval,
                                                        rscale=rscale), silent=TRUE)
                    }

                    if (isError(res)) {

                        ttest$setRow(rowKey=name, list(
                            "stat[bf]"=NaN,
                            "err[bf]"=''))

                        message <- extractErrorMessage(res)
                        if (message == 'not enough observations')
                            message = .('Variable does not contain enough observations')
                        else if (message == 'data are essentially constant')
                            message <- .('All observations are tied')
                        ttest$addFootnote(rowKey=name, 'stat[bf]', message)

                    } else {

                        extracted <- BayesFactor::extractBF(res)
                        error <- extracted$error[1]
                        bf <- extracted$bf[1]
                        if (is.na(error))
                            error <- NaN
                        if ( ! is.numeric(bf))
                            bf <- NaN

                        ttest$setRow(rowKey=name, list(
                            "stat[bf]"=extracted$bf[1],
                            "err[bf]"=error))

                        if ( ! is.na(bf) && bf < 1)
                            ttest$addFormat(col='stat[bf]', rowKey=name, Cell.NEGATIVE)
                    }
                }
            }
        },
        .init=function() {

            hypothesis <- self$options$get('hypothesis')
            testValue  <- self$options$get('testValue')
            table <- self$results$get("ttest")

            ciTitleString <- .('{ciWidth}% Confidence Interval')

            ciTitle <- jmvcore::format(ciTitleString, ciWidth=self$options$ciWidth)
            table$getColumn('ciu[stud]')$setSuperTitle(ciTitle)
            table$getColumn('cil[stud]')$setSuperTitle(ciTitle)
            table$getColumn('ciu[bf]')$setSuperTitle(ciTitle)
            table$getColumn('cil[bf]')$setSuperTitle(ciTitle)
            table$getColumn('ciu[wilc]')$setSuperTitle(ciTitle)
            table$getColumn('cil[wilc]')$setSuperTitle(ciTitle)

            ciTitleES <- jmvcore::format(ciTitleString, ciWidth=self$options$ciWidthES)
            table$getColumn('ciues[stud]')$setSuperTitle(ciTitleES)
            table$getColumn('ciles[stud]')$setSuperTitle(ciTitleES)
            table$getColumn('ciues[bf]')$setSuperTitle(ciTitleES)
            table$getColumn('ciles[bf]')$setSuperTitle(ciTitleES)
            table$getColumn('ciues[wilc]')$setSuperTitle(ciTitleES)
            table$getColumn('ciles[wilc]')$setSuperTitle(ciTitleES)

            if (hypothesis == 'dt')
                table$setNote("hyp", jmvcore::format("H\u2090 \u03BC \u2260 {}", testValue))
            else if (hypothesis == 'gt')
                table$setNote("hyp", jmvcore::format("H\u2090 \u03BC > {}", testValue))
            else if (hypothesis == 'lt')
                table$setNote("hyp", jmvcore::format("H\u2090 \u03BC < {}", testValue))
        },
        .desc=function(image, ggtheme, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            ciw <- self$options$get('ciWidth')
            testValue <- self$options$get("testValue")

            pd <- position_dodge(0.2)

            plot <- ggplot(data=image$state, aes(x=var, y=stat, shape=type)) +
                geom_hline(yintercept = testValue, linetype="dashed") +
                geom_errorbar(aes(x=var, ymin=stat-cie, ymax=stat+cie, shape=type, width=.1),
                              size=.8, colour=theme$color[1], position=pd) +
                geom_point(aes(x=var, y=stat, colour=type, shape=type),
                           fill=theme$fill[1], size=3, colour=theme$color[1], position=pd) +
                labs(x='', y='') +
                scale_shape_manual(name='', values=c(mean=21, median=22),
                                   labels=c(mean=paste0('Mean (', ciw, '% CI)'), median='Median')) +
                ggtheme +
                theme(plot.title = ggplot2::element_text(margin=ggplot2::margin(b = 5.5 * 1.2)),
                      plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))

            return(plot)
        },
        .qq=function(image, ggtheme, theme, ...) {

            y <- self$data[[image$key]]
            y <- toNumeric(y)
            y <- scale(y)

            data <- data.frame(y=y)

            plot <- ggplot(data=data) +
                geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
                stat_qq(aes(sample=y), size=2, colour=theme$color[1]) +
                xlab(.("Theoretical Quantiles")) +
                ylab(.("Standardized Residuals")) +
                ggtheme

            print(plot)

            return(TRUE)
        }
    )
)
