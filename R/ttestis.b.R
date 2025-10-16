
#' @importFrom jmvcore .
ttestISClass <- R6::R6Class(
    "ttestISClass",
    inherit=ttestISBase,
    private=list(
        .run=function() {

            groupVarName <- self$options$group
            depVarNames <- self$options$vars
            varNames <- c(groupVarName, depVarNames)

            if (is.null(groupVarName) || length(depVarNames) == 0)
                return()

            data <- select(self$data, varNames)

            for (name in depVarNames)
                data[[name]] <- jmvcore::toNumeric(data[[name]])
            data[[groupVarName]] <- droplevels(as.factor(data[[groupVarName]]))

            ttestTable <- self$results$ttest
            descTable <- self$results$desc
            normTable <- self$results$assum$get('norm')
            eqvTable <- self$results$assum$get('eqv')

            confInt <- self$options$ciWidth / 100
            confIntES <- 1 - self$options$ciWidthES / 100

            if (any(depVarNames == groupVarName))
                jmvcore::reject(.("Grouping variable '{a}' must not also be a dependent variable"),
                                code="a_is_dependent_variable", a=groupVarName)

            # exclude rows with missings in the grouping variable
            data <- data[ ! is.na(data[[groupVarName]]),]

            # if (dimn(data)[1] == 0)
            #    jmvcore::reject("Grouping variable '{a}' must not also be a dependent variable",
            #                    code="a_is_dependent_variable", a=groupVarName)

            groupLevels <- base::levels(data[[groupVarName]])

            if (length(groupLevels) != 2)
                jmvcore::reject(.("Grouping variable '{a}' must have exactly 2 levels"),
                                code="grouping_var_must_have_2_levels", a=groupVarName)

            if (self$options$miss == "listwise") {
                data <- naOmit(data)
                if (dim(data)[1] == 0)
                    jmvcore::reject(.("Grouping variable '{a}' has less than 2 levels after missing values are excluded"),
                                    code="grouping_var_must_have_2_levels", a=groupVarName)
            }

            ## Hypothesis options checking
            if (self$options$hypothesis == 'oneGreater')
                Ha <- "greater"
            else if (self$options$hypothesis == 'twoGreater')
                Ha <- "less"
            else
                Ha <- "two.sided"

            for (depName in depVarNames) {

                dataTTest <- data.frame(dep=data[[depName]], group=data[[groupVarName]])

                if (self$options$miss == "perAnalysis")
                    dataTTest <- naOmit(dataTTest)

                groupLevels <- base::levels(dataTTest$group)
                v <- tapply(dataTTest$dep, dataTTest$group, function(x) tryNaN(var(x)))
                n <- tapply(dataTTest$dep, dataTTest$group, length)
                m <- tapply(dataTTest$dep, dataTTest$group, function(x) tryNaN(mean(x)))
                med <- tapply(dataTTest$dep, dataTTest$group, function(x) tryNaN(median(x)))
                se <- sqrt(v/n)
                sd <- sqrt(v)

                pooledVARSTUD <- tryNaN(((n[1]-1)*v[1]+(n[2]-1)*v[2])/(n[1]+n[2]-2))
                pooledVARWELC <- tryNaN((v[1] + v[2]) / 2)
                sediffSTUD <- tryNaN(sqrt((pooledVARSTUD/n[1])+(pooledVARSTUD/n[2])))
                sediffWELC <- tryNaN(sqrt((v[1]/n[1])+(v[2]/n[2])))

                dSTUD <- (m[1]-m[2])/sqrt(pooledVARSTUD) # Cohen's d for student's t
                dWELC <- (m[1]-m[2])/sqrt(pooledVARWELC) # Cohen's d for Welch's t
                dCISTUD <- psych::d.ci(dSTUD, n1=n[1], n2=n[2], alpha=confIntES)
                dCIWELC <- psych::d.ci(dWELC, n1=n[1], n2=n[2], alpha=confIntES)


                n[is.na(n)] <- 0
                m[is.na(m)] <- NaN
                med[is.na(med)] <- NaN
                se[is.na(se)] <- NaN
                sd[is.na(sd)] <- NaN
                sediffSTUD[is.na(sediffSTUD)] <- NaN
                sediffWELC[is.na(sediffWELC)] <- NaN
                pooledVARSTUD[is.na(pooledVARSTUD)] <- NaN
                dSTUD[is.na(dSTUD)] <- NaN

                ## Levene's test and equality of variances table

                levene <- try(car::leveneTest(dep ~ group, data=dataTTest, "mean"), silent=TRUE)

                if (isError(levene)) {

                    eqvTable$setRow(rowKey=depName, list("f"=NaN, "df"="", "df2"="", "p"=""))
                    eqvTable$addFootnote(rowKey=depName, "f", .("F-statistic could not be calculated"))

                } else if (is.na(levene[1,"F value"])) {

                    eqvTable$setRow(rowKey=depName, list("f"=NaN, "df"="", "df2"="", "p"=""))
                    eqvTable$addFootnote(rowKey=depName, "f", .("F-statistic could not be calculated"))

                } else {

                    eqvTable$setRow(rowKey=depName, list(
                        "f"=levene[1,"F value"],
                        "df"=levene[1,"Df"],
                        "df2"=levene[2,"Df"],
                        "p"=levene[1,"Pr(>F)"]))
                }


                if (self$options$students) {

                    if (is.factor(dataTTest$dep))
                        res <- createError(.('Variable is not numeric'))
                    else if (any(is.infinite(dataTTest$dep)))
                        res <- createError(.('Variable contains infinite values'))
                    else
                        res <- try(t.test(dep ~ group, data=dataTTest, var.equal=TRUE,
                                          alternative=Ha, conf.level=confInt), silent=TRUE)

                    if (isError(res)) {

                        ttestTable$setRow(rowKey=depName, list(
                            "stat[stud]"=NaN,
                            "df[stud]"='',
                            "p[stud]"='',
                            "md[stud]"='',
                            "sed[stud]"='',
                            "cil[stud]"='',
                            "ciu[stud]"='',
                            "es[stud]"='',
                            "ciles[stud]"='',
                            "ciues[stud]"=''))

                        message <- extractErrorMessage(res)
                        if (message == 'grouping factor must have exactly 2 levels')
                            message <- .('One or both groups do not contain enough observations')
                        else if (message == 'not enough observations')
                            message <- .('One or both groups do not contain enough observations')
                        else if (message == 'data are essentially constant')
                            message <- .('All observations are tied')

                        ttestTable$addFootnote(rowKey=depName, 'stat[stud]', message)

                    } else {

                        ttestTable$setRow(rowKey=depName, list(
                            "stat[stud]"=res$statistic,
                            "df[stud]"=res$parameter,
                            "p[stud]"=res$p.value,
                            "md[stud]"=res$estimate[1]-res$estimate[2],
                            "sed[stud]"=sediffSTUD,
                            "cil[stud]"=res$conf.int[1],
                            "ciu[stud]"=res$conf.int[2],
                            "es[stud]"=dSTUD,
                            "ciles[stud]"=dCISTUD[1],
                            "ciues[stud]"=dCISTUD[3]))
                    }

                    # ## Inform if a student's t-test is appropriate using Levene's test
                    if (!isError(levene) && !is.na(levene[1,"Pr(>F)"]) && levene[1,"Pr(>F)"] < .05)
                        ttestTable$addFootnote(rowKey=depName, "stat[stud]",
                                               .("Levene's test is significant (p < .05), suggesting a violation of the assumption of equal variances"))
                }

                if (self$options$welchs) {

                    if (is.factor(dataTTest$dep))
                        res <- createError(.('Variable is not numeric'))
                    else if (any(is.infinite(dataTTest$dep)))
                        res <- createError(.('Variable contains infinite values'))
                    else
                        res <- try(t.test(dep ~ group, data=dataTTest, var.equal=FALSE,
                                          alternative=Ha, conf.level=confInt), silent=TRUE)

                    if ( ! isError(res)) {

                        ttestTable$setRow(rowKey=depName, list(
                            "stat[welc]"=res$statistic,
                            "df[welc]"=res$parameter,
                            "p[welc]"=res$p.value,
                            "md[welc]"=res$estimate[1]-res$estimate[2],
                            "sed[welc]"=sediffWELC,
                            "cil[welc]"=res$conf.int[1],
                            "ciu[welc]"=res$conf.int[2],
                            "es[welc]"=dWELC,
                            "ciles[welc]"='',
                            "ciues[welc]"=''))

                    } else {

                        ttestTable$setRow(rowKey=depName, list(
                            "stat[welc]"=NaN,
                            "df[welc]"='',
                            "p[welc]"='',
                            "md[welc]"='',
                            "sed[welc]"='',
                            "cil[welc]"='',
                            "ciu[welc]"='',
                            "es[welc]"='',
                            "ciles[welc]"='',
                            "ciues[welc]"=''))

                        message <- extractErrorMessage(res)
                        if (message == 'grouping factor must have exactly 2 levels')
                            message <- .('One or both groups do not contain enough observations')
                        else if (message == "not enough 'x' observations")
                            message <- .('One or both groups do not contain enough observations')
                        else if (message == 'missing value where TRUE/FALSE needed')
                            message <- .('Variable contains infinite values')
                        else if (message == 'data are essentially constant')
                            message <- .('All observations are tied')

                        ttestTable$addFootnote(rowKey=depName, 'stat[welc]', message)
                    }
                }

                if (self$options$mann) {

                    if (is.factor(dataTTest$dep)) {
                        res <- createError(.('Variable is not numeric'))
                    }
                    else if (any(is.infinite(dataTTest$dep))) {
                        res <- createError(.('Variable contains infinite values'))
                    }
                    else {

                        x <- dataTTest$dep[dataTTest$group == groupLevels[1]]
                        y <- dataTTest$dep[dataTTest$group == groupLevels[2]]

                        if (self$options$hypothesis == 'oneGreater') {
                            Ha1 <- "greater"
                            Ha2 <- "less"
                        }
                        else if (self$options$hypothesis == 'twoGreater') {
                            Ha1 <- "less"
                            Ha2 <- "greater"
                        }
                        else {
                            Ha1 <- "two.sided"
                            Ha2 <- "two.sided"
                        }

                        res <- try(suppressWarnings(
                            wilcox.test(
                                x=x,
                                y=y,
                                alternative=Ha1,
                                exact=FALSE,
                                paired=FALSE,
                                conf.int=TRUE,
                                conf.level=confInt)
                            ), silent=TRUE)

                        res2 <- try(suppressWarnings(
                            wilcox.test(
                                x=y,
                                y=x,
                                alternative=Ha2,
                                exact=FALSE,
                                paired=FALSE,
                                conf.int=TRUE,
                                conf.level=confInt)
                            ), silent=TRUE)

                        m1 <- res$statistic
                        m2 <- res2$statistic
                        mm <- res$estimate
                        cil <- res$conf.int[1]
                        ciu <- res$conf.int[2]

                        if ( ! is.na(m1) && m2 < m1)
                            res <- res2

                        biSerial <- 1 - (2 * m1 / (n[1] * n[2]))
                    }

                    if ( ! isError(res)) {

                        ttestTable$setRow(rowKey=depName, list(
                            "stat[mann]"=res$statistic,
                            "df[mann]"=res$parameter,
                            "p[mann]"=res$p.value,
                            "md[mann]"=mm,
                            "sed[mann]"='',
                            "cil[mann]"=cil,
                            "ciu[mann]"=ciu,
                            "es[mann]"=biSerial,
                            "ciles[mann]"='',
                            "ciues[mann]"=''))

                    } else {

                        ttestTable$setRow(rowKey=depName, list(
                            "stat[mann]"=NaN,
                            "df[mann]"='',
                            "p[mann]"='',
                            "md[mann]"='',
                            "sed[mann]"='',
                            "cil[mann]"='',
                            "ciu[mann]"='',
                            "es[mann]"='',
                            "ciles[mann]"='',
                            "ciues[mann]"=''))

                        message <- extractErrorMessage(res)
                        if (message == 'grouping factor must have exactly 2 levels')
                            message <- .('One or both groups do not contain enough observations')
                        else if (message == 'not enough observations')
                            message <- .('One or both groups do not contain enough observations')
                        else if (message == 'cannot compute confidence interval when all observations are tied')
                            message <- .('All observations are tied')

                        ttestTable$addFootnote(rowKey=depName, 'stat[mann]', message)
                    }
                }

                if (self$options$norm) {

                    values <- list()
                    footnote <- NULL
                    values[["name"]] <- depName

                    if (length(dataTTest$dep) < 3) {
                        values[['w']] <- NaN
                        values[['p']] <- ''
                        footnote <- .('Too few samples to compute statistic (N < 3)')
                    } else if (length(dataTTest$dep) > 5000) {
                        values[['w']] <- NaN
                        values[['p']] <- ''
                        footnote <- .('Too many samples to compute statistic (N > 5000)')
                    } else {
                        residuals <- tapply(dataTTest$dep, dataTTest$group, function(x) x - mean(x))
                        residuals <- unlist(residuals, use.names=FALSE)
                        res <- try(shapiro.test(residuals), silent=TRUE)

                        if ( ! isError(res)) {
                            values[['w']] <- res$statistic
                            values[['p']] <- res$p.value
                        }
                        else {
                            values[['w']] <- NaN
                            values[['p']] <- ''
                        }
                    }

                    normTable$setRow(rowKey=depName, values)
                    if ( ! is.null(footnote))
                        normTable$addFootnote(rowKey=depName, 'w', footnote)
                }

                if (self$options$desc) {

                    descTable$setRow(rowKey=depName, list(
                        "dep"=depName,
                        "group[1]"=groupLevels[1],
                        "num[1]"=n[1],
                        "mean[1]"=m[1],
                        "sd[1]"=sd[1],
                        "se[1]"=se[1],
                        "med[1]"=med[1],
                        "group[2]"=groupLevels[2],
                        "num[2]"=n[2],
                        "mean[2]"=m[2],
                        "sd[2]"=sd[2],
                        "se[2]"=se[2],
                        "med[2]"=med[2]
                    ))
                }

                if (self$options$bf) {

                    if (is.factor(dataTTest$dep))
                        res <- createError(.('Variable is not numeric'))
                    else if (any(is.infinite(dataTTest$dep)))
                        res <- createError(.('Variable contains infinite values'))
                    else {

                        if (self$options$hypothesis == 'oneGreater') {
                            nullInterval <- c(0, Inf)
                        } else if (self$options$hypothesis == 'twoGreater') {
                            nullInterval <- c(-Inf, 0)
                        } else {
                            nullInterval <- NULL
                        }

                        rscale <- self$options$bfPrior

                        res <- try(BayesFactor::ttestBF(formula=dep ~ group, data=dataTTest, paired=FALSE,
                                                        nullInterval=nullInterval, rscale=rscale), silent=TRUE)
                    }

                    if (isError(res)) {

                        ttestTable$setRow(rowKey=depName, list(
                            "stat[bf]"=NaN,
                            "err[bf]"=''))

                        message <- extractErrorMessage(res)
                        if (message == 'grouping factor must have exactly 2 levels')
                            message <- .('One or both groups do not contain enough observations')
                        else if (message == 'not enough observations')
                            message <- .('One or both groups do not contain enough observations')
                        else if (message == 'Dependent variable must be numeric.')
                            message <- .('Variable is not numeric')
                        else if (message == 'data are essentially constant')
                            message <- .('All observations are tied')
                        ttestTable$addFootnote(rowKey=depName, 'stat[bf]', message)

                    } else {

                        extracted <- BayesFactor::extractBF(res)
                        error <- extracted$error[1]
                        bf <- extracted$bf[1]
                        if (is.na(error))
                            error <- NaN
                        if ( ! is.numeric(bf))
                            bf <- NaN

                        ttestTable$setRow(rowKey=depName, list(
                            "stat[bf]"=bf,
                            "err[bf]"=error))

                        if ( ! is.na(bf) && bf < 1)
                            ttestTable$addFormat(col='stat[bf]', rowKey=depName, Cell.NEGATIVE)
                    }
                }

                if (self$options$qq) {
                    image <- self$results$plots$get(key=depName)$qq
                    image$setState(depName)
                }

                if (self$options$plots) {

                    image <- self$results$plots$get(key=depName)$desc

                    if (nrow(dataTTest) > 0) {

                        ciWidth <- self$options$ciWidth
                        tail <- qnorm(1 - (100 - ciWidth) / 200)

                        means <- aggregate(dataTTest$dep, by=list(dataTTest$group),
                                           function(x) tryNaN(mean(x)), simplify=FALSE)
                        cies  <- aggregate(dataTTest$dep, by=list(dataTTest$group),
                                           function(x) { tail * tryNaN(sd(x)) / sqrt(length(x)) }, simplify=FALSE)
                        medians <- aggregate(dataTTest$dep, by=list(dataTTest$group),
                                             function(x) tryNaN(median(x)), simplify=FALSE)

                        meanPlotData <- data.frame(group=means$Group.1)
                        meanPlotData <- cbind(meanPlotData, stat=unlist(means$x))
                        meanPlotData <- cbind(meanPlotData, cie=unlist(cies$x))
                        meanPlotData <- cbind(meanPlotData, type='mean')

                        medianPlotData <- data.frame(group=medians$Group.1)
                        medianPlotData <- cbind(medianPlotData, stat=unlist(medians$x))
                        medianPlotData <- cbind(medianPlotData, cie=NA)
                        medianPlotData <- cbind(medianPlotData, type='median')

                        plotData <- rbind(meanPlotData, medianPlotData)

                        if (all(is.na(plotData$stat)))
                            image$setState(NULL)
                        else
                            image$setState(plotData)

                    } else {

                        image$setState(NULL)
                    }
                }
            }
        },
        .init=function() {

            hypothesis <- self$options$hypothesis
            groupName <- self$options$group

            groups <- NULL
            if ( ! is.null(groupName))
                groups <- base::levels(self$data[[groupName]])
            if (length(groups) != 2)
                groups <- c('Group 1', 'Group 2')

            table <- self$results$ttest

            ciTitleString <- .('{ciWidth}% Confidence Interval')

            ciTitle <- jmvcore::format(ciTitleString, ciWidth=self$options$ciWidth)
            table$getColumn('ciu[stud]')$setSuperTitle(ciTitle)
            table$getColumn('cil[stud]')$setSuperTitle(ciTitle)
            table$getColumn('ciu[bf]')$setSuperTitle(ciTitle)
            table$getColumn('cil[bf]')$setSuperTitle(ciTitle)
            table$getColumn('ciu[welc]')$setSuperTitle(ciTitle)
            table$getColumn('cil[welc]')$setSuperTitle(ciTitle)
            table$getColumn('ciu[mann]')$setSuperTitle(ciTitle)
            table$getColumn('cil[mann]')$setSuperTitle(ciTitle)

            ciTitleES <- jmvcore::format(ciTitleString, ciWidth=self$options$ciWidthES)
            table$getColumn('ciues[stud]')$setSuperTitle(ciTitleES)
            table$getColumn('ciles[stud]')$setSuperTitle(ciTitleES)
            table$getColumn('ciues[bf]')$setSuperTitle(ciTitleES)
            table$getColumn('ciles[bf]')$setSuperTitle(ciTitleES)
            table$getColumn('ciues[welc]')$setSuperTitle(ciTitleES)
            table$getColumn('ciles[welc]')$setSuperTitle(ciTitleES)
            table$getColumn('ciues[mann]')$setSuperTitle(ciTitleES)
            table$getColumn('ciles[mann]')$setSuperTitle(ciTitleES)

            if (hypothesis == 'oneGreater')
                table$setNote("hyp", jmvcore::format("H\u2090 \u03BC\u2009<sub>{}</sub> > \u03BC\u2009<sub>{}</sub>", groups[1], groups[2]))
            else if (hypothesis == 'twoGreater')
                table$setNote("hyp", jmvcore::format("H\u2090 \u03BC\u2009<sub>{}</sub> < \u03BC\u2009<sub>{}</sub>", groups[1], groups[2]))
            else
                table$setNote("hyp", jmvcore::format("H\u2090 \u03BC\u2009<sub>{}</sub> \u2260 \u03BC\u2009<sub>{}</sub>", groups[1], groups[2]))

        },
        .desc=function(image, ggtheme, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            groupName <- self$options$group

            ciw <- self$options$ciWidth

            pd <- position_dodge(0.2)

            plot <- ggplot(data=image$state, aes(x=group, y=stat, shape=type)) +
                geom_errorbar(aes(x=group, ymin=stat-cie, ymax=stat+cie, width=.1),
                              size=.8, colour=theme$color[2], position=pd) +
                geom_point(aes(x=group, y=stat, shape=type), color=theme$color[1],
                           fill=theme$fill[1], size=3, position=pd) +
                labs(x=groupName, y=image$key) +
                scale_shape_manual(
                    name='',
                    values=c(mean=21, median=22),
                    labels=c(
                        mean=jmvcore::format(.('Mean ({ciWidth}% CI)'), ciWidth=ciw),
                        median=.('Median')
                    )
                ) +
                ggtheme +
                theme(
                    plot.title=ggplot2::element_text(margin=ggplot2::margin(b = 5.5 * 1.2)),
                    plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5)
                )

            return(plot)
        },
        .qq = function(image, ggtheme, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            y <- toNumeric(self$data[[image$state]])
            x <- self$data[[self$options$group]]

            pieces <- split(y, x)
            # scale groups individually
            pieces <- lapply(pieces, function(y) y - mean(y, na.rm=TRUE))
            # join back together
            y <- unsplit(pieces, x)
            y <- scale(y)

            data <- data.frame(x=x, y=y)

            plot <- ggplot(data=data) +
                geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
                stat_qq(aes(sample=y), size=2, colour=theme$color[1]) +
                xlab(.("Theoretical Quantiles")) +
                ylab(.("Standardized Residuals")) +
                ggtheme

            print(plot)

            return(TRUE)
        },
        .sourcifyOption = function(option) {
            if (option$name %in% c('deps', 'group'))
                return('')
            super$.sourcifyOption(option)
        },
        .formula=function() {
            jmvcore:::composeFormula(self$options$vars, self$options$group)
        }
    )
)
