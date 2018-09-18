
anovaOneWClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "anovaOneWClass",
    inherit = anovaOneWBase,
    private = list(
        #### Init + run functions ----
        .init = function() {

            private$.initAnovaTable()
            private$.initDescTable()
            private$.initDescPlot()
            private$.initPostHocTables()

        },
        .run = function() {

            ready <- TRUE
            if (is.null(self$options$group) || length(self$options$deps) < 1)
                return()

            if (ready) {

                data <- private$.cleanData()
                results <- private$.compute(data)

                private$.populateAnovaTable(results)
                private$.populateDescTable(results)
                private$.populateLevenesTable(results)
                private$.populateShapiroWilkTable(results)
                private$.populatePostHocTables(results)
                private$.prepareDescPlot(results)
                private$.prepareQQPlot(results)

            }
        },

        #### Compute results ----
        .compute = function(data) {

            group <- self$options$group

            r <- list()
            for (dep in self$options$deps) {

                dataA <- data.frame(
                    dep = jmvcore::toNumeric(data[[dep]]),
                    group = data[[group]])

                if (self$options$miss != 'listwise')
                    dataA <- na.omit(dataA)

                welch <- oneway.test(dep ~ group, data=dataA, var.equal=FALSE)
                fisher <-  oneway.test(dep ~ group, data=dataA, var.equal=TRUE)
                residuals <- rstandard(lm(dep ~ group, data=dataA))

                desc <- tapply(dataA$dep, dataA$group, function (x) {
                    n <- length(x)
                    mean <- mean(x)
                    sd <- sd(x)
                    se <- sd / sqrt(n)
                    ci <- se * qt(95 / 200 + .5, n - 1)

                    return(c(n=n, mean=mean, sd=sd, se=se, ci=ci))
                })

                levene <- car::leveneTest(dep ~ group, dataA, center="mean")

                postHoc <- private$.postHoc(desc, method = self$options$phMethod)

                r[[dep]] <- list(welch=welch, fisher=fisher, desc=desc, postHoc=postHoc,
                                 levene=levene, residuals=residuals)
            }

            return(r)
        },

        #### Init tables/plots functions ----
        .initAnovaTable = function() {

            table <- self$results$anova

            if (self$options$fishers && !self$options$welchs)
                table$setTitle("One-Way ANOVA (Fisher's)")
            else if (self$options$welchs && !self$options$fishers)
                table$setTitle("One-Way ANOVA (Welch's)")

        },
        .initDescTable = function() {

            table <- self$results$desc

            group <- self$options$group

            if (is.null(group))
                return()

            levels <- levels(self$data[[group]])

            table$getColumn('group')$setTitle(group)

            index <- 1
            for (dep in self$options$deps) {

                for (i in seq_along(levels)) {
                    table$addRow(paste0(dep,levels[i]), list(dep=dep, group=levels[i]))

                    if (i == 1)
                        table$addFormat(rowKey=paste0(dep,levels[i]), col=1, jmvcore::Cell.BEGIN_GROUP)
                }
            }
        },
        .initDescPlot = function() {

            plots <- self$results$plots
            size <- private$.descPlotSize()

            for (dep in self$options$deps) {
                image <- plots$get(key=dep)$desc
                image$setSize(size[1], size[2])
            }
        },
        .initPostHocTables = function() {

            tables <- self$results$postHoc
            deps <- self$options$deps
            group <- self$options$group

            if (is.null(group))
                return()

            levels <- levels(self$data[[group]])

            for (i in seq_along(deps)) {

                table <- tables[[i]]

                if (self$options$phMethod == 'gamesHowell')
                    table$setTitle(paste0('Games-Howell Post-Hoc Test \u2013 ', deps[i]))
                else  if (self$options$phMethod == 'tukey')
                    table$setTitle(paste0('Tukey Post-Hoc Test \u2013 ', deps[i]))

                for (j in seq_along(levels)) {

                    level <- levels[j]

                    table$addColumn(name=paste0(level, '[md]'), title=level, type='number', visible='(phMeanDif)')
                    table$addColumn(name=paste0(level, '[t]'), title=level, type='number', visible='(phTest)')
                    table$addColumn(name=paste0(level, '[df]'), title=level, type='number', visible='(phTest)')
                    table$addColumn(name=paste0(level, '[p]'), title=level, format='zto,pvalue', type='number', visible='(phSig)')

                    values <- list()

                    for (k in seq(1, j)) {
                        l <- levels[k]
                        values[[paste0(l, '[md]')]] <- ''
                        values[[paste0(l, '[t]')]] <- ''
                        values[[paste0(l, '[df]')]] <- ''
                        values[[paste0(l, '[p]')]] <- ''
                    }

                    values[[paste0(level, '[md]')]] <- '\u2014'
                    values[[paste0(level, '[t]')]] <- '\u2014'
                    values[[paste0(level, '[df]')]] <- '\u2014'
                    values[[paste0(level, '[p]')]] <- '\u2014'

                    table$setRow(rowKey=level, values)
                }

                if (self$options$phFlag)
                    table$setNote('flag', '* p < .05, ** p < .01, *** p < .001')

            }
        },

        #### Populate tables functions ----
        .populateAnovaTable = function(results) {

            table <- self$results$anova
            for (dep in self$options$deps) {

                r <- results[[dep]]

                row <- list(
                    "F[fisher]" = as.numeric(r$fisher$statistic),
                    "F[welch]" = as.numeric(r$welch$statistic),
                    "df1[fisher]" = as.numeric(r$fisher$parameter[1]),
                    "df1[welch]" = as.numeric(r$welch$parameter[1]),
                    "df2[fisher]" = as.numeric(r$fisher$parameter[2]),
                    "df2[welch]" = as.numeric(r$welch$parameter[2]),
                    "p[fisher]" = as.numeric(r$fisher$p.value),
                    "p[welch]" = as.numeric(r$welch$p.value)
                )

                table$setRow(rowKey=dep, row)

            }
        },
        .populateDescTable = function(results) {

            table <- self$results$desc

            group <- self$options$group
            levels <- levels(self$data[[group]])

            for (dep in self$options$deps) {

                r <- results[[dep]]$desc

                for (level in levels) {

                    row <- list(
                        "num" = as.numeric(r[[level]]['n']),
                        "mean" = as.numeric(r[[level]]['mean']),
                        "sd" = as.numeric(r[[level]]['sd']),
                        "se" = as.numeric(r[[level]]['se'])
                    )

                    table$setRow(rowKey=paste0(dep,level), row)
                }
            }
        },
        .populateLevenesTable = function(results) {

            table <- self$results$assump$eqv
            for (dep in self$options$deps) {

                r <- results[[dep]]$levene

                row <- list(
                    "F" = as.numeric(r[1,'F value']),
                    "df1" = as.numeric(r[1,'Df']),
                    "df2" = as.numeric(r[2,'Df']),
                    "p" = as.numeric(r[1,'Pr(>F)'])
                )

                table$setRow(rowKey=dep, row)

            }

        },
        .populateShapiroWilkTable = function(results) {

            table <- self$results$assump$norm

            for (dep in self$options$deps) {

                r <- results[[dep]]$residuals

                row <- list()
                footnote <- NULL

                if (length(r) < 3) {

                    row[['w']] <- NaN
                    row[['p']] <- ''
                    footnote <- 'Too few samples to compute statistic (N < 3)'

                } else if (length(r) > 5000) {

                    row[['w']] <- NaN
                    row[['p']] <- ''
                    footnote <- 'Too many samples to compute statistic (N > 5000)'

                } else {

                    sw <- try(shapiro.test(r), silent=TRUE)


                    if ( ! isError(sw)) {
                        row[['w']] <- sw$statistic
                        row[['p']] <- sw$p.value
                    }
                    else {
                        row[['w']] <- NaN
                        row[['p']] <- ''
                    }
                }

                table$setRow(rowKey=dep, row)
                if ( ! is.null(footnote))
                    table$addFootnote(rowKey=dep, 'w', footnote)
            }
        },
        .populatePostHocTables = function(results) {

            tables <- self$results$postHoc
            deps <- self$options$deps
            group <- self$options$group
            levels <- levels(self$data[[group]])

            for (i in seq_along(deps)) {

                table <- tables[[i]]
                r <- results[[deps[i]]]$postHoc

                for (j in 1:(length(levels)-1)) {
                    row <- list()
                    for (k in (j+1):length(levels)) {
                        g1 <- levels[j]
                        g2 <- levels[k]

                        index <- which(r$g1 == g1 & r$g2 == g2)

                        row[paste0(g2, '[md]')] <- r[index, 'md']
                        row[paste0(g2, '[t]')] <- r[index, 't']
                        row[paste0(g2, '[df]')] <- r[index, 'df']
                        row[paste0(g2, '[p]')] <- r[index, 'p']

                        table$setRow(rowKey=g1, row)

                        if (self$options$phFlag) {
                            if (r[index, 'p'] < .001)
                                table$addSymbol(rowNo=j, paste0(g2, '[md]'), '***')
                            else if (r[index, 'p'] < .01)
                                table$addSymbol(rowNo=j, paste0(g2, '[md]'), '**')
                            else if (r[index, 'p'] < .05)
                                table$addSymbol(rowNo=j, paste0(g2, '[md]'), '*')
                        }
                    }
                }
            }
        },

        #### Plot functions ----
        .prepareDescPlot = function(results) {

            plots <- self$results$plots

            group <- self$options$group
            levels <- levels(self$data[[group]])

            for (dep in self$options$deps) {

                image <- plots$get(key=dep)$desc

                r <- results[[dep]]$desc

                df <- as.data.frame(do.call("rbind", r))
                df$levels <- factor(levels, levels=levels)

                image$setState(list(df=df, dep=dep))

            }
        },
        .desc = function(image, ggtheme, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            groupName <- self$options$group

            ciw <- 95
            errorType <- paste0('Mean (', ciw, '% CI)')

            p <- ggplot2::ggplot(data=image$state$df, ggplot2::aes(x=levels, y=mean)) +
                ggplot2::geom_errorbar(ggplot2::aes(ymin=mean-ci, ymax=mean+ci, width=.1), size=.8, color=theme$color[2]) +
                ggplot2::geom_point(ggplot2::aes(color=errorType), fill=theme$fill[1], size=3, shape=21) +
                ggplot2::labs(x=groupName, y=image$state$dep) +
                ggtheme + ggplot2::theme(legend.title = ggplot2::element_blank(),
                                         legend.justification = 0.5, legend.position = 'top')

            suppressWarnings(print(p))

            return(TRUE)
        },
        .prepareQQPlot = function(results) {

            plots <- self$results$plots
            group <- self$options$group

            for (dep in self$options$deps) {

                image <- plots$get(key=dep)$qq
                r <- results[[dep]]$residuals
                df <- as.data.frame(qqnorm(r, plot.it=FALSE))

                if (nrow(df) > 10000)
                    df <- df[ ! duplicated(round(df$x,2)), ]

                image$setState(df)

            }
        },
        .qq = function(image, ggtheme, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            p <- ggplot2::ggplot(data=image$state, ggplot2::aes(y=y, x=x)) +
                ggplot2::geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
                ggplot2::geom_point(size=2, colour=theme$color[1]) +
                ggplot2::xlab("Theoretical Quantiles") +
                ggplot2::ylab("Standardized Residuals") +
                ggtheme

            print(p)

            return(TRUE)
        },

        #### Helper functions ----
        .cleanData = function() {

            data <- self$data
            if (self$options$miss == 'listwise')
                data <- na.omit(data)

            data
        },
        .descPlotSize = function() {

            group <- self$options$group

            if (is.null(group))
                return(c(300, 350))

            levels <- levels(self$data[[group]])
            nLevels <- length(levels)

            xAxis <- 30 + 20
            yAxis <- 30 + 20

            width <- max(250, 45 * nLevels)
            height <- 300

            width <- yAxis + width
            height <- xAxis + height

            return(c(width, height))

        },
        .postHoc = function(desc, method = 'gamesHowell') {

            # Based on https://rpubs.com/aaronsc32/games-howell-test

            levels <- names(desc)
            nLevels <- length(levels)

            combs <- combn(levels, 2)
            ns <- sapply(desc, `[[`, 'n')
            means <- sapply(desc, `[[`, 'mean')
            vars <- sapply(desc, `[[`, 'sd')^2

            stats <- lapply(1:ncol(combs), function(x) {

                md <- means[combs[1,x]] - means[combs[2,x]]

                if (method == 'gamesHowell') {

                    t <- md / sqrt((vars[combs[1,x]] / ns[combs[1,x]]) + (vars[combs[2,x]] / ns[combs[2,x]]))

                    df <- (vars[combs[1,x]] / ns[combs[1,x]] + vars[combs[2,x]] / ns[combs[2,x]])^2 /
                        ((vars[combs[1,x]] / ns[combs[1,x]])^2 / (ns[combs[1,x]] - 1) +
                             (vars[combs[2,x]] / ns[combs[2,x]])^2 / (ns[combs[2,x]] - 1))

                } else {

                    df <- sum(ns) - nLevels

                    errorVar <- sum((ns - 1) * vars) / df
                    se <- sqrt(errorVar * sum(1/ns[combs[,x]]))

                    t <- md / se
                }

                p <- ptukey(abs(t) * sqrt(2), nLevels, df, lower.tail = FALSE)

                return(list(g1=combs[1,x], g2=combs[2,x], md=as.numeric(md), t=as.numeric(t), df=as.numeric(df), p=as.numeric(p)))

            })

            dat <- as.data.frame(do.call("rbind", stats))
            dat <- as.data.frame(lapply(dat, unlist))

            return(dat)
        })
)
