
propTest2Class <- R6::R6Class(
    "propTest2Class",
    inherit = propTest2Base,
    private = list(
        #### Init + run functions ----
        .init = function() {

            private$.initTable()
            private$.initPostPlots()

        },
        .run = function() {

            private$.errorCheck()
            results <- private$.compute()

            if (length(self$options$vars) > 0) {

                private$.populateTable(results)

                if (self$options$postPlots)
                    private$.preparePostPlots(results)

            }
        },

        #### Compute results ----
        .compute = function() {

            varList <- list()

            hyp <- self$options$hypothesis
            if (hyp == 'notequal')
                hyp <- 'two.sided'

            for (var in self$options$vars) {

                levelList <- list()

                results <- private$.counts(var)

                counts <- results$counts
                total  <- results$total
                levels <- results$levels

                for (i in seq_along(counts)) {

                    level <- levels[i]
                    count <- counts[i]
                    prop <- count / total

                    r <- try(binom.test(x=count, n=total, p=self$options$testValue,
                                        alternative=hyp, conf.level=self$options$ciWidth/100))

                    rBF <- private$.bf(count, total, prop=self$options$testValue,
                                       hyp=hyp, self$options$ciWidth/100,
                                       a=self$options$priorA, b=self$options$priorB)

                    if ( ! base::inherits(r, 'try-error')) {
                        p <- r$p.value
                        cil <- r$conf.int[1]
                        ciu <- r$conf.int[2]
                    } else {
                        p <- 'NaN'
                        cil <- ''
                        ciu <- ''
                    }

                    levelList[[i]] <- list(var=var, level=level, count=count, total=total,
                                           prop=prop, p=p, cil=cil, ciu=ciu, bf=rBF$bf,
                                           cilBayes=rBF$ci[1], ciuBayes=rBF$ci[2])
                }

                varList[[var]] <- levelList
            }

            return(varList)
        },

        #### Init tables/plots functions ----
        .initTable = function() {

            table <- self$results$get('table')

            hyp <- self$options$hypothesis
            if (hyp == 'greater')
                op <- '>'
            else if (hyp == 'less')
                op <- '<'
            else
                op <- '\u2260'

            table$setNote('hyp', jmvcore::format('H\u2090 is proportion {} {}', op, self$options$testValue))
            table$getColumn('cil')$setSuperTitle(paste0(self$options$ciWidth, '% Confidence Interval'))
            table$getColumn('ciu')$setSuperTitle(paste0(self$options$ciWidth, '% Confidence Interval'))
            table$getColumn('cilBayes')$setSuperTitle(paste0(self$options$ciBayesWidth, '% Credible Interval'))
            table$getColumn('ciuBayes')$setSuperTitle(paste0(self$options$ciBayesWidth, '% Credible Interval'))

            for (var in self$options$vars) {

                varData <- jmvcore::naOmit(self$data[[var]])

                if (self$options$areCounts) {

                    levels <- paste(1:2)

                } else {

                    levels <- base::levels(varData)
                    if (length(levels) == 0)
                        levels <- c('\u2026', '\u2026 ')
                }

                for (level in levels) {
                    key <- paste0(var, '`', level)
                    table$addRow(rowKey=key, values=list(var=var, level=level))
                }

                table$addFormat(rowKey=paste0(var, '`', levels[1]), 'var', jmvcore::Cell.BEGIN_GROUP)
                table$addFormat(rowKey=paste0(var, '`', levels[length(levels)]), 'var', jmvcore::Cell.END_GROUP)
            }
        },
        .initPostPlots = function() {

            plots <- self$results$postPlots

            data <- self$data
            vars <- self$options$vars

            for (var in vars) {

                group <- plots$get(var)
                column <- data[[var]]

                if (self$options$areCounts) {

                    levels <- paste(1:2)

                } else {

                    if ( ! is.factor(column))
                        levels <- levels(as.factor(column))
                    else
                        levels <- levels(column)
                }

                for (level in levels)
                    group$addItem(level)
            }
        },

        #### Populate tables functions ----
        .populateTable = function(results) {

            table <- self$results$get('table')
            table$deleteRows()

            for (var in self$options$vars) {

                varResults <- results[[var]]

                levels <- character(length(varResults))
                for (i in seq_along(levels))
                    levels[i] <- varResults[[i]]$level

                for (i in seq_along(levels)) {

                    level <- levels[i]
                    key <- paste0(var, '`', level)
                    table$addRow(rowKey=key, values=varResults[[i]])

                    if ( varResults[[i]][['bf']] < 1)
                        table$addFormat(col='bf', rowKey=key, Cell.NEGATIVE)
                }

                table$addFormat(rowKey=paste0(var, '`', levels[1]), 'var', jmvcore::Cell.BEGIN_GROUP)
                table$addFormat(rowKey=paste0(var, '`', levels[length(levels)]), 'var', jmvcore::Cell.END_GROUP)
            }
        },

        #### Plot functions ----
        .preparePostPlots = function(results) {

            plots <- self$results$postPlots

            vars <- self$options$vars
            hyp <- self$options$hypothesis
            prop <- self$options$testValue
            a <- self$options$priorA
            b <- self$options$priorB

            for (var in vars) {

                group <- plots$get(var)
                group$clear()

                varResults <- results[[var]]

                levels <- character(length(varResults))
                for (i in seq_along(levels))
                    levels[i] <- varResults[[i]]$level

                for (i in seq_along(levels)) {

                    group$addItem(levels[i])
                    image <- group$get(levels[i])

                    k <- varResults[[i]]$count
                    n <- varResults[[i]]$total

                    x <- rep(seq(.001, .999, .001), 2)

                    if (hyp == "notequal") {

                        prior <- dbeta(x, a, b)
                        post <- dbeta(x, a + k, b + n - k)
                        like <- dbeta(x, 1 + k, 1 + n - k)

                    } else if (hyp == "greater") {

                        prior <- ifelse(x >= prop, dbeta(x, a, b) / pbeta(prop, a, b, lower.tail=FALSE), 0)
                        post <- ifelse(x >= prop, dbeta(x, a + k, b + n - k) / pbeta(prop, a + k, b + n - k, lower.tail=FALSE), 0)
                        like <- ifelse(x >= prop, dbeta(x, 1 + k, 1 + n - k) / pbeta(prop, 1 + k, 1 + n - k, lower.tail=FALSE), 0)

                    } else {

                        prior <- ifelse(x <= prop, dbeta(x, a, b) / pbeta(prop, a, b, lower.tail=TRUE), 0)
                        post <- ifelse(x <= prop, dbeta(x, a + k, b + n - k) / pbeta(prop, a + k, b + n - k, lower.tail=TRUE), 0)
                        like <- ifelse(x <= prop, dbeta(x, 1 + k, 1 + n - k) / pbeta(prop, 1 + k, 1 + n - k, lower.tail=TRUE), 0)
                    }

                    y <- c(prior, like, post)
                    g <- factor(rep(c("Prior", "Likelihood", "Posterior"), each=length(prior)),
                                levels=c("Prior", "Likelihood", "Posterior"))

                    df <- data.frame(x=x, y=y, group=g)

                    image$setState(df)

                }
            }
        },
        .postPlot = function(image, ggtheme, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            prop <- self$options$testValue

            themeSpec <- theme(axis.text.y=element_blank(),
                               axis.ticks.y=element_blank(),
                               legend.title = element_blank(),
                               legend.position="top")

            p <- ggplot(data=image$state, aes(x=x, y=y, group=group, color=group, linetype=group)) +
                geom_line(size=0.7) +
                xlim(0, 1) + labs(x="Proportion", y="Density") +
                scale_color_manual(values=c("#3E6DA9", "#6b9de8", theme$color[1])) +
                scale_linetype_manual(values=c("dotted", "dashed", "solid")) +
                # geom_vline(xintercept = prop, size=0.1, linetype="dashed") +
                ggtheme + themeSpec


            suppressWarnings(
                suppressMessages(
                    print(p)
                )
            )

            TRUE
        },

        #### Helper functions ----
        .errorCheck = function() {

            vars <- self$options$vars
            data <- self$data

            for (var in vars) {
                column <- naOmit(data[[var]])
                if (length(column) == 0)
                    jmvcore::reject(jmvcore::format('Variable \'{}\' contains no data', var), code='')
            }
        },
        .counts = function(var) {

            initing <- nrow(self$data) == 0
            varData <- jmvcore::naOmit(self$data[[var]])

            if (self$options$areCounts) {

                levels <- paste(1:length(varData))

                if (jmvcore::canBeNumeric(varData))
                    counts <- jmvcore::toNumeric(varData)
                else
                    counts <- suppressWarnings(as.numeric(as.character(varData)))

            } else {

                levels <- base::levels(varData)
                if (length(levels) == 0)
                    levels <- paste(sort(unique(varData)))

                counts <- as.vector(table(varData))

            }

            total <- base::sum(counts, na.rm=TRUE)

            list(levels=levels, counts=counts, total=total)
        },
        .bf = function(k, n, prop, hyp='two.sided', ciWidth=0.95, a = 1, b = 1) {

            lower <- (1 - ciWidth) / 2
            upper <- 1 - lower

            if (hyp == 'two.sided') {

                bf <- dbeta(prop, a, b) / dbeta(prop, a + k, b + n - k)
                ci <- qbeta(c(lower, upper), a + k, b + n - k)

            } else if (hyp == 'greater') {

                heightPrior <- dbeta(prop, a, b) / pbeta(prop, a, b, lower.tail=FALSE)
                heightPosterior <- dbeta(prop, a + k, b + n - k) / pbeta(prop, a + k, b + n - k, lower.tail=FALSE)
                bf <- heightPrior / heightPosterior

                right <- pbeta(prop, a + k , b + n - k, lower.tail = FALSE)
                left <- 1 - right
                ci <- qbeta(left + right * c(lower, upper), a + k , b + n - k)

            } else {

                heightPrior <- dbeta(prop, a, b) / pbeta(prop, a, b, lower.tail=TRUE)
                heightPosterior <- dbeta(prop, a + k, b + n - k) / pbeta(prop, a + k, b + n - k, lower.tail=TRUE)
                bf <- heightPrior / heightPosterior

                left <- pbeta(prop, a + k , b + n - k)
                ci <- qbeta(left * c(lower, upper), a + k , b + n - k)
            }

            return(list(bf=bf, ci=ci))
        })
)
