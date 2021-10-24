
#' @importFrom jmvcore .
logLinearClass <- R6::R6Class(
    "logLinearClass",
    inherit = logLinearBase,
    private = list(
        #### Member variables ----
        terms = NULL,
        coefTerms = list(),
        emMeans = list(),

        #### Init + run functions ----
        .init = function() {

            private$.modelTerms()

            private$.initModelFitTable()
            private$.initModelCompTable()
            private$.initModelSpec()
            private$.initLrtTables()
            private$.initCoefTables()
            private$.initEmm()
            private$.initEmmTable()

        },
        .run = function() {

            ready <- TRUE
            if (length(self$options$blocks) < 1 || length(self$options$blocks[[1]]) == 0)
                ready <- FALSE

            if (ready) {

                data <- private$.cleanData()

                results <- private$.compute(data)

                private$.populateModelFitTable(results)
                private$.populateModelCompTable(results)
                private$.populateLrtTables(results)
                private$.populateCoefTables(results)
                private$.prepareEmmPlots(results$models, data=data)
                private$.populateEmmTables()
            }
        },

        #### Compute results ----
        .compute = function(data) {

            formulas <- private$.formulas()

            globalContr <- options('contrasts')$contrasts
            options('contrasts' = c('contr.treatment', 'contr.poly'))
            on.exit(options('contrasts', substitute(globalContr)), add=TRUE)

            suppressWarnings({
                suppressMessages({

                    models <- list(); modelTest <- list(); lrTestTerms <- list(); dev <- list();
                    AIC <- list(); BIC <- list(); pseudoR <- list(); CI <- list(); CIRR <- list();

                    for (i in seq_along(formulas)) {

                        models[[i]] <- stats::glm(formulas[[i]],  data=data, family="poisson")

                        if (self$options$omni)
                            lrTestTerms[[i]] <- car::Anova(models[[i]], test="LR", type=3, singular.ok=TRUE)

                        modelTest[[i]] <- private$.modelTest(models[[i]])
                        dev[[i]] <- models[[i]]$deviance
                        AIC[[i]] <- stats::AIC(models[[i]])
                        BIC[[i]] <- stats::BIC(models[[i]])
                        pseudoR[[i]] <- private$.pseudoR2(models[[i]])

                        CI[[i]] <- try(confint.default(models[[i]], level=self$options$ciWidth/100), silent=TRUE)
                        CILO <- try(confint.default(models[[i]], level=self$options$ciWidthRR/100), silent=TRUE)
                        CIRR[[i]] <- exp(CILO)

                    }

                    lrTest <- do.call(stats::anova, c(models, test="Chisq"))

                }) # Supress messages
            }) # Supress warnings

            results <- list(models=models, modelTest=modelTest, lrTestTerms=lrTestTerms, dev=dev,
                            AIC=AIC, BIC=BIC, pseudoR=pseudoR, lrTest=lrTest, CI=CI, CIRR=CIRR)

            return(results)
        },

        #### Init tables/plots functions ----
        .initModelFitTable = function() {

            table <- self$results$modelFit

            for (i in seq_along(private$terms))
                table$addRow(rowKey=i, values=list(model = i))

        },
        .initModelCompTable = function() {

            table <- self$results$modelComp
            terms <- private$terms

            if (length(terms) <= 1) {
                table$setVisible(visible = FALSE)
                return()
            }

            for (i in 1:(length(terms)-1))
                table$addRow(rowKey=i, values=list(model1 = i, model2 = as.integer(i+1)))

        },
        .initModelSpec = function() {

            groups <- self$results$models

            for (i in seq_along(private$terms)) {
                groups$addItem(key=i)
                group <- groups$get(key=i)
                group$setTitle(paste("Model",i))
            }
        },
        .initLrtTables = function() {

            groups <- self$results$models
            termsAll <- private$terms

            for (i in seq_along(termsAll)) {

                table <- groups$get(key=i)$lrt
                terms <- termsAll[[i]]

                # if (length(terms) == 0)
                #     next()

                for (j in seq_along(terms))
                    table$addRow(rowKey=paste0(terms[[j]]), values=list(term = jmvcore::stringifyTerm(terms[j])))
            }
        },
        .initCoefTables = function() {

            groups <- self$results$models
            termsAll <- private$terms
            data <- self$data

            factors <- self$options$factors

            for (i in seq_along(termsAll)) {

                table <- groups$get(key=i)$coef

                ciWidth <- self$options$ciWidth
                ciWidthTitle <- jmvcore::format(.('{ciWidth}% Confidence Interval'), ciWidth=ciWidth)
                table$getColumn('lower')$setSuperTitle(ciWidthTitle)
                table$getColumn('upper')$setSuperTitle(ciWidthTitle)

                ciWidthRR <- self$options$ciWidthRR
                ciWidthRRTitle <- jmvcore::format(.('{ciWidth}% Confidence Interval'), ciWidth=ciWidthRR)
                table$getColumn('rateLower')$setSuperTitle(ciWidthRRTitle)
                table$getColumn('rateUpper')$setSuperTitle(ciWidthRRTitle)

                coefTerms <- list()

                table$addRow(rowKey="`(Intercept)`", values=list(term = .("Intercept")))
                coefTerms[[1]] <- "(Intercept)"

                terms <- termsAll[[i]]

                for (j in seq_along(terms)) {

                    table$addRow(rowKey=terms[[j]], values=list(term = paste0(jmvcore::stringifyTerm(terms[[j]]), ':'),
                                                                est='', se='', rate='', z='', p='',
                                                                lower='', upper='', rateLower='', rateUpper=''))

                    coefs <- private$.coefTerms(terms[[j]])
                    coefNames <- coefs$coefNames

                    for (k in seq_along(coefNames)) {
                        rowKey <- jmvcore::composeTerm(coefs$coefTerms[[k]])
                        table$addRow(rowKey=rowKey, values=list(term = coefNames[[k]]))
                        table$addFormat(rowKey=rowKey, col=1, Cell.INDENTED)
                    }

                    coefTerms <- c(coefTerms, coefs$coefTerms)
                }

                private$coefTerms[[i]] <- coefTerms
            }
        },
        .initEmm = function() {

            groups <- self$results$models
            termsAll <- private$terms
            emMeans <- self$options$emMeans

            for (i in seq_along(termsAll)) {

                group <- groups$get(key=i)$emm
                terms <- unique(unlist(termsAll[[i]]))

                for (j in seq_along(emMeans)) {

                    emm <- emMeans[[j]]

                    if ( ! is.null(emm) && all(emm %in% terms)) {
                        group$addItem(key=j)
                        emmGroup <- group$get(key=j)
                        emmGroup$setTitle(jmvcore::stringifyTerm(emm))

                        image <- emmGroup$emmPlot
                        size <- private$.plotSize(emm)
                        image$setSize(size[1], size[2])
                    }
                }
            }
        },
        .initEmmTable = function() {

            groups <- self$results$models
            termsAll <- private$terms
            emMeans <- self$options$emMeans
            factors <- self$options$factors

            emMeansTableTitle <- .('Estimated Marginal Means - {term}')
            ciWidthTitle <- jmvcore::format(.('{ciWidth}% Confidence Interval'), ciWidth=self$options$ciWidthEmm)

            for (i in seq_along(termsAll)) {

                group <- groups$get(key=i)$emm
                terms <- unique(unlist(termsAll[[i]]))

                for (j in seq_along(emMeans)) {

                    emm <- emMeans[[j]]

                    if ( ! is.null(emm) && all(emm %in% terms)) {

                        emmGroup <- group$get(key=j)

                        table <- emmGroup$emmTable
                        table$setTitle(jmvcore::format(emMeansTableTitle, term=jmvcore::stringifyTerm(emm)))

                        nLevels <- numeric(length(emm))
                        for (k in rev(seq_along(emm))) {
                                table$addColumn(name=emm[k], title=emm[k], type='text', combineBelow=TRUE)
                                nLevels[k] <- length(levels(self$data[[ emm[k] ]]))
                        }

                        table$addColumn(name='counts', title=.('Counts'), type='number')
                        table$addColumn(name='se', title=.('SE'), type='number')
                        table$addColumn(name='lower', title=.('Lower'), type='number', superTitle=ciWidthTitle, visibl="(ciEmm)")
                        table$addColumn(name='upper', title=.('Upper'), type='number', superTitle=ciWidthTitle, visibl="(ciEmm)")

                        nRows <- prod(nLevels)

                        for (k in 1:nRows) {
                            row <- list()
                            table$addRow(rowKey=k, row)
                        }
                    }
                }
            }
        },

        #### Populate tables functions ----
        .populateModelFitTable = function(results) {

            table <- self$results$modelFit

            AIC <- results$AIC
            BIC <- results$BIC
            pR2 <- results$pseudoR
            modelTest <- results$modelTest
            dev <- results$dev

            for (i in seq_along(AIC)) {

                row <- list()
                row[["r2mf"]] <- pR2[[i]]$r2mf
                row[["r2cs"]] <- pR2[[i]]$r2cs
                row[["r2n"]] <- pR2[[i]]$r2n
                row[["dev"]] <- dev[[i]]
                row[["aic"]] <- AIC[[i]]
                row[["bic"]] <- BIC[[i]]
                row[["chi"]] <- modelTest[[i]]$chi
                row[["df"]] <- modelTest[[i]]$df
                row[["p"]] <- modelTest[[i]]$p

                table$setRow(rowNo=i, values = row)
            }
        },
        .populateModelCompTable = function(results) {

            table <- self$results$modelComp

            models <- results$models
            lrTest <- results$lrTest
            r <- lrTest[-1,]

            if (length(models) <= 1)
                return()

            for (i in 1:(length(models)-1)) {

                row <- list()
                row[["chi"]] <- r$Deviance[i]
                row[["df"]] <- r$Df[i]
                row[["p"]] <- r$`Pr(>Chi)`[i]

                table$setRow(rowNo=i, values = row)
            }
        },
        .populateLrtTables = function(results) {

            if ( ! self$options$omni)
                return()

            groups <- self$results$models
            termsAll <- private$terms
            lrTests <- results$lrTestTerms

            for (i in seq_along(termsAll)) {

                table <- groups$get(key=i)$lrt

                terms <- termsAll[[i]]
                termsB64 <- lapply(terms, jmvcore::toB64)
                lrt <- lrTests[[i]]
                rowTerms <- jmvcore::decomposeTerms(rownames(lrt))

                for (j in seq_along(terms)) {

                    term <- termsB64[[j]]

                    # check which rows have the same length + same terms
                    index <- which(length(term) == sapply(rowTerms, length) &
                                       sapply(rowTerms, function(x) all(term %in% x)))

                    row <- list()
                    row[["chi"]] <- lrt[index, 'LR Chisq']
                    row[["df"]] <- lrt[index, 'Df']
                    row[["p"]] <- lrt[index, 'Pr(>Chisq)']

                    table$setRow(rowKey=paste0(terms[[j]]), values = row)
                }
            }
        },
        .populateCoefTables = function(results) {

            groups <- self$results$models
            termsAll <- private$coefTerms
            models <- results$models

            for (i in seq_along(termsAll)) {

                table <- groups$get(key=i)$coef

                model <- summary(models[[i]])

                CI <- results$CI[[i]]
                CIRR <- results$CIRR[[i]]
                coef<- model$coefficients
                terms <- termsAll[[i]]
                rowTerms <- jmvcore::decomposeTerms(rownames(coef))

                for (j in seq_along(terms)) {

                    term <- terms[[j]]

                    # check which rows have the same length + same terms
                    index <- which(length(term) == sapply(rowTerms, length) &
                                       sapply(rowTerms, function(x) all(term %in% x)))

                    row <- list()
                    row[["est"]] <- coef[index, 'Estimate']
                    row[["se"]] <- coef[index, 'Std. Error']
                    row[["rate"]] <- exp(coef[index, 'Estimate'])
                    row[["z"]] <- coef[index, 'z value']
                    row[["p"]] <- coef[index, 'Pr(>|z|)']
                    row[["lower"]] <- CI[index, 1]
                    row[["upper"]] <- CI[index, 2]
                    row[["rateLower"]] <- CIRR[index, 1]
                    row[["rateUpper"]] <- CIRR[index, 2]

                    table$setRow(rowKey=jmvcore::composeTerm(term), values = row)

                }
            }
        },
        .populateEmmTables = function() {

            groups <- self$results$models
            termsAll <- private$terms
            emMeans <- self$options$emMeans
            factors <- self$options$factors
            emmTables <- private$emMeans

            for (i in seq_along(termsAll)) {

                group <- groups$get(key=i)$emm
                terms <- unique(unlist(termsAll[[i]]))

                for (j in seq_along(emMeans)) {

                    emm <- emMeans[[j]]

                    if ( ! is.null(emm) && all(emm %in% terms)) {

                        emmGroup <- group$get(key=j)
                        table <- emmGroup$emmTable

                        emmTable <- emmTables[[i]][[j]]

                        for (k in 1:nrow(emmTable)) {
                            row <- list()
                            sign <- list()

                            for (l in seq_along(emm)) {
                                value <- emmTable[k, jmvcore::toB64(emm[l])]
                                row[[emm[l]]] <- jmvcore::fromB64(value)
                            }

                            row[['counts']] <- emmTable[k, 'rate']
                            row[['se']] <- emmTable[k, 'SE']
                            row[['lower']] <- emmTable[k, 'asymp.LCL']
                            row[['upper']] <- emmTable[k, 'asymp.UCL']

                            table$setRow(rowNo=k, values=row)
                        }
                    }
                }
            }
        },

        #### Plot functions ----
        .prepareEmmPlots = function(models, data) {

            factors <- self$options$factors

            groups <- self$results$models
            termsAll <- private$terms
            emMeans <- self$options$emMeans

            emmTables <- list()

            for (i in seq_along(termsAll)) {

                group <- groups$get(key=i)$emm
                terms <- unique(unlist(termsAll[[i]]))
                model <- models[[i]]

                emmTable <- list()

                for (j in seq_along(emMeans)) {

                    term <- emMeans[[j]]

                    if ( ! is.null(term) && all(term %in% terms)) {

                        image <- group$get(key=j)$emmPlot

                        termB64 <- jmvcore::toB64(term)
                        formula <- formula(paste('~', jmvcore::composeTerm(termB64)))

                        if (self$options$emmWeights)
                            weights <- 'equal'
                        else
                            weights <- 'cells'

                        suppressMessages({
                            emmeans::emm_options(sep = ",", parens = "a^")

                            mm <- try(
                                emmeans::emmeans(model, formula, type='response', options=list(level=self$options$ciWidthEmm / 100), weights = weights, data=data),
                                silent = TRUE
                            )
                        })

                        d <- as.data.frame(summary(mm))
                        emmTable[[ j ]] <- d

                        for (k in 1:3) {
                            if ( ! is.na(termB64[k])) {
                                d[[ termB64[k] ]] <- factor(jmvcore::fromB64(d[[ termB64[k] ]]),
                                                            jmvcore::fromB64(levels(d[[ termB64[k] ]])))
                            }
                        }

                        names <- list('x'=termB64[1], 'y'='rate', 'lines'=termB64[2], 'plots'=termB64[3], 'lower'='asymp.LCL', 'upper'='asymp.UCL')
                        names <- lapply(names, function(x) if (is.na(x)) NULL else x)

                        labels <- list('x'=term[1], 'y'=.('Counts'), 'lines'=term[2], 'plots'=term[3])
                        labels <- lapply(labels, function(x) if (is.na(x)) NULL else x)

                        image$setState(list(data=d, names=names, labels=labels))

                    }
                }

                emmTables[[i]] <- emmTable
            }

            private$emMeans <- emmTables
        },
        .emmPlot = function(image, ggtheme, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            data <- image$state$data
            names <- image$state$names
            labels <- image$state$labels

            dodge <- position_dodge(0.55)

            p <- ggplot(data=data, aes_string(x=names$x, y=names$y, color=names$lines, fill=names$lines), inherit.aes = FALSE)

            if (self$options$ciEmm) {
                p <- p + geom_col(position = dodge, width=.5, alpha = .5) +
                    geom_errorbar(aes_string(x=names$x, ymin=names$lower, ymax=names$upper), width=.1, size=.8, position=dodge)
            } else {
                p <- p + geom_col(position = dodge, width=.5)
            }

            # p <- p + geom_point(position = dodge)

            if ( ! is.null(names$plots)) {
                formula <- as.formula(paste(". ~", names$plots))
                p <- p + facet_grid(formula)
            }

            p <- p +
                labs(x=labels$x, y=labels$y, fill=labels$lines, color=labels$lines) +
                ggtheme + theme(panel.spacing = unit(2, "lines"))

            return(p)
        },

        #### Helper functions ----
        .modelTerms = function() {

            blocks <- self$options$blocks

            terms <- list()

            if (is.null(blocks)) {
                terms[[1]] <- self$options$factors
            } else {
                for (i in seq_along(blocks)) {
                    terms[[i]] <- unlist(blocks[1:i], recursive = FALSE)
                }
            }

            private$terms <- terms

        },
        .coefTerms = function(terms) {

            factors <- self$options$factors
            refLevels <- self$options$refLevels

            refVars <- sapply(refLevels, function(x) x$var)

            levels <- list()
            for (factor in factors)
                levels[[factor]] <- levels(self$data[[factor]])

            contrLevels <- list(); refLevel <- list(); contr <- list(); rContr <- list()
            for (term in terms) {
                ref <- refLevels[[which(term == refVars)]][['ref']]
                refNo <- which(ref == levels[[term]])

                contrLevels[[term]] <- levels[[term]][-refNo]
                refLevel[[term]] <- levels[[term]][refNo]

                if (length(terms) > 1)
                    contr[[term]] <- paste0('(', paste(contrLevels[[term]], refLevel[[term]], sep = ' \u2013 '), ')')
                else
                    contr[[term]] <- paste(contrLevels[[term]], refLevel[[term]], sep = ' \u2013 ')

                rContr[[term]] <- paste0(jmvcore::toB64(term), jmvcore::toB64(contrLevels[[term]]))
            }

            grid <- expand.grid(contr)
            coefNames <- apply(grid, 1, jmvcore::stringifyTerm)

            grid2 <- expand.grid(rContr)
            coefTerms <- list()
            for (i in 1:nrow(grid2))
                coefTerms[[i]] <- as.character(unlist(grid2[i,]))

            return(list(coefNames=coefNames, coefTerms=coefTerms))
        },
        .formulas = function() {

            terms <- private$terms

            formulas <- list();
            for (i in seq_along(terms)) {
                termsB64 <- lapply(terms[[i]], jmvcore::toB64)
                composedTerms <- jmvcore::composeTerms(termsB64)
                formulas[[i]] <- as.formula(paste('Freq', paste0(composedTerms, collapse ="+"), sep="~"))
            }

            return(formulas)
        },
        .cleanData = function() {

            counts <- self$options$counts
            factors <- self$options$factors
            refLevels <- self$options$refLevels

            dataRaw <- self$data

            data <- list()

            refVars <- sapply(refLevels, function(x) x$var)

            for (factor in factors) {

                ref <- refLevels[[which(factor == refVars)]][['ref']]

                rows <- jmvcore::toB64(as.character(dataRaw[[factor]]))
                levels <- jmvcore::toB64(levels(dataRaw[[factor]]))

                column <- factor(rows, levels=levels)
                column <- relevel(column, ref = jmvcore::toB64(ref))

                data[[jmvcore::toB64(factor)]] <- column
            }

            if ( ! is.null(counts)) {

                data[['Freq']] <- jmvcore::toNumeric(dataRaw[[counts]])

            } else {

                tab <- table(data)
                data <- as.data.frame(tab)

                if (length(dim(tab)) == 1)
                    colnames(data)[1] <- jmvcore::toB64(factors[1])

            }

            attr(data, 'row.names') <- seq_len(length(data[[1]]))
            attr(data, 'class') <- 'data.frame'
            data <- jmvcore::naOmit(data)

            return(data)
        },
        .plotSize = function(emm) {

            data <- self$data
            factors <- self$options$factors

            levels <- list()
            for (i in seq_along(emm)) {
                column <- data[[ emm[i] ]]
                levels[[ emm[i] ]] <- levels(column)
            }

            nLevels <- as.numeric(sapply(levels, length))
            nLevels <- ifelse(is.na(nLevels[1:3]), 1, nLevels[1:3])
            nCharLevels <- as.numeric(sapply(lapply(levels, nchar), max))
            nCharLevels <- ifelse(is.na(nCharLevels[1:3]), 0, nCharLevels[1:3])
            nCharNames <- as.numeric(nchar(names(levels)))
            nCharNames <- ifelse(is.na(nCharNames[1:3]), 0, nCharNames[1:3])

            xAxis <- 30 + 20
            yAxis <- 30 + 20

            width <- max(350, 25 * nLevels[1] * nLevels[2] * nLevels[3])
            height <- 300 + ifelse(nLevels[3] > 1, 20, 0)

            legend <- max(25 + 21 + 3.5 + 8.3 * nCharLevels[2] + 28, 25 + 10 * nCharNames[2] + 28)
            width <- yAxis + width + ifelse(nLevels[2] > 1, legend, 0)
            height <- xAxis + height

            return(c(width, height))
        },
        .pseudoR2 = function(model) {

            dev <- model$deviance
            nullDev <- model$null.deviance
            n <- length(model$fitted.values)

            r2mf <- 1 - dev/nullDev
            r2cs <- 1 - exp(-(nullDev - dev) / n)
            r2n <- r2cs / (1 - exp(-nullDev / n))

            return(list(r2mf=r2mf, r2cs=r2cs, r2n=r2n))
        },
        .modelTest = function(model) {

            chi <- model$null.deviance - model$deviance
            df <- model$df.null - model$df.residual
            p <- 1 - pchisq(chi, df)

            return(list(chi=chi, df=df, p=p))
        })
)
