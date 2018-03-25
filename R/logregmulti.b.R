
logRegMultiClass <- R6::R6Class(
    "logRegMultiClass",
    inherit = logRegMultiBase,
    private = list(
        #### Member variables ----
        terms = NULL,
        coefTerms = list(),
        emMeans = list(),
        compLevels = NULL,

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
            if (is.null(self$options$dep) || length(self$options$blocks) < 1 || length(self$options$blocks[[1]]) == 0)
                ready <- FALSE

            if (ready) {

                data <- private$.cleanData()
                private$.errorCheck(data)

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

            formulas <- private$.formula()

            globalContr <- options('contrasts')$contrasts
            options('contrasts' = c('contr.treatment', 'contr.poly'))
            on.exit(options('contrasts', substitute(globalContr)), add=TRUE)

            suppressWarnings({
                suppressMessages({

                    models <- list(); modelTest <- list(); lrTestTerms <- list(); dev <- list();
                    AIC <- list(); BIC <- list(); pseudoR <- list(); CI <- list(); CIOR <- list()

                    nullFormula <- as.formula(paste0(jmvcore::toB64(self$options$dep), '~ 1'))
                    nullModel <- nnet::multinom(nullFormula, data=data, model = TRUE, trace=FALSE)
                    null <- list(dev=nullModel$deviance, df=nullModel$edf)

                    for (i in seq_along(formulas)) {

                        models[[i]] <- nnet::multinom(formulas[[i]], data=data, model=TRUE, trace=FALSE)
                        models[[i]]$call$formula <- formulas[[i]]

                        lrTestTerms[[i]] <- car::Anova(models[[i]], test="LR", type=3, singular.ok=TRUE)
                        modelTest[[i]] <- private$.modelTest(models[[i]], null)
                        dev[[i]] <- models[[i]]$deviance
                        AIC[[i]] <- stats::AIC(models[[i]])
                        BIC[[i]] <- stats::BIC(models[[i]])
                        pseudoR[[i]] <- private$.pseudoR2(models[[i]], null)

                        CI[[i]] <- try(confint(models[[i]], level=self$options$ciWidth/100), silent=TRUE)
                        if (class(CI[[i]]) == 'try-error')
                            CI[[i]] <- confint.default(models[[i]], level=self$options$ciWidth/100)

                        CILO <- try(confint(models[[i]], level=self$options$ciWidthOR/100), silent=TRUE)
                        if (class(CILO[[i]]) == 'try-error')
                            CILO <- confint.default(models[[i]], level=self$options$ciWidthOR/100)

                        CIOR[[i]] <- exp(CILO)
                    }

                    if (length(formulas) > 1)
                        lrTest <- do.call(stats::anova, c(models, test="Chisq"))
                    else
                        lrTest <- NULL

                }) # Supress messages
            }) # Supress warnings

            results <- list(models=models, modelTest=modelTest, lrTestTerms=lrTestTerms, dev=dev,
                            AIC=AIC, BIC=BIC, pseudoR=pseudoR, lrTest=lrTest, CI=CI, CIOR=CIOR)

            return(results)
        },

        #### Init tables/plots functions ----
        .initModelFitTable = function() {

            table <- self$results$modelFit

            for (i in seq_along(self$options$blocks))
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

            for (i in seq_along(self$options$blocks)) {
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

                for (j in seq_along(terms))
                    table$addRow(rowKey=paste0(terms[[j]]), values=list(term = jmvcore::stringifyTerm(terms[j])))
            }
        },
        .initCoefTables = function() {

            groups <- self$results$models
            termsAll <- private$terms
            data <- self$data

            factors <- self$options$factors

            dep <- self$options$dep

            if ( ! is.null(dep) ) {
                refLevels <- self$options$refLevels
                refVars <- sapply(refLevels, function(x) x$var)
                depLevels <- levels(self$data[[dep]])
                depRefLevel <- refLevels[[which(dep == refVars)]][['ref']]
                depCompLevels <- depLevels[-which(depRefLevel == depLevels)]
                private$compLevels <- depCompLevels
            } else {
                depCompLevels <- NULL
            }

            for (i in seq_along(termsAll)) {

                table <- groups$get(key=i)$coef

                table$getColumn('dep')$setTitle(dep)

                ciWidth <- self$options$ciWidth
                table$getColumn('lower')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
                table$getColumn('upper')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))

                ciWidthOR <- self$options$ciWidthOR
                table$getColumn('oddsLower')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidthOR))
                table$getColumn('oddsUpper')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidthOR))

                terms <- termsAll[[i]]

                coefTerms <- list()

                for (j in seq_along(depCompLevels)) {

                    comparison <- paste(depCompLevels[j], "-", depRefLevel)

                    rowKey <- paste0(j, jmvcore::composeTerm("(Intercept)"))
                    table$addRow(rowKey=rowKey, values=list(dep=comparison, term = "Intercept"))
                    table$addFormat(rowKey=rowKey, col=1, Cell.BEGIN_GROUP)

                    if (j == 1)
                        coefTerms[[1]] <- "(Intercept)"

                    for (k in seq_along(terms)) {

                        if (any(terms[[k]] %in% factors)) { # check if there are factors in the term

                            table$addRow(rowKey=paste0(j, terms[[k]]),
                                         values=list(dep=comparison, term = paste0(jmvcore::stringifyTerm(terms[[k]]), ':'),
                                                     est='', se='', odds='', z='', p='',
                                                     lower='', upper='', oddsLower='', oddsUpper=''))

                            coefs <- private$.coefTerms(terms[[k]])
                            coefNames <- coefs$coefNames

                            for (l in seq_along(coefNames)) {
                                rowKey <- paste0(j,jmvcore::composeTerm(coefs$coefTerms[[l]]))
                                table$addRow(rowKey=rowKey, values=list(dep=comparison, term = coefNames[[l]]))
                                table$addFormat(rowKey=rowKey, col=2, Cell.INDENTED)
                            }

                            if (j == 1)
                                coefTerms <- c(coefTerms, coefs$coefTerms)

                        } else {

                            rowKey <- paste0(j,jmvcore::composeTerm(jmvcore::toB64(terms[[k]])))
                            table$addRow(rowKey=rowKey, values=list(dep=comparison, term = jmvcore::stringifyTerm(terms[[k]])))

                            if (j == 1)
                                coefTerms[[length(coefTerms) + 1]] <- jmvcore::toB64(terms[[k]])

                        }
                    }
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
            dep <- self$options$dep

            for (i in seq_along(termsAll)) {

                group <- groups$get(key=i)$emm
                terms <- unique(unlist(termsAll[[i]]))

                for (j in seq_along(emMeans)) {

                    emm <- emMeans[[j]]

                    if ( ! is.null(emm) && all(emm %in% terms)) {

                        emmGroup <- group$get(key=j)

                        table <- emmGroup$emmTable
                        table$setTitle(paste0('Estimated Marginal Means - ', jmvcore::stringifyTerm(emm)))

                        emm <- c(dep, emm)
                        nLevels <- numeric(length(emm))
                        for (k in rev(seq_along(emm))) {
                            if (emm[k] %in% c(dep, factors)) {
                                table$addColumn(name=emm[k], title=emm[k], type='text', combineBelow=TRUE)
                                nLevels[k] <- length(levels(self$data[[ emm[k] ]]))
                            } else {
                                table$addColumn(name=emm[k], title=emm[k], type='number', combineBelow=TRUE)
                                nLevels[k] <- 3
                            }
                        }

                        table$addColumn(name='prob', title='Probability', type='number')
                        table$addColumn(name='se', title='SE', type='number')
                        table$addColumn(name='lower', title='Lower', type='number', superTitle=paste0(self$options$ciWidthEmm, '% Confidence Interval'), visibl="(ciEmm)")
                        table$addColumn(name='upper', title='Upper', type='number', superTitle=paste0(self$options$ciWidthEmm, '% Confidence Interval'), visibl="(ciEmm)")

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
                row[["chi"]] <- r[['LR stat.']][i]
                row[["df"]] <- r[['   Df']][i]
                row[["p"]] <- r[['Pr(Chi)']][i]

                table$setRow(rowNo=i, values = row)
            }
        },
        .populateLrtTables = function(results) {

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

            compLevels <- jmvcore::toB64(private$compLevels)

            for (i in seq_along(termsAll)) {

                table <- groups$get(key=i)$coef

                model <- summary(models[[i]], Wald.ratios = TRUE)
                CI <- results$CI[[i]]
                CIOR <- results$CIOR[[i]]
                coef<- model$coefficients
                se <- model$standard.errors
                wald <- model$Wald.ratios
                p <- (1 - pnorm(abs(wald), 0, 1)) * 2

                terms <- termsAll[[i]]
                rowTerms <- jmvcore::decomposeTerms(rownames(coef))
                colTerms <- jmvcore::decomposeTerms(colnames(coef))

                for (j in seq_along(compLevels)) {

                    for (k in seq_along(terms)) {

                        term <- terms[[k]]

                        index1 <- which(rowTerms == compLevels[j])
                        index2 <- which(length(term) == sapply(colTerms, length) &
                                            sapply(colTerms, function(x) all(term %in% x)))

                        row <- list()
                        row[["est"]] <- coef[index1, index2]
                        row[["se"]] <- se[index1, index2]
                        row[["odds"]] <- exp(coef[index1, index2])
                        row[["z"]] <- wald[index1, index2]
                        row[["p"]] <- p[index1, index2]
                        row[["lower"]] <- CI[index2, 1, index1]
                        row[["upper"]] <- CI[index2, 2, index1]
                        row[["oddsLower"]] <- CIOR[index2, 1, index1]
                        row[["oddsUpper"]] <- CIOR[index2, 2, index1]

                        table$setRow(rowKey=paste0(j, jmvcore::composeTerm(terms[[k]])), values = row)
                    }
                }
            }
        },
        .populateEmmTables = function() {

            groups <- self$results$models
            termsAll <- private$terms
            emMeans <- self$options$emMeans
            factors <- self$options$factors
            covs <- self$options$covs
            dep <- self$options$dep
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

                        emm <- c(emm, dep)

                        covValues <- list()
                        for (k in seq_along(emm)) {
                            if (emm[k] %in% covs)
                                covValues[[ emm[k] ]] <- sort(unique(emmTable[, jmvcore::toB64(emm[k])]))
                        }

                        for (k in 1:nrow(emmTable)) {
                            row <- list()
                            sign <- list()

                            for (l in seq_along(emm)) {

                                value <- emmTable[k, jmvcore::toB64(emm[l])]

                                if (emm[l] %in% covs) {
                                    row[[emm[l]]] <- value

                                    if (value == covValues[[ emm[l] ]][1])
                                        sign[[ emm[l] ]] <- '\u207B'
                                    else if (value == covValues[[ emm[l] ]][3])
                                        sign[[ emm[l] ]] <- '\u207A'
                                    else
                                        sign[[ emm[l] ]] <- '<sup>\u03BC</sup>'
                                } else {
                                    row[[emm[l]]] <- jmvcore::fromB64(value)
                                }
                            }

                            row[['prob']] <- emmTable[k, 'prob']
                            row[['se']] <- emmTable[k, 'SE']
                            row[['lower']] <- emmTable[k, 'lower.CL']
                            row[['upper']] <- emmTable[k, 'upper.CL']

                            table$setRow(rowNo=k, values=row)

                            if (length(covValues) > 0) {

                                table$setNote("sub", "\u207B mean - 1SD, <sup>\u03BC</sup> mean, \u207A mean + 1SD")

                                for (l in seq_along(emm)) {
                                    if (emm[l] %in% covs)
                                        table$addSymbol(rowNo=k, emm[l], sign[[ emm[l] ]])
                                }
                            }
                        }
                    }
                }
            }
        },

        #### Plot functions ----
        .prepareEmmPlots = function(models, data) {

            covs <- self$options$covs
            factors <- self$options$factors
            dep <- self$options$dep

            refLevels <- self$options$refLevels

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

                        term <- c(dep, term)
                        termB64 <- jmvcore::toB64(term)

                        FUN <- list(TRUE); FUN2 <- list(TRUE)
                        cont <- FALSE

                        for(k in seq_along(termB64)) {

                            if (term[k] %in% covs) {
                                if (k == 2) {
                                    FUN[[termB64[k]]] <- function(x)  pretty(x, 25)
                                    cont <- TRUE
                                } else {
                                    FUN[[termB64[k]]] <- function(x)  c(mean(x)-sd(x), mean(x), mean(x)+sd(x))
                                }

                                FUN2[[termB64[[k]]]] <- function(x)  c(mean(x)-sd(x), mean(x), mean(x)+sd(x))
                            }
                        }

                        formula <- formula(paste('~', jmvcore::composeTerm(termB64)))

                        if (self$options$emmWeights)
                            weights <- 'equal'
                        else
                            weights <- 'cell'

                        suppressMessages({
                            mm <- try(
                                emmeans::emmeans(model, formula, cov.reduce=FUN, type='response', options=list(level=self$options$ciWidthEmm / 100), weights = weights, data=data),
                                silent = TRUE
                            )

                            emmTable[[ j ]] <- try(
                                as.data.frame(summary(emmeans::emmeans(model, formula, cov.reduce=FUN2, type='response', options=list(level=self$options$ciWidthEmm / 100), weights = weights, data=data))),
                                silent = TRUE
                            )
                        })

                        # if (class(mm) == 'try-error')
                        #     jmvcore::reject('No variable named rank in the reference grid')

                        d <- as.data.frame(summary(mm))

                        for (k in 1:4) {
                            if ( ! is.na(termB64[k])) {
                                if (term[k] %in% covs) {
                                    if (k > 2) {
                                        d[[ termB64[k] ]] <- factor(d[[ termB64[k] ]])
                                        levels(d[[ termB64[k] ]]) <- c('-1SD', 'Mean', '+1SD')
                                    }
                                } else {
                                    d[[ termB64[k] ]] <- factor(jmvcore::fromB64(d[[ termB64[k] ]]),
                                                                jmvcore::fromB64(levels(d[[ termB64[k] ]])))
                                }
                            }
                        }

                        names <- list('x'=termB64[2], 'y'='prob', 'lines'=termB64[1], 'xPlots'=termB64[3], 'yPlots'=termB64[4], 'lower'='lower.CL', 'upper'='upper.CL')
                        names <- lapply(names, function(x) if (is.na(x)) NULL else x)

                        labels <- list('x'=term[2], 'y'='Probability', 'lines'=term[1], 'xPlots'=term[3], 'yPlots'=term[4])
                        labels <- lapply(labels, function(x) if (is.na(x)) NULL else x)

                        image$setState(list(data=d, names=names, labels=labels, cont=cont))

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
            cont <- image$state$cont

            dodge <- position_dodge(0.4)

            p <- ggplot(data=data, aes_string(x=names$x, y=names$y, color=names$lines, fill=names$lines), inherit.aes = FALSE)

            if (cont) {

                p <- p + geom_line()

                if (self$options$ciEmm && is.null(names$plots) && is.null(names$lines))
                    p <- p + geom_ribbon(aes_string(x=names$x, ymin=names$lower, ymax=names$upper), show.legend=TRUE, alpha=.3)

            } else {

                p <- p + geom_point(position = dodge)

                if (self$options$ciEmm)
                    p <- p + geom_errorbar(aes_string(x=names$x, ymin=names$lower, ymax=names$upper), width=.1, size=.8, position=dodge)
            }

            if ( ! is.null(names$xPlots)) {

                if (! is.null(names$yPlots)) {
                    formula <- as.formula(paste(names$yPlots, "~", names$xPlots))
                } else {
                    formula <- as.formula(paste(". ~", names$xPlots))
                }

                p <- p + facet_grid(formula)
            }

            p <- p + ylim(0,1) +
                labs(list(x=labels$x, y=labels$y, fill=labels$lines, color=labels$lines)) +
                ggtheme + theme(panel.spacing = unit(2, "lines"))

            print(p)

            TRUE
        },

        #### Helper functions ----
        .modelTerms = function() {

            blocks <- self$options$blocks

            terms <- list()

            if (is.null(blocks)) {
                terms[[1]] <- c(self$options$covs, self$options$factors)
            } else {
                for (i in seq_along(blocks)) {
                    terms[[i]] <- unlist(blocks[1:i], recursive = FALSE)
                    terms[[i]][sapply(terms[[i]], is.null)] <- NULL
                }
            }

            private$terms <- terms

        },
        .coefTerms = function(terms) {

            covs <- self$options$covs
            factors <- self$options$factors
            refLevels <- self$options$refLevels

            refVars <- sapply(refLevels, function(x) x$var)

            levels <- list()
            for (factor in factors)
                levels[[factor]] <- levels(self$data[[factor]])

            contrLevels <- list(); refLevel <- list(); contr <- list(); rContr <- list()
            for (term in terms) {

                if (term %in% factors) {

                    ref <- refLevels[[which(term == refVars)]][['ref']]
                    refNo <- which(ref == levels[[term]])

                    contrLevels[[term]] <- levels[[term]][-refNo]
                    refLevel[[term]] <- levels[[term]][refNo]

                    if (length(terms) > 1)
                        contr[[term]] <- paste0('(', paste(contrLevels[[term]], refLevel[[term]], sep = ' \u2013 '), ')')
                    else
                        contr[[term]] <- paste(contrLevels[[term]], refLevel[[term]], sep = ' \u2013 ')

                    rContr[[term]] <- paste0(jmvcore::toB64(term), jmvcore::toB64(contrLevels[[term]]))

                    # If custom contrast is used:
                    # rContr[[term]] <- paste0(jmvcore::toB64(term), 1:length(contrLevels[[term]]))

                } else {

                    contr[[term]] <- term
                    rContr[[term]] <- jmvcore::toB64(term)

                }
            }

            grid <- expand.grid(contr)
            coefNames <- apply(grid, 1, jmvcore::stringifyTerm)

            grid2 <- expand.grid(rContr)
            coefTerms <- list()
            for (i in 1:nrow(grid2))
                coefTerms[[i]] <- as.character(unlist(grid2[i,]))

            return(list(coefNames=coefNames, coefTerms=coefTerms))
        },
        .formula = function() {

            dep <- self$options$dep
            depB64 <- jmvcore::toB64(dep)
            terms <- private$terms

            formulas <- list();
            for (i in seq_along(terms)) {
                termsB64 <- lapply(terms[[i]], jmvcore::toB64)
                composedTerms <- jmvcore::composeTerms(termsB64)
                formulas[[i]] <- as.formula(paste(depB64, paste0(composedTerms, collapse ="+"), sep="~"))
            }

            return(formulas)
        },
        .errorCheck = function(data) {

            dep <- self$options$dep
            column <- data[[jmvcore::toB64(dep)]]

            if (length(levels(column)) == 2)
                jmvcore::reject(jmvcore::format('The dependent variable \'{}\' has only two levels, consider doing a binomial logistic regression.', dep), code='')
        },
        .cleanData = function() {

            dep <- self$options$dep
            covs <- self$options$covs
            factors <- self$options$factors
            refLevels <- self$options$refLevels

            dataRaw <- self$data

            data <- list()

            refVars <- sapply(refLevels, function(x) x$var)

            for (factor in c(dep, factors)) {

                ref <- refLevels[[which(factor == refVars)]][['ref']]

                rows <- jmvcore::toB64(as.character(dataRaw[[factor]]))
                levels <- jmvcore::toB64(levels(dataRaw[[factor]]))

                column <- factor(rows, levels=levels)
                column <- relevel(column, ref = jmvcore::toB64(ref))

                data[[jmvcore::toB64(factor)]] <- column
                # stats::contrasts(data[[jmvcore::toB64(factor)]]) <- private$.createContrasts(levels)
            }

            for (cov in covs)
                data[[jmvcore::toB64(cov)]] <- jmvcore::toNumeric(dataRaw[[cov]])

            attr(data, 'row.names') <- seq_len(length(data[[1]]))
            attr(data, 'class') <- 'data.frame'
            data <- jmvcore::naOmit(data)

            return(data)
        },
        .plotSize = function(emm) {

            data <- self$data
            covs <- self$options$covs
            factors <- self$options$factors
            dep <- self$options$dep

            levels <- list()
            levels[[ dep ]] <- levels(data[[ dep ]])
            for (i in seq_along(emm)) {

                column <- data[[ emm[i] ]]

                if (emm[i] %in% factors) {
                    levels[[ emm[i] ]] <- levels(column)
                } else {
                    if (i == 1)
                        levels[[ emm[i] ]] <- ''
                    else
                        levels[[ emm[i] ]] <- c('-1SD', 'Mean', '+1SD')
                }
            }

            nLevels <- as.numeric(sapply(levels, length))
            nLevels <- ifelse(is.na(nLevels[1:4]), 1, nLevels[1:4])
            nCharLevels <- as.numeric(sapply(lapply(levels, nchar), max))
            nCharLevels <- ifelse(is.na(nCharLevels[1:4]), 0, nCharLevels[1:4])
            nCharNames <- as.numeric(nchar(names(levels)))
            nCharNames <- ifelse(is.na(nCharNames[1:4]), 0, nCharNames[1:4])

            xAxis <- 30 + 20
            yAxis <- 30 + 20

            if (emm[1] %in% factors) {

                width <- max(350, 25 * nLevels[1] * nLevels[2] * nLevels[3]) + ifelse(nLevels[4] > 1, 20, 0)
                height <- 300 * nLevels[4] + ifelse(nLevels[3] > 1, 20, 0)

            } else {

                width <- max(350, 300 * nLevels[3]) + ifelse(nLevels[3] > 1, 20, 0)
                height <- 300 *  nLevels[4] + ifelse(nLevels[4] > 1, 20, 0)

            }

            legend <- max(25 + 21 + 3.5 + 8.3 * nCharLevels[2] + 28, 25 + 10 * nCharNames[2] + 28)
            width <- yAxis + width + legend
            height <- xAxis + height

            return(c(width, height))
        },
        .createContrasts=function(levels) {

            nLevels <- length(levels)

            dummy <- contr.treatment(levels)
            dimnames(dummy) <- NULL
            coding <- matrix(rep(1/nLevels, prod(dim(dummy))), ncol=nLevels-1)
            contrast <- (dummy - coding)

            return(contrast)
        },
        .pseudoR2 = function(model, null) {

            dev <- model$deviance
            n <- length(model$fitted.values)

            r2mf <- 1 - dev/null$dev
            r2cs <- 1 - exp(-(null$dev - dev) / n)
            r2n <- r2cs / (1 - exp(-null$dev / n))

            return(list(r2mf=r2mf, r2cs=r2cs, r2n=r2n))
        },
        .modelTest = function(model, null) {

            chi <- null$dev - model$deviance
            df <- abs(null$df - model$edf)
            p <- 1 - pchisq(chi, df)

            return(list(chi=chi, df=df, p=p))
        })
)
