
#' @importFrom jmvcore .
logRegMultiClass <- R6::R6Class(
    "logRegMultiClass",
    inherit = logRegMultiBase,
    #### Active bindings ----
    active = list(
        dataProcessed = function() {
            if (is.null(private$.dataProcessed))
                private$.dataProcessed <- private$.cleanData()

            return(private$.dataProcessed)
        },
        weights = function() {
            if (is.null(private$.weights))
                private$.weights <- private$.computeWeights()

            return(private$.weights)
        },
        formulas = function() {
            if (is.null(private$.formulas))
                private$.formulas <- private$.getFormulas()

            return(private$.formulas)
        },
        models = function() {
            if (is.null(private$.models))
                private$.models <- private$.computeModels()

            return(private$.models)
        },
        nModels = function() {
            if (is.null(private$.nModels))
                private$.nModels <- length(self$options$blocks)

            return(private$.nModels)
        },
        nullModel = function() {
            if (is.null(private$.nullModel))
                private$.nullModel <- private$.computeNullModel()

            return(private$.nullModel)
        },
        lrtModelComparison = function() {
            if (is.null(private$.lrtModelComparison) && self$nModels > 1) {
                private$.lrtModelComparison <- do.call(
                    stats::anova,
                    c(self$models, test="Chisq")
                )
            }

            return(private$.lrtModelComparison)
        },
        lrtModelTerms = function() {
            if (is.null(private$.lrtModelTerms))
                private$.lrtModelTerms <- private$.computeLrtModelTerms()

            return(private$.lrtModelTerms)
        },
        deviance = function() {
            if (is.null(private$.deviance))
                private$.deviance <- private$.computeDeviance()

            return(private$.deviance)
        },
        AIC = function() {
            if (is.null(private$.AIC))
                private$.AIC <- private$.computeAIC()

            return(private$.AIC)
        },
        BIC = function() {
            if (is.null(private$.BIC))
                private$.BIC <- private$.computeBIC()

            return(private$.BIC)
        },
        pseudoR2 = function() {
            if (is.null(private$.pseudoR2))
                private$.pseudoR2 <- private$.computePseudoR2()

            return(private$.pseudoR2)
        },
        modelTest = function() {
            if (is.null(private$.modelTest))
                private$.modelTest <- private$.computeModelTest()

            return(private$.modelTest)
        },
        CICoefEst = function() {
            if (is.null(private$.CICoefEst))
                private$.CICoefEst <- private$.computeCICoefEst()

            return(private$.CICoefEst)
        },
        CICoefEstOR = function() {
            if (is.null(private$.CICoefEstOR))
                private$.CICoefEstOR <- private$.computeCICoefEst(type="OR")

            return(private$.CICoefEstOR)
        },
        refLevels = function() {
            if (is.null(private$.refLevels)) {
                factors <- c(self$options$dep, self$options$factors)
                refLevels <- getReferenceLevels(
                    self$data, factors, self$options$refLevels
                )
                private$.refLevels <- refLevels$refLevels

                if (length(refLevels$changedVars) > 0)
                    setRefLevelWarning(self, refLevels$changedVars)
            }

            return(private$.refLevels)
        }
    ),
    private = list(
        #### Member variables ----
        .dataProcessed = NULL,
        .weights = NULL,
        .models = NULL,
        .nModels = NULL,
        .nullModel = NULL,
        .formulas = NULL,
        .lrtModelComparison = NULL,
        .lrtModelTerms = NULL,
        .deviance = NULL,
        .AIC = NULL,
        .BIC = NULL,
        .pseudoR2 = NULL,
        .modelTest = NULL,
        .CICoefEst = NULL,
        .CICoefEstOR = NULL,
        .refLevels = NULL,
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
            if (
                is.null(self$options$dep) ||
                length(self$options$blocks) < 1 ||
                length(self$options$blocks[[1]]) == 0
            ) {
                return()
            }

            private$.errorCheck()

            private$.populateModelFitTable()
            private$.populateModelCompTable()
            private$.populateLrtTables()
            private$.populateCoefTables()

            private$.prepareEmmPlots()
            private$.populateEmmTables()
        },

        #### Compute results ----
        .computeModels = function(modelNo = NULL) {
            data <- self$dataProcessed
            formulas <- self$formulas

            if (is.numeric(modelNo))
                formulas <- formulas[modelNo]

            globalContr <- options('contrasts')$contrasts
            options('contrasts' = c('contr.treatment', 'contr.poly'))
            on.exit(options('contrasts', substitute(globalContr)), add=TRUE)

            models <- list()
            for (i in seq_along(formulas)) {
                models[[i]] <- nnet::multinom(
                    formulas[[i]], data=data, model=TRUE, trace=FALSE, weights=self$weights
                )
                models[[i]]$call$formula <- formulas[[i]]
            }

            return(models)
        },
        .computeNullModel = function() {
            nullFormula <- as.formula(paste0(jmvcore::toB64(self$options$dep), '~ 1'))
            nullModel <- nnet::multinom(
                nullFormula, data=self$dataProcessed, model=TRUE, trace=FALSE, weights=self$weights
            )
            return(list(dev=nullModel$deviance, df=nullModel$edf))
        },
        .computeWeights = function() {
            global_weights <- attr(self$data, "jmv-weights")

            if (is.null(global_weights))
                return()

            weights <- self$dataProcessed[[".WEIGHTS"]]

            if (any(weights < 0)) {
                jmvcore::reject(
                    .("Weights contains negative values. Negative weights are not permitted.")
                )
            }

            return(weights)
        },
        .computeLrtModelTerms = function() {
            lrtModelTerms <- list()
            for (i in seq_len(self$nModels)) {
                lrtModelTerms[[i]] <- car::Anova(
                    self$models[[i]],
                    test="LR",
                    type=3,
                    singular.ok=TRUE
                )
            }
            return(lrtModelTerms)
        },
        .computeDeviance = function() {
            dev <- list()
            for (i in seq_len(self$nModels))
                dev[[i]] <- self$models[[i]]$deviance

            return(dev)
        },
        .computeAIC = function() {
            AIC <- list()
            for (i in seq_len(self$nModels))
                AIC[[i]] <- stats::AIC(self$models[[i]])

            return(AIC)
        },
        .computeBIC = function() {
            BIC <- list()
            for (i in seq_len(self$nModels))
                BIC[[i]] <- stats::BIC(self$models[[i]])

            return(BIC)
        },
        .computePseudoR2 = function() {
            pR2 <- list()
            for (i in seq_len(self$nModels)) {
                dev <- self$deviance[[i]]
                n <- length(self$models[[i]]$fitted.values)

                r2mf <- 1 - dev / self$nullModel$dev
                r2cs <- 1 - exp(-(self$nullModel$dev - dev) / n)
                r2n <- r2cs / (1 - exp(-self$nullModel$dev / n))

                pR2[[i]] <- list(r2mf=r2mf, r2cs=r2cs, r2n=r2n)
            }
            return(pR2)
        },
        .computeModelTest = function() {
            modelTest <- list()
            for (i in seq_len(self$nModels)) {
                chi <- self$nullModel$dev - self$models[[i]]$deviance
                df <- abs(self$nullModel$df - self$models[[i]]$edf)
                p <- 1 - pchisq(chi, df)
                modelTest[[i]] <- list(chi=chi, df=df, p=p)
            }
            return(modelTest)
        },
        .computeCICoefEst = function(type="LOR") {
            if (type == "OR")
                level <- self$options$ciWidthOR / 100
            else
                level <- self$options$ciWidth / 100

            ci <- list()
            for (i in seq_len(self$nModels)) {
                ci[[i]] <- confint(self$models[[i]], level=level)
                if (type == "OR")
                    ci[[i]] <- exp(ci[[i]])
            }
            return(ci)
        },

        #### Init tables/plots functions ----
        .initModelFitTable = function() {
            table <- self$results$modelFit

            for (i in seq_along(self$options$blocks))
                table$addRow(rowKey=i, values=list(model = i))

            table$setNote(
                "n",
                jmvcore::format(
                    .("Models estimated using sample size of N={n}"), n="..."
                )
            )
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

                for (j in seq_along(terms)) {
                    table$addRow(
                        rowKey=paste0(terms[[j]]),
                        values=list(term = jmvcore::stringifyTerm(terms[j]))
                    )
                }
            }
        },
        .initCoefTables = function() {
            groups <- self$results$models
            termsAll <- private$terms
            data <- self$data

            factors <- self$options$factors

            dep <- self$options$dep

            if ( ! is.null(dep) ) {
                refLevels <- self$refLevels
                refVars <- sapply(refLevels, function(x) x$var)
                depLevels <- levels(self$data[[dep]])
                depRefLevel <- refLevels[[which(dep == refVars)]][['ref']]
                depCompLevels <- depLevels[-which(depRefLevel == depLevels)]
                private$compLevels <- depCompLevels
            } else {
                depCompLevels <- NULL
            }

            ciWidthTitleString <- .('{ciWidth}% Confidence Interval')
            ciWidthTitle <- jmvcore::format(ciWidthTitleString, ciWidth=self$options$ciWidth)
            ciWidthORTitle <- jmvcore::format(ciWidthTitleString, ciWidth=self$options$ciWidthOR)

            for (i in seq_along(termsAll)) {
                table <- groups$get(key=i)$coef

                table$getColumn('dep')$setTitle(dep)
                table$getColumn('lower')$setSuperTitle(ciWidthTitle)
                table$getColumn('upper')$setSuperTitle(ciWidthTitle)
                table$getColumn('oddsLower')$setSuperTitle(ciWidthORTitle)
                table$getColumn('oddsUpper')$setSuperTitle(ciWidthORTitle)

                terms <- termsAll[[i]]

                coefTerms <- list()

                for (j in seq_along(depCompLevels)) {
                    comparison <- paste(depCompLevels[j], "-", depRefLevel)

                    rowKey <- paste0(j, jmvcore::composeTerm("(Intercept)"))
                    table$addRow(rowKey=rowKey, values=list(dep=comparison, term = .("Intercept")))
                    table$addFormat(rowKey=rowKey, col=1, Cell.BEGIN_GROUP)

                    if (j == 1)
                        coefTerms[[1]] <- "(Intercept)"

                    for (k in seq_along(terms)) {

                        if (any(terms[[k]] %in% factors)) { # check if there are factors in the term
                            table$addRow(
                                rowKey=paste0(j, terms[[k]]),
                                values=list(
                                    dep=comparison,
                                    term=paste0(jmvcore::stringifyTerm(terms[[k]]), ':'),
                                    est='',
                                    se='',
                                    odds='',
                                    z='',
                                    p='',
                                    lower='',
                                    upper='',
                                    oddsLower='',
                                    oddsUpper=''
                                )
                            )

                            coefs <- private$.coefTerms(terms[[k]])
                            coefNames <- coefs$coefNames

                            for (l in seq_along(coefNames)) {
                                rowKey <- paste0(j,jmvcore::composeTerm(coefs$coefTerms[[l]]))
                                table$addRow(
                                    rowKey=rowKey,
                                    values=list(dep=comparison, term = coefNames[[l]])
                                )
                                table$addFormat(rowKey=rowKey, col=2, Cell.INDENTED)
                            }

                            if (j == 1)
                                coefTerms <- c(coefTerms, coefs$coefTerms)
                        } else {
                            rowKey <- paste0(j,jmvcore::composeTerm(jmvcore::toB64(terms[[k]])))
                            table$addRow(
                                rowKey=rowKey,
                                values=list(dep=comparison, term=jmvcore::stringifyTerm(terms[[k]]))
                            )

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

            emMeansTableTitle <- .('Estimated Marginal Means - {term}')
            ciWidthTitle <- jmvcore::format(
                .('{ciWidth}% Confidence Interval'), ciWidth=self$options$ciWidthEmm
            )

            for (i in seq_along(termsAll)) {

                group <- groups$get(key=i)$emm
                terms <- unique(unlist(termsAll[[i]]))

                for (j in seq_along(emMeans)) {

                    emm <- emMeans[[j]]

                    if ( ! is.null(emm) && all(emm %in% terms)) {

                        emmGroup <- group$get(key=j)

                        table <- emmGroup$emmTable
                        table$setTitle(
                            jmvcore::format(emMeansTableTitle, term=jmvcore::stringifyTerm(emm))
                        )

                        emm <- c(dep, emm)
                        nLevels <- numeric(length(emm))
                        for (k in rev(seq_along(emm))) {
                            if (emm[k] %in% c(dep, factors)) {
                                table$addColumn(
                                    name=emm[k], title=emm[k], type='text', combineBelow=TRUE
                                )
                                nLevels[k] <- length(levels(self$data[[ emm[k] ]]))
                            } else {
                                table$addColumn(
                                    name=emm[k], title=emm[k], type='number', combineBelow=TRUE
                                )
                                nLevels[k] <- 3
                            }
                        }

                        table$addColumn(name='prob', title=.('Probability'), type='number')
                        table$addColumn(name='se', title=.('SE'), type='number')
                        table$addColumn(
                            name='lower', title=.('Lower'), type='number', superTitle=ciWidthTitle,
                            visibl="(ciEmm)"
                        )
                        table$addColumn(
                            name='upper', title=.('Upper'), type='number', superTitle=ciWidthTitle,
                            visibl="(ciEmm)"
                        )

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
        .populateModelFitTable = function() {
            table <- self$results$modelFit
            for (i in seq_len(self$nModels)) {
                row <- list()
                row[["r2mf"]] <- self$pseudoR2[[i]]$r2mf
                row[["r2cs"]] <- self$pseudoR2[[i]]$r2cs
                row[["r2n"]] <- self$pseudoR2[[i]]$r2n
                row[["dev"]] <- self$deviance[[i]]
                row[["aic"]] <- self$AIC[[i]]
                row[["bic"]] <- self$BIC[[i]]
                row[["chi"]] <- self$modelTest[[i]]$chi
                row[["df"]] <- self$modelTest[[i]]$df
                row[["p"]] <- self$modelTest[[i]]$p

                table$setRow(rowNo=i, values = row)
            }

            table$setNote(
                "n",
                jmvcore::format(
                    "Models estimated using sample size of N={n}",
                    n=nrow(model.frame(self$models[[1]]))
                )
            )
        },
        .populateModelCompTable = function() {
            if (self$nModels <= 1)
                return()

            table <- self$results$modelComp
            r <- self$lrtModelComparison[-1,]

            for (i in seq_len(self$nModels - 1)) {
                row <- list()
                row[["chi"]] <- r[['LR stat.']][i]
                row[["df"]] <- r[['   Df']][i]
                row[["p"]] <- r[['Pr(Chi)']][i]

                table$setRow(rowNo=i, values=row)
            }
        },
        .populateLrtTables = function() {
            groups <- self$results$models
            termsAll <- private$terms

            for (i in seq_along(termsAll)) {
                table <- groups$get(key=i)$lrt

                terms <- termsAll[[i]]
                termsB64 <- lapply(terms, jmvcore::toB64)
                lrt <- self$lrtModelTerms[[i]]
                rowTerms <- jmvcore::decomposeTerms(rownames(lrt))

                for (j in seq_along(terms)) {
                    term <- termsB64[[j]]

                    # check which rows have the same length + same terms
                    index <- which(
                        length(term) == sapply(rowTerms, length) &
                            sapply(rowTerms, function(x) all(term %in% x))
                    )

                    row <- list()
                    row[["chi"]] <- lrt[index, 'LR Chisq']
                    row[["df"]] <- lrt[index, 'Df']
                    row[["p"]] <- lrt[index, 'Pr(>Chisq)']

                    table$setRow(rowKey=paste0(terms[[j]]), values = row)
                }
            }
        },
        .populateCoefTables = function() {
            groups <- self$results$models
            termsAll <- private$coefTerms

            compLevels <- jmvcore::toB64(private$compLevels)

            for (i in seq_along(termsAll)) {

                table <- groups$get(key=i)$coef

                model <- summary(self$models[[i]], Wald.ratios = TRUE)
                CI <- self$CICoefEst[[i]]
                CIOR <- self$CICoefEstOR[[i]]
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
                        index2 <- which(
                            length(term) == sapply(colTerms, length) &
                                sapply(colTerms, function(x) all(term %in% x))
                        )

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

                        table$setRow(
                            rowKey=paste0(j, jmvcore::composeTerm(terms[[k]])),
                            values = row
                        )
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
                            if (emm[k] %in% covs) {
                                covValues[[ emm[k] ]] <- sort(
                                    unique(emmTable[, jmvcore::toB64(emm[k])])
                                )
                            }
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

                                table$setNote(
                                    "sub",
                                    .("\u207B mean - 1SD, <sup>\u03BC</sup> mean, \u207A mean + 1SD")
                                )

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
        .prepareEmmPlots = function() {
            covs <- self$options$covs
            factors <- self$options$factors
            dep <- self$options$dep

            refLevels <- self$refLevels

            groups <- self$results$models
            termsAll <- private$terms
            emMeans <- self$options$emMeans
            data <- self$dataProcessed

            emmTables <- list()

            for (i in seq_along(termsAll)) {

                group <- groups$get(key=i)$emm
                terms <- unique(unlist(termsAll[[i]]))
                model <- self$models[[i]]

                emmTable <- list()

                for (j in seq_along(emMeans)) {

                    term <- emMeans[[j]]

                    if ( ! is.null(term) && all(term %in% terms)) {

                        image <- group$get(key=j)$emmPlot

                        term <- c(dep, term)
                        termB64 <- jmvcore::toB64(term)

                        FUN <- list(); FUN2 <- list()
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
                            weights <- 'cells'

                        suppressMessages({
                            emmeans::emm_options(sep = ",", parens = "a^")

                            mm <- try(
                                emmeans::emmeans(
                                    model, formula, cov.reduce=FUN, type='response',
                                    options=list(level=self$options$ciWidthEmm / 100),
                                    weights=weights, data=data
                                ),
                                silent = TRUE
                            )

                            emmTable[[ j ]] <- try(
                                as.data.frame(
                                    summary(
                                        emmeans::emmeans(
                                            model, formula, cov.reduce=FUN2, type='response',
                                            options=list(level=self$options$ciWidthEmm / 100),
                                            weights = weights, data=data
                                        )
                                    )
                                ),
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
                                    d[[ termB64[k] ]] <- factor(
                                        jmvcore::fromB64(d[[ termB64[k] ]]),
                                        jmvcore::fromB64(levels(d[[ termB64[k] ]]))
                                    )
                                }
                            }
                        }

                        names <- list(
                            'x'=termB64[2], 'y'='prob', 'lines'=termB64[1], 'xPlots'=termB64[3],
                            'yPlots'=termB64[4], 'lower'='lower.CL', 'upper'='upper.CL'
                        )
                        names <- lapply(names, function(x) if (is.na(x)) NULL else x)

                        labels <- list(
                            'x'=term[2], 'y'=.('Probability'), 'lines'=term[1], 'xPlots'=term[3],
                            'yPlots'=term[4]
                        )
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

            p <- ggplot(
                data=data,
                aes_string(x=names$x, y=names$y, color=names$lines, fill=names$lines),
                inherit.aes = FALSE
            )

            if (cont) {
                p <- p + geom_line()

                if (self$options$ciEmm && is.null(names$plots) && is.null(names$lines)) {
                    p <- p + geom_ribbon(
                        aes_string(x=names$x, ymin=names$lower, ymax=names$upper),
                        show.legend=TRUE,
                        alpha=.3
                    )
                }
            } else {
                p <- p + geom_point(position = dodge)

                if (self$options$ciEmm) {
                    p <- p + geom_errorbar(
                        aes_string(x=names$x, ymin=names$lower, ymax=names$upper),
                        width=.1, size=.8, position=dodge
                    )
                }
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
                labs(x=labels$x, y=labels$y, fill=labels$lines, color=labels$lines) +
                ggtheme + theme(panel.spacing = unit(2, "lines"))

            return(p)
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
                }
            }

            private$terms <- terms
        },
        .coefTerms = function(terms) {
            covs <- self$options$covs
            factors <- self$options$factors
            refLevels <- self$refLevels

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

                    if (length(terms) > 1) {
                        contr[[term]] <- paste0(
                            '(', paste(contrLevels[[term]], refLevel[[term]], sep=' \u2013 '), ')'
                        )
                    } else {
                        contr[[term]] <- paste(
                            contrLevels[[term]], refLevel[[term]], sep = ' \u2013 '
                        )
                    }

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
        .getFormulas = function() {
            dep <- self$options$dep
            depB64 <- jmvcore::toB64(dep)
            terms <- private$terms

            formulas <- list();
            for (i in seq_along(terms)) {
                termsB64 <- lapply(terms[[i]], jmvcore::toB64)
                composedTerms <- jmvcore::composeTerms(termsB64)
                formulas[[i]] <- as.formula(
                    paste(depB64, paste0(composedTerms, collapse ="+"), sep="~")
                )
            }

            return(formulas)
        },
        .errorCheck = function() {
            dep <- self$options$dep
            column <- self$dataProcessed[[jmvcore::toB64(dep)]]

            if (length(levels(column)) == 2) {
                jmvcore::reject(
                    jmvcore::format(
                        .('The dependent variable "{dep}" has only two levels, consider doing a binomial logistic regression.'),
                        dep=dep
                    ),
                    code=''
                )
            }
        },
        .cleanData = function() {
            dep <- self$options$dep
            covs <- self$options$covs
            factors <- self$options$factors
            refLevels <- self$refLevels

            dataRaw <- self$data

            data <- list()

            refVars <- sapply(refLevels, function(x) x$var)

            for (factor in c(dep, factors)) {
                ref <- refLevels[[which(factor == refVars)]][['ref']]
                column <- factor(
                    dataRaw[[factor]],
                    ordered = FALSE,
                    levels = levels(dataRaw[[factor]])
                )
                levels(column) <- jmvcore::toB64(levels(column))
                column <- relevel(column, ref = jmvcore::toB64(ref))

                data[[jmvcore::toB64(factor)]] <- column
            }

            for (cov in covs)
                data[[jmvcore::toB64(cov)]] <- jmvcore::toNumeric(dataRaw[[cov]])

            global_weights <- attr(dataRaw, "jmv-weights")
            if (! is.null(global_weights))
                data[[".WEIGHTS"]] <- jmvcore::toNumeric(global_weights)

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

                width <- max(350, 25 * nLevels[1] * nLevels[2] * nLevels[3]) +
                    ifelse(nLevels[4] > 1, 20, 0)
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
        })
)
