#' @importFrom jmvcore .
linRegClass <- R6::R6Class(
    "linRegClass",
    inherit = linRegBase,
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
        models = function() {
            if (is.null(private$.models))
                private$.models <- private$.computeModels()

            return(private$.models)
        },
        modelsScaled = function() {
            if (is.null(private$.modelsScaled))
                private$.modelsScaled <- private$.computeModels(scaled = TRUE)

            return(private$.modelsScaled)
        },
        nModels = function() {
            if (is.null(private$.nModels))
                private$.nModels <- length(self$options$blocks)

            return(private$.nModels)
        },
        residuals = function() {
            if (is.null(private$.residuals))
                private$.residuals <- private$.computeResiduals()

            return(private$.residuals)
        },
        fitted = function() {
            if (is.null(private$.fitted))
                private$.fitted <- private$.computeFitted()

            return(private$.fitted)
        },
        predicted = function() {
            if (is.null(private$.predicted))
                private$.predicted <- private$.computePredicted()

            return(private$.predicted)
        },
        cooks = function() {
            if (is.null(private$.cooks))
                private$.cooks <- private$.computeCooks()

            return(private$.cooks)
        },
        mahalanobis = function() {
            if (is.null(private$.mahalanobis))
                private$.mahalanobis <- private$.computeMahalanobis()

            return(private$.mahalanobis)
        },
        anovaModelComparison = function() {
            if (is.null(private$.anovaModelComparison))
                private$.anovaModelComparison <- do.call(stats::anova, self$models)

            return(private$.anovaModelComparison)
        },
        anovaModelTerms = function() {
            if (is.null(private$.anovaModelTerms))
                private$.anovaModelTerms <- private$.computeAnovaModelTerms()

            return(private$.anovaModelTerms)
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
        CICoefEst = function() {
            if (is.null(private$.CICoefEst))
                private$.CICoefEst <- private$.computeCICoefEst()

            return(private$.CICoefEst)
        },
        CICoefEstScaled = function() {
            if (is.null(private$.CICoefEstScaled))
                private$.CICoefEstScaled <- private$.computeCICoefEst(scaled = TRUE)

            return(private$.CICoefEstScaled)
        },
        VIF = function() {
            if (is.null(private$.VIF))
                private$.VIF <- private$.computeVIF()

            return(private$.VIF)
        },
        durbinWatson = function() {
            if (is.null(private$.durbinWatson))
                private$.durbinWatson <- private$.computeDurbinWatson()

            return(private$.durbinWatson)
        },
        emMeans = function() {
            if (is.null(private$.emMeans))
                private$.emMeans <- private$.computeEmMeans()

            return(private$.emMeans)
        },
        refLevels = function() {
            if (is.null(private$.refLevels)) {
                refLevels <- getReferenceLevels(
                    self$data, self$options$factors, self$options$refLevels
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
        .dataRowNums = NULL,
        .weights = NULL,
        .models = NULL,
        .modelsScaled = NULL,
        .isAliased = NULL,
        .nModels = NULL,
        .residuals = NULL,
        .fitted = NULL,
        .predicted = NULL,
        .cooks = NULL,
        .mahalanobis = NULL,
        .anovaModelComparison = NULL,
        .anovaModelTerms = NULL,
        .AIC = NULL,
        .BIC = NULL,
        .CICoefEst = NULL,
        .CICoefEstScaled = NULL,
        .VIF = NULL,
        .durbinWatson = NULL,
        .modelTerms = NULL,
        .rowNamesModel = NULL,
        .emMeans = NULL,
        .emMeansForPlot = NULL,
        .refLevels = NULL,

        #### Init + run functions ----
        .init = function() {
            private$.initModelFitTable()
            private$.initModelCompTable()
            private$.initModelSpec()
            private$.initAnovaTables()
            private$.initCoefTable()
            private$.initCollinearityTable()
            private$.initResPlots()
            private$.initEmm()
            private$.initEmmTable()
            private$.initOutputs()
        },
        .run = function() {
            if (is.null(self$options$dep) || self$nModels < 1 || length(self$options$blocks[[1]]) == 0)
                return()

            private$.populateModelFitTable()
            private$.populateModelCompTable()
            private$.populateAnovaTables()
            private$.populateCoefTables()

            private$.populateCooksTable()
            private$.populateMahalanobisTable()
            private$.populateDurbinWatsonTable()
            private$.populateCollinearityTable()
            private$.populateNormality()
            private$.populateEmmTables()

            private$.prepareQQPlot()
            private$.prepareResPlots()
            private$.prepareEmmPlots()

            private$.populateOutputs()
        },

        #### Compute results ----
        .computeModels = function(modelNo = NULL, scaled = FALSE) {
            if (scaled)
                data <- private$.scaleData(self$dataProcessed)
            else
                data <- self$dataProcessed

            formulas <- private$.formulas()

            if (is.numeric(modelNo))
                formulas <- formulas[modelNo]

            models <- list()
            for (i in seq_along(formulas))
                models[[i]] <- lm(formulas[[i]], data=data, weights=self$weights)

            return(models)
        },
        .computeWeights = function() {
            legacy_weights <- self$options$weights
            global_weights <- attr(self$data, "jmv-weights")

            if (is.null(legacy_weights) && is.null(global_weights))
                return()

            if (! is.null(legacy_weights)) {
                weights <- self$dataProcessed[[jmvcore::toB64(legacy_weights)]]
            } else {
                weights <- self$dataProcessed[[".WEIGHTS"]]
            }

            if (any(weights < 0)) {
                jmvcore::reject(
                    .("'{var}' contains negative values. Negative weights are not permitted."),
                    var=legacy_weights
                )
            }

            return(weights)
        },
        .computeAnovaModelTerms = function() {
            anovaTerms <- list()
            for (i in seq_along(self$models)) {
                suppressMessages({
                    anovaTerms[[i]] <- car::Anova(
                        self$models[[i]], type=3, singular.ok=TRUE
                    )
                })
            }

            return(anovaTerms)
        },
        .computeResiduals = function() {
            res <- list()
            for (i in seq_along(self$models))
                res[[i]] <- self$models[[i]]$residuals

            return(res)
        },
        .computeFitted = function() {
            fitted <- list()
            for (i in seq_along(self$models))
                fitted[[i]] <- self$models[[i]]$fitted.values

            return(fitted)
        },
        .computePredicted = function() {
            data <- private$.cleanData(naSkip=jmvcore::toB64(self$options$dep))
            fitted <- list()
            for (i in seq_along(self$models))
                fitted[[i]] <- predict(self$models[[i]], data)

            return(fitted)
        },
        .computeCooks = function() {
            cooks <- list()
            for (i in seq_along(self$models))
                cooks[[i]] <- stats::cooks.distance(self$models[[i]])

            return(cooks)
        },
        .computeCooksSummary = function() {
            cooksSummary <- list()
            for (i in seq_along(self$cooks)) {
                cooks <- self$cooks[[i]]
                cooksSummary[[i]] <- list(
                    'mean' = mean(cooks),
                    'median' = median(cooks),
                    'sd' = sd(cooks),
                    'min' = min(cooks),
                    'max' = max(cooks)
                )
            }

            return(cooksSummary)
        },
        .computeMahalanobis = function() {
            mahalanobis <- list()
            for (i in seq_along(self$models))
                print(self$models[[i]])
                mahalanobis[[i]] <- NA

            return(mahalanobis)
        },
        .computeMahalanobisSummary = function() {
            mahalanobisSummary <- list()
            for (i in seq_along(self$mahalanobis)) {
                mahalanobis <- self$mahalanobis[[i]]
                mahalanobis <- sort(mahalanobis, decreasing = TRUE)
                # determine number of dependent variables and the resulting threshold for chi squared
                # self$options$mahalp
                mahalanobisSummary[[i]] <- list(
                    'row' = names(mahalanobis),
                    'chisq' = mahalanobis,
                    'p' = 
                )
            }

            return(mahalanobisSummary)
        },
        .computeAIC = function() {
            AIC <- list()
            for (i in seq_along(self$models))
                AIC[[i]] <- stats::AIC(self$models[[i]])

            return(AIC)
        },
        .computeBIC = function() {
            BIC <- list()
            for (i in seq_along(self$models))
                BIC[[i]] <- stats::BIC(self$models[[i]])

            return(BIC)
        },
        .computeCICoefEst = function(scaled = FALSE) {
            if (scaled)
                models <- self$modelsScaled
            else
                models <- self$models

            CICoefEst <- list()
            for (i in seq_along(models))
                CICoefEst[[i]] <- stats::confint(models[[i]], level = self$options$ciWidth / 100)

            return(CICoefEst)
        },
        .computeVIF = function() {
            modelTerms <- private$.getModelTerms()

            VIF <- list()
            for (i in seq_along(self$models)) {
                if ( ! private$.getIsAliased()[[i]] && length(modelTerms[[i]]) > 1 )
                    VIF[[i]] <- car::vif(self$models[[i]])
                else
                    VIF[[i]] <- NULL
            }

            return(VIF)
        },
        .computeDurbinWatson = function() {
            durbinWatson <- list()
            for (i in seq_along(self$models)) {
                durbinWatson[[i]] <- car::durbinWatsonTest(self$models[[i]])
            }

            return(durbinWatson)
        },
        .computeEmMeans = function(forPlot = FALSE) {
            covs <- self$options$covs
            termsAll <- private$.getModelTerms()
            emMeansTerms <- self$options$emMeans

            emMeans <- list()
            for (i in seq_along(termsAll)) {
                terms <- unique(unlist(termsAll[[i]]))
                model <- self$models[[i]]

                emmTable <- list()
                for (j in seq_along(emMeansTerms)) {
                    term <- emMeansTerms[[j]]

                    if ( ! is.null(term) && all(term %in% terms)) {
                        termB64 <- jmvcore::toB64(term)

                        FUN <- list()
                        for (k in seq_along(termB64)) {
                            if (term[k] %in% covs) {
                                if (forPlot && k == 1) {
                                    FUN[[termB64[k]]] <- function(x) pretty(x, 25)
                                } else {
                                    FUN[[termB64[k]]] <- function(x) c(mean(x)-sd(x), mean(x), mean(x)+sd(x))
                                }
                            }
                        }

                        formula <- formula(paste('~', jmvcore::composeTerm(termB64)))

                        if (self$options$emmWeights)
                            weights <- 'equal'
                        else
                            weights <- 'cells'

                        suppressMessages({
                            emmeans::emm_options(sep=",", parens="a^", cov.keep=1)

                            mm <- try(
                                emmeans::emmeans(
                                    model,
                                    formula,
                                    cov.reduce = FUN,
                                    options = list(level=self$options$ciWidthEmm / 100),
                                    weights = weights,
                                    data = self$dataProcessed,
                                    non.nuis = all.vars(formula),
                                ),
                                silent = TRUE
                            )

                            emmTable[[j]] <- try(as.data.frame(summary(mm)), silent = TRUE)
                        })
                    }
                }

                emMeans[[i]] <- emmTable
            }

            return(emMeans)
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
            blocks <- self$options$blocks

            if (length(blocks) <= 1) {
                table$setVisible(visible = FALSE)
                return()
            }

            for (i in 1:(length(blocks)-1))
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
        .initAnovaTables = function() {
            groups <- self$results$models
            termsAll <- private$.getModelTerms()

            for (i in seq_along(termsAll)) {

                table <- groups$get(key=i)$anova
                terms <- termsAll[[i]]

                for (j in seq_along(terms))
                    table$addRow(rowKey=paste0(terms[[j]]), values=list(term = jmvcore::stringifyTerm(terms[[j]])))

                table$addRow(rowKey='.RES', values=list(term = .('Residuals')))
                table$addFormat(col=1, rowKey='.RES', format=Cell.BEGIN_GROUP)

                table$setNote("ss", .("Type 3 sum of squares"))
            }
        },
        .initCoefTable = function() {
            groups <- self$results$models
            factors <- self$options$factors
            weights <- self$options$weights
            termsAll <- private$.getModelTerms()
            rowNamesModel <- private$.getRowNamesModel()

            for (i in seq_along(termsAll)) {
                table <- groups$get(key=i)$coef

                ciWidth <- self$options$ciWidth
                ciWidthTitle <- jmvcore::format(.('{ciWidth}% Confidence Interval'), ciWidth=ciWidth)
                table$getColumn('lower')$setSuperTitle(ciWidthTitle)
                table$getColumn('upper')$setSuperTitle(ciWidthTitle)

                ciWidthStdEst <- self$options$ciWidthStdEst
                ciWidthStdEstTitle <- jmvcore::format(.('{ciWidth}% Confidence Interval'), ciWidth=ciWidthStdEst)
                table$getColumn('stdEstLower')$setSuperTitle(ciWidthStdEstTitle)
                table$getColumn('stdEstUpper')$setSuperTitle(ciWidthStdEstTitle)

                coefTerms <- rowNamesModel[[i]]

                table$addRow(rowKey="`(Intercept)`", values=list(term = .("Intercept")))

                if (! is.null(weights)) {
                    private$.weightsName <- weights
                    table$setNote(
                        "weights",
                        jmvcore::format(.("Weighted by '{varName}'"), varName=weights)
                    )
                }

                if ( ! is.null(factors)) {
                    note <- ifelse(self$options$intercept == 'refLevel',
                                   .('Represents reference level'),
                                   .('Represents grand mean'))
                    table$addFootnote(rowKey="`(Intercept)`", 'term', note)
                }

                terms <- termsAll[[i]]

                iter <- 1
                for (j in seq_along(terms)) {
                    if (any(terms[[j]] %in% factors)) {
                        table$addRow(rowKey=terms[[j]],
                                     values=list(term = paste0(jmvcore::stringifyTerm(terms[[j]]), ':'),
                                                 est='', se='', t='', p='', lower='', upper='', stdEst='',
                                                 stdEstLower='', stdEstUpper=''))

                        contrastNames <- private$.contrastsCoefTable(terms[[j]])

                        for (k in seq_along(contrastNames)) {
                            iter <-iter + 1
                            rowKey <- jmvcore::composeTerm(coefTerms[[iter]])
                            table$addRow(rowKey=rowKey, values=list(term = contrastNames[[k]]))
                            table$addFormat(rowKey=rowKey, col=1, Cell.INDENTED)
                        }
                    } else {
                        iter <- iter + 1
                        rowKey <- jmvcore::composeTerm(jmvcore::toB64(terms[[j]]))
                        table$addRow(rowKey=rowKey, values=list(term = jmvcore::stringifyTerm(terms[[j]])))
                    }
                }
            }
        },
        .initCollinearityTable = function() {
            groups <- self$results$models
            termsAll <- private$.getModelTerms()

            for (i in seq_along(termsAll)) {

                table <- groups$get(key=i)$assump$collin
                terms <- termsAll[[i]]

                if (length(terms) < 1)
                    terms <- ''

                for (i in seq_along(terms))
                    table$addRow(rowKey=i, values=list(term = jmvcore::stringifyTerm(terms[i])))
            }
        },
        .initResPlots=function() {
            groups <- self$results$models
            termsAll <- private$.getModelTerms()
            covs <- self$options$covs

            for (i in seq_along(termsAll)) {
                modelTerms <- termsAll[[i]]
                if (length(modelTerms) < 1) {
                    terms <- ''
                } else {
                    terms <- c(.('Fitted'), self$options$dep)
                    for (term in modelTerms) {
                        if (length(term) == 1 && term %in% covs)
                            terms <- c(terms, term)
                    }
                }

                images <- groups$get(key=i)$assump$resPlots

                for (term in terms)
                    images$addItem(term)
            }
        },
        .initEmm = function() {
            groups <- self$results$models
            termsAll <- private$.getModelTerms()
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
            termsAll <- private$.getModelTerms()
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
                            if (emm[k] %in% factors) {
                                table$addColumn(name=emm[k], title=emm[k], type='text', combineBelow=TRUE)
                                nLevels[k] <- length(levels(self$data[[ emm[k] ]]))
                            } else {
                                table$addColumn(name=emm[k], title=emm[k], type='number', combineBelow=TRUE)
                                nLevels[k] <- 3
                            }
                        }

                        table$addColumn(name='emmean', title=.('Marginal Mean'), type='number')
                        table$addColumn(name='se', title=.('SE'), type='number')
                        table$addColumn(name='lower', title=.('Lower'), type='number', superTitle=ciWidthTitle, visible="(ciEmm)")
                        table$addColumn(name='upper', title=.('Upper'), type='number', superTitle=ciWidthTitle, visible="(ciEmm)")

                        nRows <- prod(nLevels)

                        for (k in 1:nRows) {
                            row <- list()
                            table$addRow(rowKey=k, row)
                        }
                    }
                }
            }
        },
        .initOutputs = function() {
            description = function(varType, modelNo=NULL) {
                return(
                    jmvcore::format(
                        .("{varType} of linear regression model{modelNo}"),
                        varType=varType,
                        modelNo=ifelse(is.null(modelNo), "", paste0(" ", modelNo))
                    )
                )
            }

            title = function(varType, modelNo) {
                return(jmvcore::format("{} {}", varType, modelNo))
            }

            if (self$nModels > 1) {
                keys <- seq_len(self$nModels)
                measureTypes <- rep('continuous', self$nModels)

                titles <- vapply(keys, function(key) title(.('Residuals'), key), '')
                descriptions <- vapply(keys, function(key) description(.('Residuals'), key), '')
                self$results$residsOV$set(keys, titles, descriptions, measureTypes)

                titles <- vapply(keys, function(key) title(.('Predicted values'), key), '')
                descriptions <- vapply(keys, function(key) description(.('Predicted values'), key), '')
                self$results$predictOV$set(keys, titles, descriptions, measureTypes)

                titles <- vapply(keys, function(key) title(.("Cook's distance"), key), '')
                descriptions <- vapply(keys, function(key) description(.("Cook's distance"), key), '')
                self$results$cooksOV$set(keys, titles, descriptions, measureTypes)
            }
        },

        #### Populate tables functions ----
        .populateModelFitTable = function() {
            table <- self$results$modelFit

            models <- self$models
            AIC <- self$AIC
            BIC <- self$BIC

            for (i in seq_along(models)) {
                row <- list()
                row[["aic"]] <- AIC[[i]]
                row[["bic"]] <- BIC[[i]]
                row[["r"]] <- sqrt(summary(models[[i]])$r.squared)
                row[["r2"]] <- summary(models[[i]])$r.squared
                row[["r2Adj"]] <- summary(models[[i]])$adj.r.squared
                row[["rmse"]] <- sqrt(mean(models[[i]]$residuals^2))

                F <- summary(models[[i]])$fstatistic

                if ( ! is.null(F)) {
                    row[["f"]] <- as.numeric(F[1])
                    row[["df1"]] <- as.numeric(F[2])
                    row[["df2"]] <- as.numeric(F[3])
                    row[["p"]] <- stats::pf(F[1], F[2], F[3], lower.tail=FALSE)
                } else {
                    row[["f"]] <- ""
                    row[["df1"]] <- ""
                    row[["df2"]] <- ""
                    row[["p"]] <- ""
                }

                table$setRow(rowNo=i, values = row)
            }

            table$setNote(
                "n",
                jmvcore::format(
                    "Models estimated using sample size of N={n}",
                    n=length(models[[1]]$fitted.values)
                )
            )
        },
        .populateModelCompTable = function() {
            table <- self$results$modelComp

            models <- self$models
            ANOVA <- self$anovaModelComparison
            r <- ANOVA[-1,]

            if (length(models) <= 1)
                return()

            for (i in 1:(length(models)-1)) {
                row <- list()
                row[["r2"]] <- abs(summary(models[[i]])$r.squared - summary(models[[i+1]])$r.squared)
                row[["f"]] <- (r[i,4] / r[i,3]) / (r[i,2] / r[i,1])
                row[["df1"]] <- r[i,3]
                row[["df2"]] <- r[i,1]
                row[["p"]] <- stats::pf(row[["f"]], row[["df1"]], row[["df2"]], lower.tail=FALSE)

                table$setRow(rowNo=i, values = row)
            }
        },
        .populateAnovaTables = function() {
            groups <- self$results$models
            termsAll <- private$.getModelTerms()
            anova <- self$anovaModelTerms

            for (i in seq_along(termsAll)) {

                table <- groups$get(key=i)$anova

                terms <- termsAll[[i]]
                termsB64 <- lapply(terms, jmvcore::toB64)
                r <- anova[[i]]
                rowTerms <- jmvcore::decomposeTerms(rownames(r))
                resIndex <- length(rowTerms)

                for (j in seq_along(terms)) {

                    term <- termsB64[[j]]

                    # check which rows have the same length + same terms
                    index <- which(length(term) == sapply(rowTerms, length) &
                                       sapply(rowTerms, function(x) all(term %in% x)))

                    ss <- r[index, 1]
                    df <- r[index, 2]

                    if (df == 0) {
                        ms <- NaN
                        F <- NaN
                        p <- NaN
                    } else {
                        ms <- ss / df
                        F  <- r[index, 3]
                        p  <- r[index, 4]
                    }

                    row <- list(ss=ss, df=df, ms=ms, F=F, p=p)

                    table$setRow(rowKey=paste0(terms[[j]]), values = row)
                }

                ss <- r[resIndex, 1]
                df <- r[resIndex, 2]
                ms <- ss / df

                row <- list(ss=ss, df=df, ms=ms, F='', p='')

                table$setRow(rowKey='.RES', values = row)

                if ( private$.getIsAliased()[[i]] )
                    table$setNote("alias", .(SINGULAR_WARNING))
            }
        },
        .populateCoefTables = function() {
            groups <- self$results$models
            termsAll <- private$.getRowNamesModel()
            models <- self$models
            modelsScaled <- self$modelsScaled

            for (i in seq_along(termsAll)) {
                table <- groups$get(key=i)$coef

                model <- summary(models[[i]])
                modelScaled <- summary(modelsScaled[[i]])

                CI <- self$CICoefEst[[i]]
                CIScaled <- self$CICoefEstScaled[[i]]
                coef<- model$coef
                coefScaled <- modelScaled$coef
                terms <- termsAll[[i]]
                rowTerms <- jmvcore::decomposeTerms(rownames(coef))

                for (j in seq_along(terms)) {
                    term <- terms[[j]]

                    # check which rows have the same length + same terms
                    index <- which(length(term) == sapply(rowTerms, length) &
                                       sapply(rowTerms, function(x) all(term %in% x)))

                    if (length(index) > 0) {
                        row <- list()
                        row[["est"]] <- coef[index, 1]
                        row[["se"]] <- coef[index, 2]
                        row[["t"]] <- coef[index, 3]
                        row[["p"]] <- coef[index, 4]
                        row[["lower"]] <- CI[index, 1]
                        row[["upper"]] <- CI[index, 2]

                        if (rowTerms[index] == "(Intercept)") {
                            row[["stdEst"]] <- ""
                            row[["stdEstLower"]] <- ""
                            row[["stdEstUpper"]] <- ""
                        } else {
                            row[["stdEst"]] <- coefScaled[index, 1]
                            row[["stdEstLower"]] <- CIScaled[index, 1]
                            row[["stdEstUpper"]] <- CIScaled[index, 2]
                        }
                    } else {
                        row <- list(
                            est = NaN,
                            se = NaN,
                            t = NaN,
                            p = NaN,
                            lower = NaN,
                            upper = NaN,
                            stdEst = NaN,
                            stdEstLower = NaN,
                            stdEstUpper = NaN
                        )
                    }

                    table$setRow(rowKey=jmvcore::composeTerm(term), values=row)

                    if ( private$.getIsAliased()[[i]] )
                        table$setNote("alias", .(SINGULAR_WARNING))
                }
            }
        },
        .populateCooksTable = function() {
            if (! self$options$cooks)
                return()

            groups <- self$results$models
            termsAll <- private$.getModelTerms()
            cooks <- private$.computeCooksSummary()

            for (i in seq_along(termsAll)) {
                table <- groups$get(key=i)$dataSummary$cooks
                table$setRow(rowNo=1, values=cooks[[i]])
            }
        },
        .populateMahalanobisTable = function() {
            if (! self$options$mahal)
                return()

            groups <- self$results$models
            termsAll <- private$.getModelTerms()
            cooks <- private$.computeCooksSummary()

            for (i in seq_along(termsAll)) {
                table <- groups$get(key=i)$dataSummary$mahal
#               table$setRow(rowNo=1, values=cooks[[i]])
            }
        },
        .populateDurbinWatsonTable = function() {
            if (! self$options$durbin)
                return()

            groups <- self$results$models
            termsAll <- private$.getModelTerms()

            for (i in seq_along(termsAll)) {
                table <- groups$get(key=i)$assump$durbin
                dwTest <- self$durbinWatson[[i]]

                row <- list()
                row[["autoCor"]] <- as.numeric(dwTest[1])
                row[["dw"]] <- as.numeric(dwTest[2])
                row[["p"]] <- as.numeric(dwTest[3])

                table$setRow(rowNo=1, values=row)
            }
        },
        .populateCollinearityTable = function() {
            groups <- self$results$models
            termsAll <- private$.getModelTerms()

            for (i in seq_along(termsAll)) {
                table <- groups$get(key=i)$assump$collin
                terms <- lapply(termsAll[[i]], jmvcore::toB64)

                if (length(self$VIF) == 0)
                    VIF <- NULL
                else
                    VIF <- self$VIF[[i]]

                if (length(dim(VIF)) > 1) {
                    names <- rownames(VIF)
                    VIF <- VIF[,3]
                    names(VIF) <- names
                }

                rowTerms <- jmvcore::decomposeTerms(names(VIF))

                for (j in seq_along(terms)) {
                    row <- list()

                    if (private$.getIsAliased()[[i]]) {
                        row[["tol"]] <- NaN
                        row[["vif"]] <- NaN
                    } else if (length(terms) <= 1) {
                        row[["tol"]] <- 1
                        row[["vif"]] <- 1
                    } else {
                        # check which rows have the same length + same terms
                        index <- which(length(terms[[j]]) == sapply(rowTerms, length) &
                                           sapply(rowTerms, function(x) all(terms[[j]] %in% x)))

                        row[["tol"]] <- 1 / as.numeric(VIF[index])
                        row[["vif"]] <- as.numeric(VIF[index])
                    }

                    table$setRow(rowNo=j, values=row)
                }

                if ( private$.getIsAliased()[[i]] )
                    table$setNote("alias", .(SINGULAR_WARNING))
            }
        },
        .populateEmmTables = function() {
            if (! self$options$emmTables)
                return()

            groups <- self$results$models
            termsAll <- private$.getModelTerms()
            emMeans <- self$options$emMeans
            factors <- self$options$factors
            covs <- self$options$covs

            for (i in seq_along(termsAll)) {
                group <- groups$get(key=i)$emm
                terms <- unique(unlist(termsAll[[i]]))

                for (j in seq_along(emMeans)) {
                    emm <- emMeans[[j]]

                    if ( ! is.null(emm) && all(emm %in% terms)) {
                        emmGroup <- group$get(key=j)
                        table <- emmGroup$emmTable

                        emmTable <- self$emMeans[[i]][[j]]

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

                                if (emm[l] %in% factors) {
                                    row[[emm[l]]] <- jmvcore::fromB64(value)
                                } else {
                                    row[[emm[l]]] <- value

                                    if (value == covValues[[ emm[l] ]][1])
                                        sign[[ emm[l] ]] <- '\u207B'
                                    else if (value == covValues[[ emm[l] ]][3])
                                        sign[[ emm[l] ]] <- '\u207A'
                                    else
                                        sign[[ emm[l] ]] <- '<sup>\u03BC</sup>'
                                }
                            }

                            row[['emmean']] <- emmTable[k, 'emmean']
                            row[['se']] <- emmTable[k, 'SE']
                            row[['lower']] <- emmTable[k, 'lower.CL']
                            row[['upper']] <- emmTable[k, 'upper.CL']

                            table$setRow(rowNo=k, values=row)

                            if (length(covValues) > 0) {
                                table$setNote("sub", .("\u207B mean - 1SD, <sup>\u03BC</sup> mean, \u207A mean + 1SD"))

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
        .populateNormality = function() {
            groups <- self$results$models
            termsAll <- private$.getModelTerms()

            for (i in seq_along(termsAll)) {
                residuals <- self$residuals[[i]]
                table <- groups$get(key=i)$assump$get('norm')

                res <- try(shapiro.test(residuals), silent=TRUE)
                if (jmvcore::isError(res)) {
                    values <- list(`s[sw]`=NaN, `p[sw]`='')
                } else {
                    values <- list(`s[sw]`=res$statistic, `p[sw]`=res$p.value)
                }
                table$setRow(rowNo=1, values)
            }
        },
        .populateOutputs = function() {
            if (self$options$residsOV && self$results$residsOV$isNotFilled()) {
                self$results$residsOV$setRowNums(private$.getDataRowNums())
                for (i in seq_along(self$residuals))
                    self$results$residsOV$setValues(index=i, self$residuals[[i]])
            }

            if (self$options$predictOV && self$results$predictOV$isNotFilled()) {
                self$results$predictOV$setRowNums(names(self$predicted[[1]]))
                for (i in seq_along(self$fitted))
                    self$results$predictOV$setValues(index=i, as.numeric(self$predicted[[i]]))
            }

            if (self$options$cooksOV && self$results$cooksOV$isNotFilled()) {
                self$results$cooksOV$setRowNums(private$.getDataRowNums())
                for (i in seq_along(self$cooks))
                    self$results$cooksOV$setValues(index=i, self$cooks[[i]])
            }

            if (self$options$mahalOV && self$results$mahalOV$isNotFilled()) {
                self$results$mahalOV$setRowNums(private$.getDataRowNums())
                for (i in seq_along(self$mahal))
                    self$results$mahalOV$setValues(index=i, self$mahal[[i]]$p)
            }
        },

        #### Plot functions ----
        .prepareQQPlot = function() {
            groups <- self$results$models
            termsAll <- private$.getModelTerms()

            for (i in seq_along(termsAll)) {
                image <- groups$get(key=i)$assump$get('qqPlot')
                image$setState(i)
            }
        },
        .qqPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            df <- as.data.frame(qqnorm(scale(self$residuals[[image$state]]), plot.it=FALSE))

            p <- ggplot(data=df, aes(x=x, y=y)) +
                      geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
                      geom_point(aes(x=x,y=y), size=2, colour=theme$color[1]) +
                      xlab(.("Theoretical Quantiles")) +
                      ylab(.("Standardized Residuals")) +
                      ggtheme

            return(p)
        },
        .prepareResPlots = function() {
            groups <- self$results$models
            termsAll <- private$.getModelTerms()

            for (i in seq_along(termsAll)) {
                images <- groups$get(key=i)$assump$resPlots
                for (term in images$itemKeys) {
                    image <- images$get(key=term)
                    image$setState(list(modelNo=i, term=term))
                }
            }
        },
        .resPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            res <- self$residuals[[image$state$modelNo]]

            if (image$state$term == .('Fitted')) {
                x <- self$fitted[[image$state$modelNo]]
            } else {
                x <- self$dataProcessed[[jmvcore::toB64(image$state$term)]]
            }

            df <- data.frame(y=res, x=x)

            p <- ggplot(data=df, aes(y=y, x=x)) +
                      geom_point(aes(x=x,y=y), colour=theme$color[1]) +
                      xlab(image$state$term) +
                      ylab(.("Residuals")) +
                      ggtheme

            return(p)
        },
        .prepareEmmPlots = function() {
            if (! self$options$emmPlots)
                return()

            covs <- self$options$covs
            dep <- self$options$dep

            groups <- self$results$models
            termsAll <- private$.getModelTerms()
            emMeans <- self$options$emMeans
            emMeansTables <- private$.getEmMeansForPlot()

            for (i in seq_along(termsAll)) {
                group <- groups$get(key=i)$emm
                terms <- unique(unlist(termsAll[[i]]))

                for (j in seq_along(emMeans)) {
                    term <- emMeans[[j]]

                    if ( ! is.null(term) && all(term %in% terms)) {
                        image <- group$get(key=j)$emmPlot

                        termB64 <- jmvcore::toB64(term)
                        cont <- FALSE
                        if (term[1] %in% covs)
                            cont <- TRUE

                        d <- emMeansTables[[i]][[j]]

                        for (k in 1:3) {
                            if ( ! is.na(termB64[k])) {
                                if (term[k] %in% covs) {
                                    if (k > 1) {
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
                            'x'=termB64[1], 'y'='emmean', 'lines'=termB64[2], 'plots'=termB64[3],
                            'lower'='lower.CL', 'upper'='upper.CL'
                        )
                        names <- lapply(names, function(x) if (is.na(x)) NULL else x)

                        labels <- list('x'=term[1], 'y'=dep, 'lines'=term[2], 'plots'=term[3])
                        labels <- lapply(labels, function(x) if (is.na(x)) NULL else x)

                        image$setState(list(data=d, names=names, labels=labels, cont=cont))
                    }
                }
            }
        },
        .emmPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            data <- image$state$data
            names <- image$state$names
            labels <- image$state$labels
            cont <- image$state$cont

            dodge <- position_dodge(0.4)

            if (theme$bw) {
                lty <- names$lines
                color <- NULL
            } else {
                lty <- NULL
                color <- names$lines
            }

            p <- ggplot(data=data,
                        aes_string(x=names$x, y=names$y, group=names$lines),
                        inherit.aes = FALSE)

            if (cont) {
                p <- p + geom_line(aes_string(color=color, fill=color, linetype=lty))

                if (self$options$ciEmm && is.null(names$plots) && is.null(names$lines))
                    p <- p +
                        geom_ribbon(
                            aes_string(x=names$x, ymin=names$lower, ymax=names$upper),
                            show.legend=TRUE, alpha=.3
                        )
            } else {
                p <- p +
                    geom_point(
                        aes_string(color=names$lines, fill=names$lines),
                        position = dodge
                    )

                if (self$options$ciEmm)
                    p <- p +
                        geom_errorbar(
                            aes_string(
                                x=names$x, ymin=names$lower, ymax=names$upper,
                                color=names$lines, fill=names$lines
                            ),
                            width=.1, size=.8, position=dodge
                        )
            }

            if ( ! is.null(names$plots)) {
                formula <- as.formula(paste(". ~", names$plots))
                p <- p + facet_grid(formula)
            }

            p <- p +
                labs(x=labels$x, y=labels$y, fill=labels$lines, color=labels$lines, linetype=labels$lines) +
                ggtheme + theme(panel.spacing = unit(2, "lines"))

            return(p)
        },

        #### Helper functions ----
        .getModelTerms = function() {
            if (is.null(private$.modelTerms)) {
                blocks <- self$options$blocks

                terms <- list()
                if (is.null(blocks)) {
                    terms[[1]] <- c(self$options$covs, self$options$factors)
                } else {
                    for (i in seq_along(blocks)) {
                        terms[[i]] <- unlist(blocks[1:i], recursive = FALSE)
                    }
                }
                private$.modelTerms <- terms
            }

            return(private$.modelTerms)
        },
        .getRowNamesModel = function() {
            if (is.null(private$.rowNamesModel)) {
                factors <- self$options$factors
                termsAll <- private$.getModelTerms()

                rowNamesAll <- list()
                for (i in seq_along(termsAll)) {
                    rowNames <- list()
                    rowNames[[1]] <- "(Intercept)"

                    terms <- termsAll[[i]]
                    for (term in terms) {
                        if (any(term %in% factors))
                            rowNames <- c(rowNames, private$.contrastModel(term))
                        else
                            rowNames[[length(rowNames) + 1]] <- jmvcore::toB64(term)
                    }
                    rowNamesAll[[i]] <- rowNames
                }
                private$.rowNamesModel <- rowNamesAll
            }

            return(private$.rowNamesModel)
        },
        .getEmMeansForPlot = function() {
            if (is.null(private$.emMeansForPlot))
                private$.emMeansForPlot <- private$.computeEmMeans(forPlot=TRUE)

            return(private$.emMeansForPlot)
        },
        .getDataRowNums = function() {
            if (is.null(private$.dataRowNums))
                private$.dataRowNums <- rownames(self$dataProcessed)

            return(private$.dataRowNums)
        },
        .getIsAliased = function() {
            if (is.null(private$.isAliased)) {
                aliased <- list()
                for (i in seq_along(self$models))
                    aliased[[i]] <- any(summary(self$models[[i]])$aliased)

                private$.isAliased <- aliased
            }

            return(private$.isAliased)
        },
        # Returns the names of the factor contrasts as they are
        # defined by the lm function
        .contrastModel = function(terms) {

            factors <- self$options$factors

            nLevels <- list()
            for (factor in factors)
                nLevels[[factor]] <- length(levels(self$data[[factor]]))

            contrast <- list()
            for (term in terms) {
                if (term %in% factors) {
                    contrast[[term]] <- paste0(jmvcore::toB64(term), 1:(nLevels[[term]] - 1))
                } else {
                    contrast[[term]] <- jmvcore::toB64(term)
                }
            }

            contrastGrid <- expand.grid(contrast)
            contrastTerms <- list()
            for (i in 1:nrow(contrastGrid))
                contrastTerms[[i]] <- as.character(unlist(contrastGrid[i,]))

            return(contrastTerms)
        },
        # Returns the names of the factor contrasts as they are
        # displayed in the coef table
        .contrastsCoefTable = function(terms) {

            factors <- self$options$factors
            refLevels <- self$refLevels
            refVars <- sapply(refLevels, function(x) x$var)

            levels <- list()
            for (factor in factors)
                levels[[factor]] <- levels(self$data[[factor]])

            contrast <- list()
            for (term in terms) {
                if (term %in% factors) {
                    ref <- refLevels[[which(term == refVars)]][['ref']]
                    refNo <- which(ref == levels[[term]])

                    contrLevels <- levels[[term]][-refNo]
                    refLevel <- levels[[term]][refNo]

                    c <- paste(contrLevels, refLevel, sep = ' \u2013 ')
                    if (length(terms) > 1)
                        c <- paste0('(', c, ')')

                    contrast[[term]] <- c
                } else {
                    contrast[[term]] <- term
                }
            }
            contrastGrid <- expand.grid(contrast)
            contrastNames <- apply(contrastGrid, 1, jmvcore::stringifyTerm)

            return(contrastNames)
        },
        .formulas = function() {
            dep <- self$options$dep
            depB64 <- jmvcore::toB64(dep)
            terms <- private$.getModelTerms()

            formulas <- list();
            for (i in seq_along(terms)) {
                termsB64 <- lapply(terms[[i]], jmvcore::toB64)
                composedTerms <- jmvcore::composeTerms(termsB64)
                formulas[[i]] <- as.formula(paste(depB64, paste0(composedTerms, collapse ="+"), sep="~"))
            }

            return(formulas)
        },
        .cleanData = function(naOmit=TRUE, naSkip=NULL) {
            dep <- self$options$dep
            covs <- self$options$covs
            factors <- self$options$factors
            weights <- self$options$weights
            refLevels <- self$refLevels

            dataRaw <- self$data

            data <- list()

            refVars <- sapply(refLevels, function(x) x$var)

            for (factor in factors) {
                if (length(levels(dataRaw[[factor]])) <= 1)
                    stop(jmvcore::format(.("Factor '{factor}' needs to have at least 2 levels"), factor=factor))

                ref <- refLevels[[which(factor == refVars)]][['ref']]
                column <- factor(
                    dataRaw[[factor]],
                    ordered = FALSE,
                    levels = levels(dataRaw[[factor]])
                )
                levels(column) <- jmvcore::toB64(levels(column))
                column <- relevel(column, ref = jmvcore::toB64(ref))

                data[[jmvcore::toB64(factor)]] <- column
                stats::contrasts(data[[jmvcore::toB64(factor)]]) <- private$.createContrasts(levels(column))
            }

            for (cov in c(dep, covs, weights))
                data[[jmvcore::toB64(cov)]] <- jmvcore::toNumeric(dataRaw[[cov]])

            global_weights <- attr(dataRaw, "jmv-weights")
            if (is.null(weights) && ! is.null(global_weights))
                data[[".WEIGHTS"]] = jmvcore::toNumeric(global_weights)

            attr(data, 'row.names') <- rownames(self$data)
            attr(data, 'class') <- 'data.frame'

            if (naOmit) {
                data <- tibble::rownames_to_column(data)
                data <- tidyr::drop_na(data, -tidyselect::all_of(naSkip))
                data <- tibble::column_to_rownames(data)
            }

            return(data)
        },
        .plotSize = function(emm) {
            data <- self$data
            factors <- self$options$factors

            levels <- list()
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
            nLevels <- ifelse(is.na(nLevels[1:3]), 1, nLevels[1:3])
            nCharLevels <- as.numeric(sapply(lapply(levels, nchar), max))
            nCharLevels <- ifelse(is.na(nCharLevels[1:3]), 0, nCharLevels[1:3])
            nCharNames <- as.numeric(nchar(names(levels)))
            nCharNames <- ifelse(is.na(nCharNames[1:3]), 0, nCharNames[1:3])

            xAxis <- 30 + 20
            yAxis <- 30 + 20

            if (emm[1] %in% factors) {
                width <- max(350, 25 * nLevels[1] * nLevels[2] * nLevels[3])
                height <- 300 + ifelse(nLevels[3] > 1, 20, 0)
            } else {
                width <- max(350, 300 * nLevels[3])
                height <- 300 + ifelse(nLevels[3] > 1, 20, 0)
            }

            legend <- max(25 + 21 + 3.5 + 8.3 * nCharLevels[2] + 28, 25 + 10 * nCharNames[2] + 28)
            width <- yAxis + width + ifelse(nLevels[2] > 1, legend, 0)
            height <- xAxis + height

            return(c(width, height))
        },
        .createContrasts=function(levels) {
            if (self$options$intercept == 'refLevel') {
                contrast <- contr.treatment(levels)
                dimnames(contrast) <- NULL
            } else {
                nLevels <- length(levels)
                dummy <- contr.treatment(levels)
                dimnames(dummy) <- NULL
                coding <- matrix(rep(1/nLevels, prod(dim(dummy))), ncol=nLevels-1)
                contrast <- (dummy - coding)
            }

            return(contrast)
        },
        .scaleData = function(data) {
            for (col in names(data)) {
                if ( ! is.factor(data[[col]]) && length(unique(data[[col]])) > 1 )
                    data[[col]] <- scale(data[[col]])
            }

            return(data)
        })
)
