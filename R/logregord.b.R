
#' @importFrom jmvcore .
logRegOrdClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "logRegOrdClass",
    inherit = logRegOrdBase,
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
        thresTerms = list(),
        emMeans = list(),

        #### Init + run functions ----
        .init = function() {
            private$.modelTerms()

            private$.initModelFitTable()
            private$.initModelCompTable()
            private$.initModelSpec()
            private$.initLrtTables()
            private$.initCoefTables()
            private$.initThresTables()

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
            private$.populateThresTables()
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
                models[[i]] <- MASS::polr(
                    formulas[[i]], data=data, model=TRUE, Hess=TRUE, weights=self$weights
                )
                models[[i]]$call$formula <- formulas[[i]]
            }

            return(models)
        },
        .computeNullModel = function() {
            nullFormula <- as.formula(paste0(jmvcore::toB64(self$options$dep), '~ 1'))
            nullModel <-  MASS::polr(
                nullFormula, data=self$dataProcessed, model=TRUE, Hess=TRUE, weights=self$weights
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

            dep <- self$options$dep

            if ( ! is.null(dep) ) {
                depLevels <- levels(self$data[[dep]])
            } else {
                return()
            }

            table$setNote(
                "note",
                jmvcore::format(
                    .("The dependent variable '{dep}' has the following order: {orderedLevels}"),
                    dep=dep,
                    orderedLevels=paste(depLevels, collapse = ' | ')
                )
            )
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
                depLevels <- levels(self$data[[dep]])
            } else {
                depLevels <- NULL
            }

            ciWidthTitleString <- .('{ciWidth}% Confidence Interval')
            ciWidthTitle <- jmvcore::format(ciWidthTitleString, ciWidth=self$options$ciWidth)
            ciWidthORTitle <- jmvcore::format(ciWidthTitleString, ciWidth=self$options$ciWidthOR)

            for (i in seq_along(termsAll)) {
                table <- groups$get(key=i)$coef

                table$getColumn('lower')$setSuperTitle(ciWidthTitle)
                table$getColumn('upper')$setSuperTitle(ciWidthTitle)
                table$getColumn('oddsLower')$setSuperTitle(ciWidthORTitle)
                table$getColumn('oddsUpper')$setSuperTitle(ciWidthORTitle)

                coefTerms <- list()

                terms <- termsAll[[i]]

                for (j in seq_along(terms)) {
                    if (any(terms[[j]] %in% factors)) { # check if there are factors in the term
                        table$addRow(
                            rowKey=terms[[j]],
                            values=list(
                                term = paste0(jmvcore::stringifyTerm(terms[[j]]), ':'),
                                est='', se='', odds='', z='', p='', lower='', upper='',
                                oddsLower='', oddsUpper=''
                            )
                        )

                        coefs <- private$.coefTerms(terms[[j]])
                        coefNames <- coefs$coefNames

                        for (k in seq_along(coefNames)) {
                            rowKey <- jmvcore::composeTerm(coefs$coefTerms[[k]])
                            table$addRow(rowKey=rowKey, values=list(term = coefNames[[k]]))
                            table$addFormat(rowKey=rowKey, col=1, Cell.INDENTED)
                        }

                        coefTerms <- c(coefTerms, coefs$coefTerms)
                    } else {
                        rowKey <- jmvcore::composeTerm(jmvcore::toB64(terms[[j]]))
                        table$addRow(
                            rowKey=rowKey, values=list(term = jmvcore::stringifyTerm(terms[[j]]))
                        )

                        coefTerms[[length(coefTerms) + 1]] <- jmvcore::toB64(terms[[j]])
                    }
                }

                private$coefTerms[[i]] <- coefTerms
            }
        },
        .initThresTables = function() {
            groups <- self$results$models
            termsAll <- private$terms
            data <- self$data

            dep <- self$options$dep

            if ( ! is.null(dep) ) {
                depLevels <- levels(self$data[[dep]])
            } else {
                return()
            }

            for (i in seq_along(termsAll)) {
                table <- groups$get(key=i)$thres

                thresTerms <- list()
                for ( k in 1:(length(depLevels) - 1) ) {
                    rowKey <- paste0(jmvcore::toB64(depLevels[k]), '|', jmvcore::toB64(depLevels[k + 1]))
                    rowName <- paste0(depLevels[k], ' | ', depLevels[k + 1])
                    table$addRow(rowKey=rowKey, values=list(term = rowName))
                    thresTerms[[k]] <- rowKey
                }

                private$thresTerms[[i]] <- thresTerms
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

            for (i in seq_along(termsAll)) {
                table <- groups$get(key=i)$coef

                model <- summary(self$models[[i]])
                CI <- self$CICoefEst[[i]]
                CIOR <- self$CICoefEstOR[[i]]
                coef<- model$coefficients[, 1]
                se <- model$coefficients[, 2]
                wald <- model$coefficients[,3]
                p <- (1 - pnorm(abs(wald), 0, 1)) * 2

                terms <- termsAll[[i]]
                rowTerms <- jmvcore::decomposeTerms(names(coef))

                for (k in seq_along(terms)) {
                    term <- terms[[k]]

                    index <- which(
                        length(term) == sapply(rowTerms, length) &
                            sapply(rowTerms, function(x) all(term %in% x))
                    )

                    row <- list()
                    row[["est"]] <- coef[index]
                    row[["se"]] <- se[index]
                    row[["odds"]] <- exp(coef[index])
                    row[["z"]] <- wald[index]
                    row[["p"]] <- p[index]

                    if (length(terms) == 1) {
                        row[["lower"]] <- CI[1]
                        row[["upper"]] <- CI[2]
                        row[["oddsLower"]] <- CIOR[1]
                        row[["oddsUpper"]] <- CIOR[2]
                    } else {
                        row[["lower"]] <- CI[index, 1]
                        row[["upper"]] <- CI[index, 2]
                        row[["oddsLower"]] <- CIOR[index, 1]
                        row[["oddsUpper"]] <- CIOR[index, 2]
                    }

                    table$setRow(rowKey=jmvcore::composeTerm(terms[[k]]), values = row)
                }
            }
        },
        .populateThresTables = function() {
            groups <- self$results$models
            termsAll <- private$thresTerms

            for (i in seq_along(termsAll)) {
                table <- groups$get(key=i)$thres

                model <- summary(self$models[[i]])
                coef<- model$coefficients[,1]
                se <- model$coefficients[,2]
                wald <- model$coefficients[,3]
                p <- (1 - pnorm(abs(wald), 0, 1)) * 2

                terms <- termsAll[[i]]
                rowTerms <- names(coef)

                for (k in seq_along(terms)) {
                    term <- terms[[k]]

                    index <- which(term == rowTerms)

                    row <- list()
                    row[["est"]] <- coef[index]
                    row[["se"]] <- se[index]
                    row[["odds"]] <- exp(coef[index])
                    row[["z"]] <- wald[index]
                    row[["p"]] <- p[index]

                    table$setRow(rowKey=terms[[k]], values = row)
                }
            }
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
                            '(',
                            paste(contrLevels[[term]], refLevel[[term]], sep = ' \u2013 '),
                            ')'
                        )
                    } else {
                        contr[[term]] <- paste(
                            contrLevels[[term]], refLevel[[term]], sep = ' \u2013 '
                        )
                    }

                    rContr[[term]] <- paste0(
                        jmvcore::toB64(term), jmvcore::toB64(contrLevels[[term]])
                    )
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

            data[[jmvcore::toB64(dep)]] <- factor(
                jmvcore::toB64(as.character(dataRaw[[dep]])),
                levels=jmvcore::toB64(levels(dataRaw[[dep]]))
            )

            refVars <- sapply(refLevels, function(x) x$var)

            for (factor in factors) {
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

            global_weights <- attr(dataRaw, "jmv-weights")
            if (! is.null(global_weights))
                data[[".WEIGHTS"]] <- jmvcore::toNumeric(global_weights)

            attr(data, 'row.names') <- seq_len(length(data[[1]]))
            attr(data, 'class') <- 'data.frame'
            data <- jmvcore::naOmit(data)

            return(data)
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
