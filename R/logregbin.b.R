
logRegBinClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "logRegBinClass",
    inherit = logRegBinBase,
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
            private$.initClassTable()
            private$.initPredMeasuresTable()
            private$.initCollinearityTable()
            # private$.initBoxTidwellTable()

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
                # private$.populateCooksTable(results)
                private$.populateClassTable(results)
                private$.populatePredMeasuresTable(results)
                private$.populateCollinearityTable(results)
                # private$.populateBoxTidwellTable(data, results)
                private$.prepareRocPlot(results$models)
                private$.prepareCutOffPlot(results$models)
            }
        },

        #### Compute results ----
        .compute = function(data) {

            formulas <- private$.formula()

            suppressWarnings({
                suppressMessages({

                    models <- list(); modelTest <- list(); lrTestTerms <- list(); dev <- list();
                    AIC <- list(); BIC <- list(); pseudoR <- list(); VIF <- list(); CI <- list();
                    CIOR <- list(); cooks <- list(); classTable <- list(); AUC <- list()

                    for (i in seq_along(formulas)) {

                        models[[i]] <- stats::glm(formulas[[i]],  data=data, family="binomial")

                        if (self$options$omni)
                            lrTestTerms[[i]] <- car::Anova(models[[i]], test="LR", type=3, singular.ok=TRUE)

                        modelTest[[i]] <- private$.modelTest(models[[i]])
                        dev[[i]] <- models[[i]]$deviance
                        AIC[[i]] <- stats::AIC(models[[i]])
                        BIC[[i]] <- stats::BIC(models[[i]])
                        pseudoR[[i]] <- private$.pseudoR2(models[[i]])

                        classTable[[i]] <- private$.classTable(models[[i]])

                        if (self$options$auc)
                            AUC[[i]] <- private$.auc(models[[i]])

                        # cooks[[i]] <- stats::cooks.distance(models[[i]])

                        CI[[i]] <- try(confint.default(models[[i]], level=self$options$ciWidth/100), silent=TRUE)
                        # if (class(CI[[i]]) == 'try-error')
                        #     CI[[i]] <- confint.default(models[[i]], level=self$options$ciWidth/100)

                        CILO <- try(confint.default(models[[i]], level=self$options$ciWidthOR/100), silent=TRUE)
                        # if (class(CILO) == 'try-error')
                        #     CILO <- confint.default(models[[i]], level=self$options$ciWidthOR/100)

                        CIOR[[i]] <- exp(CILO)

                        if (length(private$terms[[i]]) > 1) {
                            VIF[[i]] <- try(car::vif(models[[i]]), silent=TRUE)

                            if (isError(VIF[[i]]) && extractErrorMessage(VIF[[i]]) == 'there are aliased coefficients in the model')
                                jmvcore::reject("One or more coefficients in model '{}' could not be estimated due to perfect collinearity.", code='error', i)
                        } else {
                            VIF[[i]] <- NULL
                        }
                    }

                    lrTest <- do.call(stats::anova, c(models, test="Chisq"))
                    # bt <- private$.boxTidwell(data, formulas)

                }) # Supress messages
            }) # Supress warnings

            results <- list(models=models, modelTest=modelTest, lrTestTerms=lrTestTerms, dev=dev,
                            AIC=AIC, BIC=BIC, pseudoR=pseudoR, lrTest=lrTest, VIF=VIF, CI=CI,
                            CIOR=CIOR, classTable=classTable, AUC=AUC, cooks=cooks)

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

            dep <- self$options$dep

            if ( ! is.null(dep) ) {
                refLevels <- self$options$refLevels
                depLevels <- levels(self$data[[dep]])
                refVars <- sapply(refLevels, function(x) x$var)
                depRef <- refLevels[[which(dep == refVars)]][['ref']]
                depLevel <- depLevels[-which(depRef == depLevels)]
                note <- paste0("Estimates represent the log odds of \"", dep, " = ", depLevel, "\" vs. \"", dep, " = ", depRef, "\"")
            } else {
                note <- paste0("Estimates represent the log odds of \u2026")
            }

            for (i in seq_along(termsAll)) {

                table <- groups$get(key=i)$coef

                ciWidth <- self$options$ciWidth
                table$getColumn('lower')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
                table$getColumn('upper')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))

                ciWidthOR <- self$options$ciWidthOR
                table$getColumn('oddsLower')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidthOR))
                table$getColumn('oddsUpper')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidthOR))

                coefTerms <- list()

                table$addRow(rowKey="`(Intercept)`", values=list(term = "Intercept"))
                coefTerms[[1]] <- "(Intercept)"

                terms <- termsAll[[i]]

                for (j in seq_along(terms)) {

                    if (any(terms[[j]] %in% factors)) { # check if there are factors in the term

                        table$addRow(rowKey=terms[[j]], values=list(term = paste0(jmvcore::stringifyTerm(terms[[j]]), ':'),
                                                                    est='', se='', odds='', z='', p='',
                                                                    lower='', upper='', oddsLower='', oddsUpper=''))

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
                        table$addRow(rowKey=rowKey, values=list(term = jmvcore::stringifyTerm(terms[[j]])))

                        coefTerms[[length(coefTerms) + 1]] <- jmvcore::toB64(terms[[j]])

                    }
                }

                table$setNote("est", note)
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

            for (i in seq_along(termsAll)) {

                group <- groups$get(key=i)$emm
                terms <- unique(unlist(termsAll[[i]]))

                for (j in seq_along(emMeans)) {

                    emm <- emMeans[[j]]

                    if ( ! is.null(emm) && all(emm %in% terms)) {

                        emmGroup <- group$get(key=j)

                        table <- emmGroup$emmTable
                        table$setTitle(paste0('Estimated Marginal Means - ', jmvcore::stringifyTerm(emm)))

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
        .initClassTable = function() {

            groups <- self$results$models
            dep <- self$options$dep
            cutOff <- self$options$cutOff
            refLevels <- self$options$refLevels

            data <- self$data

            if (is.null(dep)) {
                levels <- c('0', '1')
            } else {
                levels <- levels(data[[dep]])
                ref <- which(refLevels[[1]]$ref == levels)
                levels <- c(levels[ref], levels[-ref])
            }

            for (i in seq_along(self$options$blocks)) {

                table <- groups$get(key=i)$pred$class
                table$setTitle(paste0('Classification Table \u2013 ', dep))

                name <- c('name[0]', 'neg[0]', 'pos[0]', 'perc[0]', 'name[1]', 'neg[1]', 'pos[1]', 'perc[1]')
                title <- rep(c('Observed', levels[1], levels[2], '% Correct'), 2)
                superTitle <- rep(c('', 'Predicted', 'Predicted', ''), 2)

                for (j in seq_along(name))
                    table$addColumn(name=name[j], title=title[j], superTitle=superTitle[j], type='number')

                table$setRow(rowNo=1, values=list('name[0]'=levels[1], 'name[1]'=levels[2]))

                table$setNote("thres", jmvcore::format("The cut-off value is set to {}", cutOff))

            }
        },
        .initPredMeasuresTable = function() {

            groups <- self$results$models
            termsAll <- private$terms
            cutOff <- self$options$cutOff

            for (i in seq_along(termsAll)) {
                table <- groups$get(key=i)$pred$measures
                table$setNote("thres", jmvcore::format("The cut-off value is set to {}", cutOff))
            }
        },
        .initCollinearityTable = function() {

            groups <- self$results$models
            termsAll <- private$terms

            for (i in seq_along(termsAll)) {

                table <- groups$get(key=i)$assump$collin
                terms <- termsAll[[i]]

                if (length(terms) < 1)
                    terms <- ''

                for (i in seq_along(terms))
                    table$addRow(rowKey=i, values=list(term = jmvcore::stringifyTerm(terms[i])))
            }
        },
        .initBoxTidwellTable = function() {

            groups <- self$results$models
            termsAll <- private$terms
            covs <- self$options$covs

            for (i in seq_along(termsAll)) {

                table <- groups$get(key=i)$assump$boxTidwell
                terms <- termsAll[[i]]

                if (length(terms) < 1)
                    terms <- ''

                for (term in terms) {
                    if (length(term) == 1 &&  term %in% covs)
                        table$addRow(rowKey=jmvcore::toB64(term), values=list(term = term))
                }

                table$setNote("ns", "A non-significant result indicates that the assumption is met")
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
                CIOR <- results$CIOR[[i]]
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
                    row[["odds"]] <- exp(coef[index, 'Estimate'])
                    row[["z"]] <- coef[index, 'z value']
                    row[["p"]] <- coef[index, 'Pr(>|z|)']
                    row[["lower"]] <- CI[index, 1]
                    row[["upper"]] <- CI[index, 2]
                    row[["oddsLower"]] <- CIOR[index, 1]
                    row[["oddsUpper"]] <- CIOR[index, 2]

                    table$setRow(rowKey=jmvcore::composeTerm(term), values = row)

                }
            }
        },
        .populateEmmTables = function() {

            groups <- self$results$models
            termsAll <- private$terms
            emMeans <- self$options$emMeans
            factors <- self$options$factors
            covs <- self$options$covs
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

                            row[['prob']] <- emmTable[k, 'prob']
                            row[['se']] <- emmTable[k, 'SE']
                            row[['lower']] <- emmTable[k, 'asymp.LCL']
                            row[['upper']] <- emmTable[k, 'asymp.UCL']

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
        .populateCooksTable = function(results) {

            groups <- self$results$models
            cooksAll <- results$cooks

            for (i in seq_along(cooksAll)) {

                table <- groups$get(key=i)$dataSummary$cooks
                cooks <- cooksAll[[i]]

                row <- list()
                row[['mean']] <- mean(cooks)
                row[['median']] <- median(cooks)
                row[['sd']] <- sd(cooks)
                row[['min']] <- min(cooks)
                row[['max']] <- max(cooks)

                table$setRow(rowNo=1, values=row)
            }
        },
        .populateClassTable = function(results) {

            groups <- self$results$models
            classTables <- results$classTable

            for (i in seq_along(classTables)) {

                table <- groups$get(key=i)$pred$class
                classTable <- classTables[[i]]

                row <- list()
                row[['neg[0]']] <- classTable[1,1]
                row[['pos[0]']] <- classTable[1,2]
                row[['perc[0]']] <- (classTable[1,1] / sum(classTable[1,])) * 100
                row[['neg[1]']] <- classTable[2,1]
                row[['pos[1]']] <- classTable[2,2]
                row[['perc[1]']] <- (classTable[2,2] / sum(classTable[2,])) * 100

                table$setRow(rowNo=1, values=row)
            }
        },
        .populatePredMeasuresTable = function(results) {

            groups <- self$results$models

            for (i in seq_along(groups)) {

                table <- groups$get(key=i)$pred$measures
                classTable <- results$classTable[[i]]

                row <- list()
                row[['accuracy']] <- (classTable[1,1] + classTable[2,2]) / sum(classTable)
                row[['spec']] <- (classTable[1,1] / sum(classTable[1,]))
                row[['sens']] <- (classTable[2,2] / sum(classTable[2,]))

                if (self$options$auc)
                    row[['auc']] <- results$AUC[[i]]

                table$setRow(rowNo=1, values=row)
            }
        },
        .populateCollinearityTable = function(results) {

            groups <- self$results$models
            termsAll <- private$terms

            for (i in seq_along(termsAll)) {

                table <- groups$get(key=i)$assump$collin
                terms <- lapply(termsAll[[i]], jmvcore::toB64)

                if (length(results$VIF) == 0)
                    VIF <- NULL
                else
                    VIF <- results$VIF[[i]]

                if (length(dim(VIF)) > 1) {
                    names <- rownames(VIF)
                    VIF <- VIF[,3]
                    names(VIF) <- names
                }

                rowTerms <- jmvcore::decomposeTerms(names(VIF))

                for (i in seq_along(terms)) {

                    row <- list()

                    if (length(terms) <= 1) {

                        row[["tol"]] <- 1
                        row[["vif"]] <- 1

                    } else {

                        # check which rows have the same length + same terms
                        index <- which(length(terms[[i]]) == sapply(rowTerms, length) &
                                           sapply(rowTerms, function(x) all(terms[[i]] %in% x)))

                        row[["tol"]] <- 1 / as.numeric(VIF[index])
                        row[["vif"]] <- as.numeric(VIF[index])
                    }

                    table$setRow(rowNo=i, values=row)
                }

            }
        },
        .populateBoxTidwellTable = function(data, results) {

            groups <- self$results$models
            termsAll <- private$terms

            for (i in seq_along(termsAll)) {

                table <- groups$get(key=i)$assump$boxTidwell
                termsB64 <- jmvcore::toB64(termsAll[[i]])
                coef <- results$bt[[i]]

                for (term in termsB64) {
                    if ( ! is.factor(data[[term]])) {

                        index <- which(rownames(coef) == paste0('log.', term))

                        row <- list()
                        row[["est"]] <- coef[index, 'Estimate']
                        row[["se"]] <- coef[index, 'Std. Error']
                        row[["z"]] <- coef[index, 'z value']
                        row[["p"]] <- coef[index, 'Pr(>|z|)']

                        table$setRow(rowKey=term, values=row)
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
            depLevels <- levels(self$data[[dep]])
            refVars <- sapply(refLevels, function(x) x$var)
            depRef <- refLevels[[which(dep == refVars)]][['ref']]
            depLevel <- depLevels[-which(depRef == depLevels)]

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

                        FUN <- list(TRUE); FUN2 <- list(TRUE)
                        cont <- FALSE

                        for(k in seq_along(termB64)) {

                            if (term[k] %in% covs) {
                                if (k == 1) {
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

                        for (k in 1:3) {
                            if ( ! is.na(termB64[k])) {
                                if (term[k] %in% covs) {
                                    if (k > 1) {
                                        d[[ termB64[k] ]] <- factor(d[[ termB64[k] ]])
                                        levels(d[[ termB64[k] ]]) <- c('-1SD', 'Mean', '+1SD')
                                    }
                                } else {
                                    d[[ termB64[k] ]] <- factor(jmvcore::fromB64(d[[ termB64[k] ]]),
                                                                jmvcore::fromB64(levels(d[[ termB64[k] ]])))
                                }
                            }
                        }

                        names <- list('x'=termB64[1], 'y'='prob', 'lines'=termB64[2], 'plots'=termB64[3], 'lower'='asymp.LCL', 'upper'='asymp.UCL')
                        names <- lapply(names, function(x) if (is.na(x)) NULL else x)

                        labels <- list('x'=term[1], 'y'=paste0('P(', dep, ' = ', depLevel,')'), 'lines'=term[2], 'plots'=term[3])
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

            if ( ! is.null(names$plots)) {
                formula <- as.formula(paste(". ~", names$plots))
                p <- p + facet_grid(formula)
            }

            p <- p + ylim(0,1) +
                labs(list(x=labels$x, y=labels$y, fill=labels$lines, color=labels$lines)) +
                ggtheme + theme(panel.spacing = unit(2, "lines"))

            print(p)

            TRUE
        },
        .prepareRocPlot = function(models) {

            groups <- self$results$models

            for (i in seq_along(groups)) {

                image <- groups$get(key=i)$pred$rocPlot

                prob <- predict(models[[i]], type=c("response"))
                pred <- ROCR::prediction(prob, models[[i]]$y)
                perf <- ROCR::performance(pred, measure = "tpr", x.measure = "fpr")

                df <- data.frame(x=unlist(perf@x.values), y=unlist(perf@y.values))

                image$setState(df)

            }
        },
        .rocPlot = function(image, ggtheme, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            p <- ggplot(data=image$state, aes(x=x, y=y)) +
                geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
                geom_line(color="red") +
                xlab("1 - Specificity") +
                ylab("Sensitivity") +
                ggtheme

            print(p)

            TRUE
        },
        .prepareCutOffPlot = function(models) {

            groups <- self$results$models

            for (i in seq_along(groups)) {

                image <- groups$get(key=i)$pred$cutOffPlot

                prob <- predict(models[[i]], type=c("response"))
                pred <- ROCR::prediction(prob, models[[i]]$y)

                perf <- ROCR::performance(pred, "sens", "spec")

                cutoff <- perf@alpha.values[[1]]
                sens <- perf@y.values[[1]]
                spec <- perf@x.values[[1]]

                cutoff[cutoff == Inf] <- 1
                cutoff[cutoff == -Inf] <- -1

                df <- data.frame(x=rep(c(0, cutoff), 2),
                                 y=c(1, sens, 0, spec),
                                 group=c(rep('Sensitivity', length(sens) + 1),
                                         rep('Specificity', length(spec) + 1)))

                image$setState(df)
            }
        },
        .cutOffPlot = function(image, ggtheme, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            cutOff <- self$options$cutOff

            p <- ggplot(data=image$state, aes(x=x, y=y, color=group)) +
                geom_vline(xintercept = cutOff, linetype=3, color=theme$color[1]) +
                geom_line() +
                xlab("Cut-Off") + xlim(0,1) + ylim(0,1) +
                ggtheme + theme(legend.title = element_blank(), legend.position = 'top',
                                axis.title.y = element_blank())

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

            if (length(levels(column)) > 2)
                jmvcore::reject(jmvcore::format('The dependent variable \'{}\' has more than two levels;
                                                binomial logistic regression can only be performed on dependent
                                                variables with two levels.', dep), code='')
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
        },
        .classTable = function(model) {

            cutOff <- self$options$cutOff
            pred <- ifelse(fitted(model) > cutOff, 1, 0)
            df <- data.frame(response = factor(model$y, levels = c(0,1)),
                             predicted = factor(pred, levels = c(0,1)))
            classTable <- xtabs(~ response + predicted, data = df)

            return(classTable)
        },
        .auc = function(model) {

            prob <- predict(model, type=c("response"))
            pred <- ROCR::prediction(prob, model$y)
            perf <- ROCR::performance(pred, measure = "auc")

            return(unlist(perf@y.values))
        },
        .boxTidwell = function(data, formulas) {

            termsAll <- private$terms
            termsFull <- termsAll[[length(termsAll)]]
            coVars <- character(0)

            for (term in termsFull) {

                termB64 <- jmvcore::toB64(term)
                column <- data[[termB64]]

                if ( ! is.factor(column)) {

                    if (min(column) <= 0)
                        column <- column - min(column) + 1

                    name <- paste0('log.', termB64)
                    data[[name]] <- column * log(column)
                    data[[termB64]] <- column
                    coVars <- c(coVars, termB64)
                }
            }

            bt <- list()
            for (i in seq_along(termsAll)) {

                formula <- formulas[[i]]

                termsB64 <- jmvcore::toB64(termsAll[[i]])
                logVars <- termsB64[termsB64 %in% coVars]

                if (length(logVars) == 0) {
                    bt[[i]] <- 0
                    next()
                }

                logTerms <- paste0(paste0('log.', logVars), collapse = ' + ')

                formula <- as.formula(paste(formula[2], "~", formula[3], "+", logTerms))
                model <- stats::glm(formula,  data=data, family="binomial")
                bt[[i]] <- summary(model)$coefficients
            }

            return(bt)
        })
)
