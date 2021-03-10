
logRegBinClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "logRegBinClass",
    inherit = logRegBinBase,
    #### Active bindings ----
    active = list(
        dataProcessed = function() {
            if (is.null(private$.dataProcessed))
                private$.dataProcessed <- private$.cleanData()

            return(private$.dataProcessed)
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
        lrtModelComparison = function() {
            if (is.null(private$.lrtModelComparison))
                private$.lrtModelComparison <- do.call(stats::anova, c(self$models, test="Chisq"))

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
        VIF = function() {
            if (is.null(private$.VIF))
                private$.VIF <- private$.computeVIF()

            return(private$.VIF)
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
        classTable = function() {
            if (is.null(private$.classTable))
                private$.classTable <- private$.computeClassTable()

            return(private$.classTable)
        },
        AUC = function() {
            if (is.null(private$.AUC))
                private$.AUC <- private$.computeAUC()

            return(private$.AUC)
        },
        emMeans = function() {
            if (is.null(private$.emMeans))
                private$.emMeans <- private$.computeEmMeans()

            return(private$.emMeans)
        }
    ),
    private = list(
        #### Member variables ----
        .dataProcessed = NULL,
        .dataRowNums = NULL,
        .formulas = NULL,
        .models = NULL,
        .nModels = NULL,
        .residuals = NULL,
        .fitted = NULL,
        .predicted = NULL,
        .cooks = NULL,
        .modelTerms = NULL,
        .lrtModelComparison = NULL,
        .lrtModelTerms = NULL,
        .deviance = NULL,
        .AIC = NULL,
        .BIC = NULL,
        .pseudoR2 = NULL,
        .modelTest = NULL,
        .VIF = NULL,
        .CICoefEst = NULL,
        .CICoefEstOR = NULL,
        .classTable = NULL,
        .AUC = NULL,
        .rowNamesModel = NULL,
        .levelsDep = NULL,
        .emMeans = NULL,
        .emMeansForPlot = NULL,

        #### Init + run functions ----
        .init = function() {
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
            private$.initOutputs()
        },
        .run = function() {
            if (is.null(self$options$dep) || self$nModels < 1 || length(self$options$blocks[[1]]) == 0)
                return()

            private$.errorCheck()

            private$.populateModelFitTable()
            private$.populateModelCompTable()
            private$.populateLrtTables()
            private$.populateCoefTables()

            private$.populateEmmTables()
            private$.populateClassTable()
            private$.populatePredMeasuresTable()
            private$.populateCollinearityTable()

            private$.prepareRocPlot()
            private$.prepareCutOffPlot()
            private$.prepareEmmPlots()

            private$.populateOutputs()
        },

        #### Compute results ----
        .computeModels = function(modelNo = NULL) {
            data <- self$dataProcessed
            formulas <- self$formulas

            if (is.numeric(modelNo))
                formulas <- formulas[modelNo]

            models <- list()
            for (i in seq_along(formulas))
                models[[i]] <- stats::glm(formulas[[i]],  data=data, family="binomial")

            return(models)
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
            predicted <- list()
            for (i in seq_along(self$models))
                predicted[[i]] <- predict(self$models[[i]], data, type="response")

            return(predicted)
        },
        .computeCooks = function() {
            cooks <- list()
            for (i in seq_along(self$models))
                cooks[[i]] <- stats::cooks.distance(self$models[[i]])

            return(cooks)
        },
        .computeLrtModelTerms = function() {
            lrtModelTerms <- list()
            for (i in seq_along(self$models)) {
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
            for (i in seq_along(self$models))
                dev[[i]] <- self$models[[i]]$deviance

            return(dev)
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
        .computePseudoR2 = function() {
            pR2 <- list()
            for (i in seq_along(self$models)) {
                dev <- self$deviance[[i]]
                nullDev <- self$models[[i]]$null.deviance
                n <- length(self$models[[i]]$fitted.values)

                r2mf <- 1 - dev/nullDev
                r2cs <- 1 - exp(-(nullDev - dev) / n)
                r2n <- r2cs / (1 - exp(-nullDev / n))

                pR2[[i]] <- list(r2mf=r2mf, r2cs=r2cs, r2n=r2n)
            }
            return(pR2)
        },
        .computeModelTest = function() {
            modelTest <- list()
            for (i in seq_along(self$models)) {
                chi <- self$models[[i]]$null.deviance - self$deviance[[i]]
                df <- self$models[[i]]$df.null - self$models[[i]]$df.residual
                p <- 1 - pchisq(chi, df)

                modelTest[[i]] <- list(chi=chi, df=df, p=p)
            }
            return(modelTest)
        },
        .computeVIF = function() {
            VIF <- list()
            for (i in seq_along(self$models)) {
                if (length(private$.getModelTerms()[[i]]) > 1) {
                    VIF[[i]] <- try(car::vif(self$models[[i]]), silent=TRUE)

                    if (isError(VIF[[i]]) && extractErrorMessage(VIF[[i]]) == 'there are aliased coefficients in the model') {
                        jmvcore::reject("One or more coefficients in model '{}' could not be estimated due to perfect collinearity.", code='error', i)
                    }
                } else {
                    VIF[[i]] <- NULL
                }
            }
            return(VIF)
        },
        .computeCICoefEst = function(type="LOR") {
            if (type == "OR")
                level <- self$options$ciWidthOR/100
            else
                level <- self$options$ciWidth/100

            ci <- list()
            for (i in seq_along(self$models)) {
                ci[[i]] <- confint.default(self$models[[i]], level=level)
                if (type == "OR")
                    ci[[i]] <- exp(ci[[i]])
            }
            return(ci)
        },
        .computeClassTable = function() {
            class <- list()
            for (i in seq_along(self$models)) {
                pred <- ifelse(self$fitted[[i]] > self$options$cutOff, 1, 0)
                df <- data.frame(response=factor(self$models[[i]]$y, levels=c(0,1)),
                                 predicted=factor(pred, levels=c(0,1)))
                class[[i]] <- stats::xtabs(~ response + predicted, data=df)
            }
            return(class)
        },
        .computeAUC = function() {
            AUC <- list()
            for (i in seq_along(self$models)) {
                prob <- self$fitted[[i]]
                pred <- ROCR::prediction(prob, self$models[[i]]$y)
                perf <- ROCR::performance(pred, measure = "auc")

                AUC[[i]] <- unlist(perf@y.values)
            }
            return(AUC)
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
                                emmeans::emmeans(model, formula, cov.reduce=FUN, type='response',
                                                 options=list(level=self$options$ciWidthEmm / 100),
                                                 weights=weights, data=self$dataProcessed),
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

            for (i in seq_along(private$.getModelTerms()))
                table$addRow(rowKey=i, values=list(model = i))
        },
        .initModelCompTable = function() {
            table <- self$results$modelComp
            terms <- private$.getModelTerms()

            if (length(terms) <= 1) {
                table$setVisible(visible = FALSE)
                return()
            }

            for (i in 1:(length(terms)-1))
                table$addRow(rowKey=i, values=list(model1 = i, model2 = as.integer(i+1)))

        },
        .initModelSpec = function() {
            groups <- self$results$models

            for (i in seq_along(private$.getModelTerms())) {
                groups$addItem(key=i)
                group <- groups$get(key=i)
                group$setTitle(paste("Model",i))
            }
        },
        .initLrtTables = function() {
            groups <- self$results$models
            termsAll <- private$.getModelTerms()

            for (i in seq_along(termsAll)) {
                table <- groups$get(key=i)$lrt
                terms <- termsAll[[i]]

                for (j in seq_along(terms))
                    table$addRow(rowKey=paste0(terms[[j]]), values=list(term = jmvcore::stringifyTerm(terms[j])))
            }
        },
        .initCoefTables = function() {
            groups <- self$results$models
            factors <- self$options$factors
            termsAll <- private$.getModelTerms()
            data <- self$data

            dep <- self$options$dep
            if (is.null(dep) ) {
                note <- paste0("Estimates represent the log odds of \u2026")
            } else {
                depLevels <- private$.getLevelsDep()
                note <- paste0("Estimates represent the log odds of \"",
                               dep, " = ", depLevels$other, "\" vs. \"", dep, " = ",
                               depLevels$ref, "\"")
            }

            rowNamesModel <- private$.getRowNamesModel()

            for (i in seq_along(termsAll)) {
                table <- groups$get(key=i)$coef

                ciWidth <- self$options$ciWidth
                table$getColumn('lower')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
                table$getColumn('upper')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))

                ciWidthOR <- self$options$ciWidthOR
                table$getColumn('oddsLower')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidthOR))
                table$getColumn('oddsUpper')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidthOR))

                coefTerms <- rowNamesModel[[i]]

                table$addRow(rowKey="`(Intercept)`", values=list(term = "Intercept"))

                terms <- termsAll[[i]]

                iter <- 1
                for (j in seq_along(terms)) {
                    if (any(terms[[j]] %in% factors)) {

                        table$addRow(rowKey=terms[[j]],
                                     values=list(term = paste0(jmvcore::stringifyTerm(terms[[j]]), ':'),
                                                 est='', se='', odds='', z='', p='',
                                                 lower='', upper='', oddsLower='', oddsUpper=''))

                        contrastNames <- private$.contrastsCoefTable(terms[[j]])

                        for (k in seq_along(contrastNames)) {
                            iter <- iter + 1
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

                table$setNote("est", note)
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
            termsAll <- private$.getModelTerms()
            cutOff <- self$options$cutOff

            for (i in seq_along(termsAll)) {
                table <- groups$get(key=i)$pred$measures
                table$setNote("thres", jmvcore::format("The cut-off value is set to {}", cutOff))
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
        .initOutputs = function() {
            description = function(part1, part2=NULL) {
                return(
                    jmvcore::format(
                        "{} of binomial logistic regression model{}",
                        part1,
                        ifelse(is.null(part2), "", paste0(" ", part2))
                    )
                )
            }

            title = function(part1=NULL, part2=NULL) {
                return(jmvcore::format("{} - {}", part1, part2))
            }

            predictTitle <- "Predicted"
            residsTitle <- "Residuals"
            cooksTitle <- "Cook's distance"

            if (is.null(self$options$dep)) {
                dep <- "\u2026"
                predictDescPrefix <- "Predicted probability of \u2026 = \u2026 (vs \u2026 = \u2026)"
            } else {
                dep <- self$options$dep
                predictDescPrefix <- jmvcore::format("Predicted probability of {} = {} (vs {} = {})",
                                                     dep,
                                                     private$.getLevelsDep()$other,
                                                     dep,
                                                     private$.getLevelsDep()$ref)
            }

            if (self$nModels == 1) {

                self$results$predictOV$set(1, title(predictTitle, dep), description(predictDescPrefix), 'continuous')
                self$results$residsOV$set(1, residsTitle, description(residsTitle), 'continuous')
                self$results$cooksOV$set(1, cooksTitle, description(cooksTitle), 'continuous')

            } else if (self$nModels > 1) {

                keys <- seq_len(self$nModels)
                measureTypes <- rep('continuous', self$nModels)

                titles <- vapply(keys, function(key) title(predictTitle, paste(dep, key)), '')
                descriptions <- vapply(keys, function(key) description(predictDescPrefix, key), '')
                self$results$predictOV$set(keys, titles, descriptions, measureTypes)

                titles <- vapply(keys, function(key) title(residsTitle, key), '')
                descriptions <- vapply(keys, function(key) description(residsTitle, key), '')
                self$results$residsOV$set(keys, titles, descriptions, measureTypes)

                titles <- vapply(keys, function(key) title(cooksTitle, key), '')
                descriptions <- vapply(keys, function(key) description(cooksTitle, key), '')
                self$results$cooksOV$set(keys, titles, descriptions, measureTypes)
            }
        },

        #### Populate tables functions ----
        .populateModelFitTable = function() {
            table <- self$results$modelFit
            for (i in 1:self$nModels) {
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
        },
        .populateModelCompTable = function() {
            if (self$nModels <= 1)
                return()

            table <- self$results$modelComp
            r <- self$lrtModelComparison[-1,]

            for (i in 1:(self$nModels - 1)) {
                row <- list()
                row[["chi"]] <- r$Deviance[i]
                row[["df"]] <- r$Df[i]
                row[["p"]] <- r$`Pr(>Chi)`[i]

                table$setRow(rowNo=i, values = row)
            }
        },
        .populateLrtTables = function() {
            if ( ! self$options$omni)
                return()

            groups <- self$results$models
            termsAll <- private$.getModelTerms()
            lrTests <- self$lrtModelTerms

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
        .populateCoefTables = function() {
            groups <- self$results$models
            termsAll <- private$.getRowNamesModel()

            for (i in seq_along(termsAll)) {
                table <- groups$get(key=i)$coef

                model <- summary(self$models[[i]])
                CI <- self$CICoefEst[[i]]
                CIOR <- self$CICoefEstOR[[i]]
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
        .populateClassTable = function(results) {
            if ( ! self$options$class)
                return()

            groups <- self$results$models
            classTables <- self$classTable

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
        .populatePredMeasuresTable = function() {
            groups <- self$results$models

            for (i in seq_along(groups)) {
                table <- groups$get(key=i)$pred$measures

                row <- list()

                if (self$options$acc || self$options$spec || self$options$sens) {
                    classTable <- self$classTable[[i]]

                    row[['accuracy']] <- (classTable[1,1] + classTable[2,2]) / sum(classTable)
                    row[['spec']] <- (classTable[1,1] / sum(classTable[1,]))
                    row[['sens']] <- (classTable[2,2] / sum(classTable[2,]))
                }

                if (self$options$auc)
                    row[['auc']] <- self$AUC[[i]]

                table$setRow(rowNo=1, values=row)
            }
        },
        .populateCollinearityTable = function() {
            if ( ! self$options$collin)
                return()

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
        .populateOutputs = function() {
            if (self$options$predictOV && self$results$predictOV$isNotFilled()) {
                self$results$predictOV$setRowNums(names(self$predicted[[1]]))
                for (i in seq_along(self$predicted))
                    self$results$predictOV$setValues(index=i, as.numeric(self$predicted[[i]]))
            }

            if (self$options$residsOV && self$results$residsOV$isNotFilled()) {
                self$results$residsOV$setRowNums(private$.getDataRowNums())
                for (i in seq_along(self$residuals))
                    self$results$residsOV$setValues(index=i, self$residuals[[i]])
            }

            if (self$options$cooksOV && self$results$cooksOV$isNotFilled()) {
                self$results$cooksOV$setRowNums(private$.getDataRowNums())
                for (i in seq_along(self$cooks))
                    self$results$cooksOV$setValues(index=i, self$cooks[[i]])
            }
        },

        #### Plot functions ----
        .prepareEmmPlots = function() {
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
                                    d[[ termB64[k] ]] <- factor(jmvcore::fromB64(d[[ termB64[k] ]]),
                                                                jmvcore::fromB64(levels(d[[ termB64[k] ]])))
                                }
                            }
                        }

                        names <- list('x'=termB64[1], 'y'='prob', 'lines'=termB64[2],
                                      'plots'=termB64[3], 'lower'='asymp.LCL', 'upper'='asymp.UCL')
                        names <- lapply(names, function(x) if (is.na(x)) NULL else x)

                        labels <- list('x'=term[1], 'y'=paste0('P(', dep, ' = ', private$.getLevelsDep()$other,')'),
                                       'lines'=term[2], 'plots'=term[3])
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
                labs(x=labels$x, y=labels$y, fill=labels$lines, color=labels$lines) +
                ggtheme + theme(panel.spacing = unit(2, "lines"))

            return(p)
        },
        .prepareRocPlot = function() {
            groups <- self$results$models
            for (i in seq_along(groups)) {
                image <- groups$get(key=i)$pred$rocPlot
                image$setState(i)
            }
        },
        .rocPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            prob <- predict(self$models[[image$state]], type=c("response"))
            pred <- ROCR::prediction(self$fitted[[image$state]], self$models[[image$state]]$y)
            perf <- ROCR::performance(pred, measure = "tpr", x.measure = "fpr")

            df <- data.frame(x=unlist(perf@x.values), y=unlist(perf@y.values))

            p <- ggplot(data=df, aes(x=x, y=y)) +
                geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
                geom_line(color="red") +
                xlab("1 - Specificity") +
                ylab("Sensitivity") +
                ggtheme

            return(p)
        },
        .prepareCutOffPlot = function() {
            groups <- self$results$models
            for (i in seq_along(groups)) {
                image <- groups$get(key=i)$pred$cutOffPlot
                image$setState(i)
            }
        },
        .cutOffPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            cutOff <- self$options$cutOff
            prob <- self$fitted[[image$state]]
            pred <- ROCR::prediction(prob, self$models[[image$state]]$y)

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

            p <- ggplot(data=df, aes(x=x, y=y, color=group)) +
                geom_vline(xintercept = cutOff, linetype=3, color=theme$color[1]) +
                geom_line() +
                xlab("Cut-Off") + xlim(0,1) + ylim(0,1) +
                ggtheme + theme(legend.title = element_blank(), legend.position = 'top',
                                axis.title.y = element_blank())

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
        .getLevelsDep = function() {
            if (is.null(private$.levelsDep) && ! is.null(self$options$dep))
                private$.levelsDep <- private$.getLevels(self$options$dep)

            return(private$.levelsDep)
        },
        .getLevels = function(var) {
            levels <- levels(self$data[[var]])
            refLevels <- self$options$refLevels
            refVars <- sapply(refLevels, function(x) x$var)
            ref <- refLevels[[which(var == refVars)]][['ref']]
            other <- levels[-which(ref == levels)]

            return(list("ref"=ref, "other"=other))
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
        .getFormulas = function() {
            dep <- self$options$dep
            depB64 <- jmvcore::toB64(dep)
            terms <- private$.getModelTerms()

            formulas <- list()
            for (i in seq_along(terms)) {
                termsB64 <- lapply(terms[[i]], jmvcore::toB64)
                composedTerms <- jmvcore::composeTerms(termsB64)
                formulas[[i]] <- as.formula(paste(depB64, paste0(composedTerms, collapse ="+"), sep="~"))
            }

            return(formulas)
        },
        .errorCheck = function() {
            dep <- self$options$dep
            column <- self$data[[dep]]

            if (length(levels(column)) > 2)
                jmvcore::reject(jmvcore::format('The dependent variable \'{}\' has more than two levels;
                                                binomial logistic regression can only be performed on dependent
                                                variables with two levels.', dep), code='')
        },
        .cleanData = function(naOmit=TRUE, naSkip=NULL) {
            dep <- self$options$dep
            covs <- self$options$covs
            factors <- self$options$factors
            refLevels <- self$options$refLevels

            dataRaw <- self$data

            data <- list()

            refVars <- sapply(refLevels, function(x) x$var)

            for (factor in c(dep, factors)) {
                ref <- refLevels[[which(factor == refVars)]][['ref']]
                column <- dataRaw[[factor]]
                levels(column) <- jmvcore::toB64(levels(column))
                column <- relevel(column, ref = jmvcore::toB64(ref))

                data[[jmvcore::toB64(factor)]] <- column
            }

            for (cov in covs)
                data[[jmvcore::toB64(cov)]] <- jmvcore::toNumeric(dataRaw[[cov]])

            attr(data, 'row.names') <- seq_len(length(data[[1]]))
            attr(data, 'class') <- 'data.frame'

            if (naOmit) {
                data <- tibble::rownames_to_column(data)
                data <- tidyr::drop_na(data, -naSkip)
                data <- tibble::column_to_rownames(data)
            }

            return(data)
        },
        .contrastModel = function(terms) {
            #' Returns the names of the factor contrasts as they are
            #' defined by the glm function

            factors <- self$options$factors

            nLevels <- list()
            for (factor in factors)
                nLevels[[factor]] <- length(levels(self$data[[factor]]))

            contrast <- list()
            for (term in terms) {
                if (term %in% factors) {
                    contrast[[term]] <- paste0(
                        jmvcore::toB64(term),
                        jmvcore::toB64(private$.getLevels(term)$other)
                    )
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
        .contrastsCoefTable = function(terms) {
            #' Returns the names of the factor contrasts as they are
            #' displayed in the jmv coef table

            factors <- self$options$factors
            refLevels <- self$options$refLevels
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
        })
)
