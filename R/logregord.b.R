
logRegOrdClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "logRegOrdClass",
    inherit = logRegOrdBase,
    private = list(
        #### Member variables ----
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
                private$.populateThresTables(results)
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
                    AIC <- list(); BIC <- list(); pseudoR <- list(); CI <- list(); CIOR <- list()

                    nullFormula <- as.formula(paste0(jmvcore::toB64(self$options$dep), '~ 1'))
                    nullModel <- MASS::polr(nullFormula, data=data, model=TRUE, Hess=TRUE)
                    null <- list(dev=nullModel$deviance, df=nullModel$edf)

                    for (i in seq_along(formulas)) {

                        models[[i]] <- MASS::polr(formulas[[i]], data=data, model=TRUE, Hess=TRUE)
                        models[[i]]$call$formula <- formulas[[i]]

                        lrTestTerms[[i]] <- car::Anova(models[[i]], test="LR", type=3, singular.ok=TRUE)
                        modelTest[[i]] <- private$.modelTest(models[[i]], null)
                        dev[[i]] <- models[[i]]$deviance
                        AIC[[i]] <- stats::AIC(models[[i]])
                        BIC[[i]] <- stats::BIC(models[[i]])
                        pseudoR[[i]] <- private$.pseudoR2(models[[i]], null)

                        CI[[i]] <- try(confint(models[[i]], level=self$options$ciWidth/100), silent=TRUE)
                        # if (class(CI[[i]]) == 'try-error')
                        #     CI[[i]] <- confint.default(models[[i]], level=self$options$ciWidth/100)

                        CILO <- try(confint(models[[i]], level=self$options$ciWidthOR/100), silent=TRUE)
                        # if (class(CILO[[i]]) == 'try-error')
                        #     CILO <- confint.default(models[[i]], level=self$options$ciWidthOR/100)

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

            dep <- self$options$dep

            if ( ! is.null(dep) ) {
                depLevels <- levels(self$data[[dep]])
            } else {
                return()
            }

            table$setNote("note", jmvcore::format("The dependent variable \'{}\' has the following order: {}", dep, paste(depLevels, collapse = ' | ')))

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
                depLevels <- levels(self$data[[dep]])
            } else {
                depLevels <- NULL
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

            for (i in seq_along(termsAll)) {

                table <- groups$get(key=i)$coef

                model <- summary(models[[i]])
                CI <- results$CI[[i]]
                CIOR <- results$CIOR[[i]]
                coef<- model$coefficients[,1]
                se <- model$coefficients[,2]
                wald <- model$coefficients[,3]
                p <- (1 - pnorm(abs(wald), 0, 1)) * 2

                terms <- termsAll[[i]]
                rowTerms <- jmvcore::decomposeTerms(names(coef))

                for (k in seq_along(terms)) {

                    term <- terms[[k]]

                    index <- which(length(term) == sapply(rowTerms, length) &
                                       sapply(rowTerms, function(x) all(term %in% x)))

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
        .populateThresTables = function(results) {

            groups <- self$results$models
            termsAll <- private$thresTerms
            models <- results$models

            for (i in seq_along(termsAll)) {

                table <- groups$get(key=i)$thres

                model <- summary(models[[i]])
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
        .formulas = function() {

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

            data[[jmvcore::toB64(dep)]] <- factor(jmvcore::toB64(as.character(dataRaw[[dep]])),
                                                  levels=jmvcore::toB64(levels(dataRaw[[dep]])))

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
