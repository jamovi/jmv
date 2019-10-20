
linRegClass <- R6::R6Class(
    "linRegClass",
    inherit = linRegBase,
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
            private$.initAnovaTables()
            private$.initCoefTable()
            private$.initCollinearityTable()
            private$.initResPlots()
            private$.initEmm()
            private$.initEmmTable()

        },
        .run = function() {

            ready <- TRUE
            if (is.null(self$options$dep) || length(self$options$blocks) < 1 || length(self$options$blocks[[1]]) == 0)
                ready <- FALSE

            if (ready) {

                data <- private$.cleanData()
                results <- private$.compute(data)

                private$.populateModelFitTable(results)
                private$.populateModelCompTable(results)
                private$.populateAnovaTables(results)
                private$.populateCoefTables(results)

                private$.populateCooksTable(results)
                private$.populateCollinearityTable(results)
                private$.populateDurbinWatsonTable(results)
                private$.populateNormality(results)
                private$.prepareQQPlot(results)
                private$.prepareResPlots(data, results)

                private$.prepareEmmPlots(results$models, data=data)
                private$.populateEmmTables()

                # private$.prepareCoefPlot(results)
            }
        },

        #### Compute results ----
        .compute = function(data) {

            formulas <- private$.formulas()
            scaledData <- private$.scaleData(data)

            models <- list(); modelsScaled <- list(); anovaTerms <- list()
            for (i in seq_along(formulas)) {
                models[[i]] <- lm(formulas[[i]], data=data)
                anovaTerms[[i]] <- car::Anova(models[[i]], type=3, singular.ok=TRUE)
                modelsScaled[[i]] <- lm(formulas[[i]], data=scaledData)
            }

            ANOVA <- do.call(stats::anova, models)

            AIC <- list(); BIC <- list(); CI <- list();
            CIScaled <- list(); dwTest <- list(); VIF <- list(); cooks <- list()
            for (i in seq_along(models)) {

                AIC[[i]] <- stats::AIC(models[[i]])
                BIC[[i]] <- stats::BIC(models[[i]])
                # betas[[i]] <- private$.stdEst(models[[i]])
                CI[[i]] <- stats::confint(models[[i]], level = self$options$ciWidth / 100)
                CIScaled[[i]] <- stats::confint(modelsScaled[[i]], level = self$options$ciWidth / 100)
                cooks[[i]] <- stats::cooks.distance(models[[i]])

                if (length(private$terms[[i]]) > 1)
                    VIF[[i]] <- car::vif(models[[i]])
                else
                    VIF[[i]] <- NULL

            }

            return(list(models=models, modelsScaled=modelsScaled, ANOVA=ANOVA,
                        anovaTerms=anovaTerms, AIC=AIC, BIC=BIC, CI=CI, CIScaled=CIScaled,
                        dwTest=dwTest, VIF=VIF, cooks=cooks))
        },

        #### Init tables/plots functions ----
        .initModelFitTable = function() {

            table <- self$results$modelFit
            blocks <- self$options$blocks

            for (i in seq_along(self$options$blocks))
                table$addRow(rowKey=i, values=list(model = i))

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
            termsAll <- private$terms

            for (i in seq_along(termsAll)) {

                table <- groups$get(key=i)$anova
                terms <- termsAll[[i]]

                for (j in seq_along(terms))
                    table$addRow(rowKey=paste0(terms[[j]]), values=list(term = jmvcore::stringifyTerm(terms[j])))

                table$addRow(rowKey='.RES', values=list(term = 'Residuals'))
                table$addFormat(col=1, rowKey='.RES', format=Cell.BEGIN_GROUP)

                table$setNote("ss", "Type 3 sum of squares")
            }
        },
        .initCoefTable = function() {

            groups <- self$results$models
            termsAll <- private$terms
            data <- self$data

            factors <- self$options$factors
            dep <- self$options$dep

            for (i in seq_along(termsAll)) {

                table <- groups$get(key=i)$coef

                ciWidth <- self$options$ciWidth
                table$getColumn('lower')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
                table$getColumn('upper')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))

                ciWidthStdEst <- self$options$ciWidthStdEst
                table$getColumn('stdEstLower')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidthStdEst))
                table$getColumn('stdEstUpper')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidthStdEst))

                coefTerms <- list()

                table$addRow(rowKey="`(Intercept)`", values=list(term = "Intercept"))
                coefTerms[[1]] <- "(Intercept)"

                terms <- termsAll[[i]]

                for (j in seq_along(terms)) {

                    if (any(terms[[j]] %in% factors)) { # check if there are factors in the term

                        table$addRow(rowKey=terms[[j]], values=list(term = paste0(jmvcore::stringifyTerm(terms[[j]]), ':'),
                                                                    est='', se='', t='', p='',
                                                                    lower='', upper='', stdEst='',
                                                                    stdEstLower='', stdEstUpper=''))

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
        .initResPlots=function() {

            groups <- self$results$models
            termsAll <- private$terms

            covs <- self$options$covs

            for (i in seq_along(termsAll)) {

                modelTerms <- termsAll[[i]]

                if (length(modelTerms) < 1) {
                    terms <- ''
                } else {
                    terms <- c('Fitted', self$options$dep)
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

                        table$addColumn(name='emmean', title='Marginal Mean', type='number')
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

            models <- results$models
            # modelsBF <- results$modelsBF
            AIC <- results$AIC
            BIC <- results$BIC

            for (i in seq_along(models)) {

                row <- list()
                row[["aic"]] <- AIC[[i]]
                row[["bic"]] <- BIC[[i]]
                row[["r"]] <- sqrt(summary(models[[i]])$r.squared)
                row[["r2"]] <- summary(models[[i]])$r.squared
                row[["r2Adj"]] <- summary(models[[i]])$adj.r.squared
                row[["rmse"]] <- sqrt(mean(models[[i]]$residuals^2))
                # row[["bf"]] <- exp(modelsBF[[i]]@bayesFactor$bf)
                # row[["err"]] <- modelsBF[[i]]@bayesFactor$error

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
        },
        .populateModelCompTable = function(results) {

            table <- self$results$modelComp

            models <- results$models
            # modelsBF <- results$modelsBF
            ANOVA <- results$ANOVA
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

                # BF <- modelsBF[[i+1]]/modelsBF[[i]]

                # row[["bf"]] <- exp(BF@bayesFactor$bf)
                # row[["err"]] <- BF@bayesFactor$error

                table$setRow(rowNo=i, values = row)
            }
        },
        .populateAnovaTables = function(results) {

            groups <- self$results$models
            termsAll <- private$terms
            anova <- results$anovaTerms

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

                    ss <- r[index,'Sum Sq']
                    df <- r[index,'Df']
                    ms <- ss / df
                    F  <- r[index,'F value']
                    p  <- r[index,'Pr(>F)']

                    if ( ! is.finite(ss))
                        ss <- 0
                    if ( ! is.finite(ms))
                        ms <- ''
                    if ( ! is.finite(F))
                        F <- ''
                    if ( ! is.finite(p))
                        p <- ''

                    row <- list(ss=ss, df=df, ms=ms, F=F, p=p)

                    table$setRow(rowKey=paste0(terms[[j]]), values = row)
                }

                ss <- r[resIndex,'Sum Sq']
                df <- r[resIndex,'Df']
                ms <- ss / df

                row <- list(ss=ss, df=df, ms=ms, F='', p='')

                table$setRow(rowKey='.RES', values = row)
            }
        },
        .populateCoefTables = function(results) {

            groups <- self$results$models
            termsAll <- private$coefTerms
            models <- results$models
            modelsScaled <- results$modelsScaled

            for (i in seq_along(termsAll)) {

                table <- groups$get(key=i)$coef

                model <- summary(models[[i]])
                modelScaled <- summary(modelsScaled[[i]])

                CI <- results$CI[[i]]
                CIScaled <- results$CIScaled[[i]]
                coef<- model$coef
                coefScaled <- modelScaled$coef
                # stdEst <- results$betas[[i]]
                terms <- termsAll[[i]]
                rowTerms <- jmvcore::decomposeTerms(rownames(coef))

                for (j in seq_along(terms)) {

                    term <- terms[[j]]

                    # check which rows have the same length + same terms
                    index <- which(length(term) == sapply(rowTerms, length) &
                                       sapply(rowTerms, function(x) all(term %in% x)))

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

                    table$setRow(rowKey=jmvcore::composeTerm(term), values = row)

                }
            }
        },
        .populateCooksTable = function(results) {

            groups <- self$results$models
            termsAll <- private$terms

            for (i in seq_along(termsAll)) {

                table <- groups$get(key=i)$dataSummary$cooks
                cooks <- results$cooks[[i]]

                row <- list()
                row[['mean']] <- mean(cooks)
                row[['median']] <- median(cooks)
                row[['sd']] <- sd(cooks)
                row[['min']] <- min(cooks)
                row[['max']] <- max(cooks)

                table$setRow(rowNo=1, values=row)
            }
        },
        .populateDurbinWatsonTable = function(results) {

            groups <- self$results$models
            termsAll <- private$terms

            for (i in seq_along(termsAll)) {

                table <- groups$get(key=i)$assump$durbin
                dwTest <- results$dwTest[[i]]

                row <- list()
                row[["autoCor"]] <- as.numeric(dwTest[1])
                row[["dw"]] <- as.numeric(dwTest[2])
                row[["p"]] <- as.numeric(dwTest[3])

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

                            row[['emmean']] <- emmTable[k, 'emmean']
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

        .populateNormality = function(results) {

            groups <- self$results$models
            termsAll <- private$terms

            for (i in seq_along(termsAll)) {

                model <- results$models[[i]]
                table <- groups$get(key=i)$assump$get('norm')

                res <- try(shapiro.test(model$residuals))
                if (jmvcore::isError(res)) {
                    values <- list(`s[sw]`=NaN, `p[sw]`='')
                } else {
                    values <- list(`s[sw]`=res$statistic, `p[sw]`=res$p.value)
                }
                table$setRow(rowNo=1, values)
            }

        },

        #### Plot functions ----
        .prepareQQPlot = function(results) {

            groups <- self$results$models
            termsAll <- private$terms

            for (i in seq_along(termsAll)) {

                model <- results$models[[i]]

                image <- groups$get(key=i)$assump$get('qqPlot')
                df <- as.data.frame(qqnorm(scale(model$residuals), plot.it=FALSE))

                image$setState(df)
            }
        },
        .qqPlot = function(image, ggtheme, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            p <- ggplot(data=image$state, aes(x=x, y=y)) +
                      geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
                      geom_point(aes(x=x,y=y), size=2, colour=theme$color[1]) +
                      xlab("Theoretical Quantiles") +
                      ylab("Standardized Residuals") +
                      ggtheme

            return(p)
        },
        .prepareResPlots = function(data, results) {

            groups <- self$results$models
            termsAll <- private$terms

            for (i in seq_along(termsAll)) {

                model <- results$models[[i]]
                res <- model$residuals

                images <- groups$get(key=i)$assump$resPlots
                for (term in images$itemKeys) {

                    if (term == 'Fitted') {
                        x <- model$fitted.values
                    } else {
                        x <- data[[jmvcore::toB64(term)]]
                    }

                    df <- data.frame(y=res, x=x)

                    image <- images$get(key=term)
                    image$setState(list(df=df, xlab=term))
                }
            }
        },
        .resPlot = function(image, ggtheme, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            p <- ggplot(data=image$state$df, aes(y=y, x=x)) +
                      geom_point(aes(x=x,y=y), colour=theme$color[1]) +
                      xlab(image$state$xlab) +
                      ylab("Residuals") +
                      ggtheme

            return(p)
        },
        .prepareCoefPlot = function(results) {

            image <- self$results$coefPlot

            betas <- results$betas[[private$modelSelected]]

            df <- data.frame(
                term = jmvcore::fromB64(names(betas$beta)),
                estimate = as.numeric(betas$beta),
                conf.low = as.numeric(betas$lower),
                conf.high = as.numeric(betas$upper),
                group = rep('CI', length(betas$beta))
            )
            df$term <- factor(df$term, rev(df$term))

            image$setState(df)
        },
        .coefPlot = function(image, ggtheme, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            themeSpec <- theme(
                legend.position = 'right',
                legend.background = element_rect("transparent"),
                legend.title = element_blank(),
                legend.key = element_blank(),
                legend.text = element_text(size=16, colour='#333333'))

            errorType <- paste0(self$options$ciWidth, '% CI')

            p <- ggplot(data=image$state) +
                geom_hline(yintercept=0, linetype="dotted", colour=theme$color[1], size=1.2) +
                geom_errorbar(aes(x=term, ymin=conf.low, ymax=conf.high, width=.1, colour='colour'), size=.8) +
                geom_point(aes(x=term, y=estimate, colour='colour'), shape=21, fill=theme$fill[1], size=3) +
                scale_colour_manual(name='', values=c(colour=theme$color[1]), labels=paste("", errorType)) +
                labs(x="Predictor", y="Standardized Estimate") +
                coord_flip() +
                ggtheme + themeSpec

            return(p)
        },
        .prepareEmmPlots = function(models, data) {

            covs <- self$options$covs
            factors <- self$options$factors
            dep <- self$options$dep

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
                            weights <- 'cells'

                        suppressMessages({
                            mm <- try(
                                emmeans::emmeans(model, formula, cov.reduce=FUN, options=list(level=self$options$ciWidthEmm / 100), weights = weights, data=data),
                                silent = TRUE
                            )

                            emmTable[[ j ]] <- try(
                                as.data.frame(summary(emmeans::emmeans(model, formula, cov.reduce=FUN2, options=list(level=self$options$ciWidthEmm / 100), weights = weights, data=data))),
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

                        names <- list('x'=termB64[1], 'y'='emmean', 'lines'=termB64[2], 'plots'=termB64[3], 'lower'='lower.CL', 'upper'='upper.CL')
                        names <- lapply(names, function(x) if (is.na(x)) NULL else x)

                        labels <- list('x'=term[1], 'y'=dep, 'lines'=term[2], 'plots'=term[3])
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

                    rContr[[term]] <- paste0(jmvcore::toB64(term), 1:length(contrLevels[[term]]))

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
        .cleanData = function() {

            dep <- self$options$dep
            covs <- self$options$covs
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
                stats::contrasts(data[[jmvcore::toB64(factor)]]) <- private$.createContrasts(levels)
            }

            for (cov in c(dep, covs))
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
        .createContrasts=function(levels) {

            nLevels <- length(levels)

            dummy <- contr.treatment(levels)
            dimnames(dummy) <- NULL
            coding <- matrix(rep(1/nLevels, prod(dim(dummy))), ncol=nLevels-1)
            contrast <- (dummy - coding)

            return(contrast)
        },
        .stdEst = function(model) {

            # From 'QuantPsyc' R package
            b <- summary(model)$coef[-1,1]
            sx <- sapply(model$model[-1], sd)
            sy <- sapply(model$model[1], sd)
            beta <- b * sx /  sy

            CI <- stats::confint(model, level = self$options$ciWidthStdEst / 100)[-1,]
            betaCI <- CI * sx / sy

            if (is.matrix(betaCI))
                r <- list(beta=beta, lower=betaCI[,1], upper=betaCI[,2])
            else
                r <- list(beta=beta, lower=betaCI[1], upper=betaCI[2])

            return(r)
        },
        .scaleData = function(data) {
            for (col in names(data)) {
                if ( ! is.factor(data[[col]]))
                    data[[col]] <- scale(data[[col]])
            }

            return(data)
        })
)
