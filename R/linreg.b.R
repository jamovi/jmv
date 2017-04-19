
linRegClass <- R6::R6Class(
    "linRegClass",
    inherit = linRegBase,
    private = list(
        #### Member variables ----
        modelSelected = NULL,
        modelTerms = NULL,

        #### Init + run functions ----
        .init = function() {

            if (self$options$modelSelected < 0 || (self$options$modelSelected + 1) > length(self$options$blocks))
                private$modelSelected <- length(self$options$blocks)
            else
                private$modelSelected <- self$options$modelSelected + 1

            private$modelTerms <- private$.modelTerms()

            private$.defineModelTitle()

            private$.initCoefTable()
            private$.initModelFitTable()
            private$.initModelCompTable()
            private$.initDescTable()
            private$.initCollinearityTable()
            private$.initResPlots()

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
                private$.populateCoefTable(results)

                private$.populateDescTable(data)
                private$.populateCooksTable(results)
                private$.populateCollinearityTable(results)
                private$.populateDurbinWatsonTable(results)
                private$.prepareQQPlot(data, results$models[[private$modelSelected]])
                private$.prepareCoefPlot(results)
                private$.prepareResPlots(data, results$models[[private$modelSelected]])

            }
        },

        #### Compute results ----
        .compute = function(data) {

            dep <- self$options$dep
            modelTerms <- lapply(private$.modelTerms()$modelTerms, function(x) return(x$terms))

            models <- list(); modelsBF <- list()
            for (i in seq_along(modelTerms)) {

                formula <- as.formula(paste(jmvcore::toB64(dep), paste0(modelTerms[[i]], collapse ="+"), sep="~"))
                modelsBF[[i]] <- BayesFactor::lmBF(formula, data=data)
                models[[i]] <- lm(formula, data=data)

            }

            ANOVA <- do.call(stats::anova, models)

            AIC <- list(); BIC <- list(); betas <- list(); CI <- list();
            dwTest <- list(); VIF <- list(); cooks <- list()
            for (i in seq_along(models)) {

                AIC[[i]] <- stats::AIC(models[[i]])
                BIC[[i]] <- stats::BIC(models[[i]])
                betas[[i]] <- private$.stdEst(models[[i]])
                CI[[i]] <- stats::confint(models[[i]], level = self$options$ciWidth / 100)
                dwTest[[i]] <- car::durbinWatsonTest(models[[i]])
                cooks[[i]] <- stats::cooks.distance(models[[i]])

                if (length(models[[i]]$coefficients) > 2)
                    VIF[[i]] <- car::vif(models[[i]])
                else
                    VIF[[i]] <- NULL

            }

            return(list(models=models, modelsBF=modelsBF, ANOVA=ANOVA, AIC=AIC, BIC=BIC,
                        betas=betas, CI=CI, dwTest=dwTest, VIF=VIF, cooks=cooks))
        },

        #### Init tables/plots functions ----
        .initModelFitTable = function() {

            table <- self$results$modelFit
            modelTerms <- private$modelTerms$modelTermsLabels
            models <- sapply(modelTerms, function(x) return(x$model))

            for (i in seq_along(models))
                table$addRow(rowKey=i, values=list(model = models[i]))

            if (length(models) > 1) {

                if (self$options$modelSelected == -1)
                    message <- jmvcore::format('By default, model specific output is only shown for the most complex model, model {}. Select a different row to use a different model.', length(models))
                else
                    message <- jmvcore::format('Model specific output is only shown for model {}', self$options$modelSelected + 1)

                table$setNote("note", message)
            }
        },
        .initModelCompTable = function() {

            table <- self$results$modelComp
            modelTerms <- private$modelTerms$modelTermsLabels

            if (length(modelTerms) <= 1) {
                table$setVisible(visible = FALSE)
                return()
            }

            models <- sapply(modelTerms, function(x) return(x$model))

            for (i in 1:(length(models)-1))
                table$addRow(rowKey=i, values=list(model1 = models[i], model2 = models[i+1]))

        },
        .initCoefTable = function() {

            table <- self$results$coef
            modelTerms <- private$modelTerms$modelTermsLabels
            rowNo <- 1

            ciWidth <- self$options$ciWidth
            table$getColumn('lower')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
            table$getColumn('upper')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))

            for (i in seq_along(modelTerms)) {
                for (j in seq_along(modelTerms[[i]][["terms"]])) {

                    row <- list("terms"=modelTerms[[i]][["terms"]][j], "model"=modelTerms[[i]][["model"]])
                    table$addRow(rowKey=rowNo, values=row)

                    if (j == 1)
                        table$addFormat(rowNo=rowNo, col=1, Cell.BEGIN_GROUP)

                    rowNo <- rowNo + 1
                }
            }
        },
        .initDescTable = function() {

            table <- self$results$dataSummary$desc
            modelTerms <- private$modelTerms$modelTermsLabels

            if (length(modelTerms) < 1)
                terms <- ''
            else
                terms <- c(self$options$dep, modelTerms[[private$modelSelected]][["terms"]][-1])

            for (i in seq_along(terms))
                table$addRow(rowKey=i, values=list(name = terms[i]))

        },
        .initCollinearityTable = function() {

            table <- self$results$assump$collinearity
            modelTerms <- private$modelTerms$modelTermsLabels

            if (length(modelTerms) < 1)
                terms <- ''
            else
                terms <- modelTerms[[private$modelSelected]][["terms"]][-1]

            for (i in seq_along(terms))
                table$addRow(rowKey=i, values=list(term = terms[i]))

        },
        .initResPlots=function() {

            modelTerms <- private$modelTerms$modelTermsLabels

            if (length(modelTerms) < 1)
                terms <- ''
            else
                terms <- c(self$options$dep, modelTerms[[private$modelSelected]][["terms"]][-1])

            plots <- self$results$assump$resPlots

            for (term in terms)
                plots$addItem(term)

        },

        #### Populate tables/plots functions ----
        .populateModelFitTable = function(results) {

            table <- self$results$modelFit

            models <- results$models
            modelsBF <- results$modelsBF
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
                row[["bf"]] <- exp(modelsBF[[i]]@bayesFactor$bf)
                row[["err"]] <- modelsBF[[i]]@bayesFactor$error

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
            modelsBF <- results$modelsBF
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

                BF <- modelsBF[[i+1]]/modelsBF[[i]]

                row[["bf"]] <- exp(BF@bayesFactor$bf)
                row[["err"]] <- BF@bayesFactor$error

                table$setRow(rowNo=i, values = row)
            }
        },
        .populateCoefTable = function(results) {

            table <- self$results$coef

            models <- results$models
            modelTerms <- lapply(private$.modelTerms()$modelTerms, function(x) return(x$terms))

            rowNo <- 1
            for (i in seq_along(modelTerms)) {

                coef <- summary(models[[i]])$coef
                CI <- results$CI[[i]]
                betas <- results$betas[[i]]$beta

                modelTermsTemp <- modelTerms[[i]]
                modelTermsTemp[1] <- "(Intercept)"

                for (j in seq_along(modelTermsTemp)) {

                    index <- which(rownames(coef) == modelTermsTemp[j])

                    row <- list()
                    row[["est"]] <- coef[index, 1]
                    row[["se"]] <- coef[index, 2]
                    row[["t"]] <- coef[index, 3]
                    row[["p"]] <- coef[index, 4]
                    row[["lower"]] <- CI[index, 1]
                    row[["upper"]] <- CI[index, 2]

                    if (modelTermsTemp[j] == "(Intercept)")
                        row[["stdEst"]] <- ""
                    else
                        row[["stdEst"]] <- betas[modelTermsTemp[j]]

                    table$setRow(rowNo=rowNo, values=row)

                    rowNo <- rowNo + 1
                }
            }
        },
        .populateDescTable = function(data) {

            table <- self$results$dataSummary$desc
            modelTerms <- private$modelTerms$modelTermsLabels
            terms <- jmvcore::toB64(c(self$options$dep, modelTerms[[private$modelSelected]][["terms"]][-1]))

            for (i in seq_along(terms)) {

                column <- data[[terms[i]]]

                row <- list()
                row[['num']] <- length(column)
                row[['mean']] <- mean(column)
                row[['median']] <- median(column)
                row[['sd']] <- sd(column)
                row[['se']] <- sd(column)/sqrt(length(column))

                table$setRow(rowKey=i, values=row)
            }
        },
        .populateCooksTable = function(results) {

            table <- self$results$dataSummary$cooks
            cooks <- results$cooks[[private$modelSelected]]

            row <- list()
            row[['mean']] <- mean(cooks)
            row[['median']] <- median(cooks)
            row[['sd']] <- sd(cooks)
            row[['min']] <- min(cooks)
            row[['max']] <- max(cooks)

            table$setRow(rowNo=1, values=row)

        },
        .populateDurbinWatsonTable = function(results) {

            table <- self$results$assump$durbinWatson
            dwTest <- results$dwTest

            row <- list()
            row[["autoCor"]] <- as.numeric(dwTest[[private$modelSelected]][1])
            row[["dw"]] <- as.numeric(dwTest[[private$modelSelected]][2])
            row[["p"]] <- as.numeric(dwTest[[private$modelSelected]][3])

            table$setRow(rowNo=1, values=row)

        },
        .populateCollinearityTable = function(results) {

            table <- self$results$assump$collinearity

            modelTerms <- private$modelTerms$modelTermsLabels
            terms <- jmvcore::toB64(modelTerms[[private$modelSelected]][["terms"]][-1])

            if (length(results$VIF) == 0)
                VIF <- NULL
            else
                VIF <- results$VIF[[private$modelSelected]]

            for (i in seq_along(terms)) {

                row <- list()

                if (length(terms) <= 1) {

                    row[["tol"]] <- 1
                    row[["vif"]] <- 1

                } else {

                    row[["tol"]] <- 1 / as.numeric(VIF[terms[i]])
                    row[["vif"]] <- as.numeric(VIF[terms[i]])
                }

                table$setRow(rowNo=i, values=row)
            }
        },
        .prepareQQPlot = function(data, model) {

            image <- self$results$assump$get('qqPlot')

            df <- as.data.frame(qqnorm(scale(model$residuals), plot.it=FALSE))

            image$setState(df)

        },
        .qqPlot = function(image, ...) {

            if (is.null(image$state))
                return(FALSE)

            the <- theme(
                text=element_text(size=16, colour='#333333'),
                plot.background=element_rect(fill='transparent', color=NA),
                panel.background=element_rect(fill='#E8E8E8'),
                plot.margin=margin(15, 15, 15, 15),
                axis.text.x=element_text(margin=margin(5,0,0,0)),
                axis.text.y=element_text(margin=margin(0,5,0,0)),
                axis.title.x=element_text(margin=margin(10,0,0,0)),
                axis.title.y=element_text(margin=margin(0,10,0,0)),
                plot.title=element_text(margin=margin(0, 0, 15, 0)))

            print(ggplot(data=image$state, aes(x=x, y=y)) +
                      geom_point(aes(x=x,y=y), colour='#333333') +
                      geom_abline(slope=1, intercept=0, colour='#333333') +
                      xlab("Theoretical Quantiles") +
                      ylab("Standardized Residuals") +
                      the)

            TRUE
        },
        .prepareResPlots = function(data, model) {

            res <- model$residuals

            images <- self$results$assump$resPlots
            for (term in images$itemKeys) {
                x <- data[[jmvcore::toB64(term)]]
                df <- data.frame(y=res, x=x)

                image <- images$get(key=term)
                image$setState(list(df=df, xlab=term))
            }
        },
        .resPlot = function(image, ...) {

            if (is.null(image$state))
                return(FALSE)

            the <- theme(
                text=element_text(size=16, colour='#333333'),
                plot.background=element_rect(fill='transparent', color=NA),
                panel.background=element_rect(fill='#E8E8E8'),
                plot.margin=margin(15, 15, 15, 15),
                axis.text.x=element_text(margin=margin(5,0,0,0)),
                axis.text.y=element_text(margin=margin(0,5,0,0)),
                axis.title.x=element_text(margin=margin(10,0,0,0)),
                axis.title.y=element_text(margin=margin(0,10,0,0)),
                plot.title=element_text(margin=margin(0, 0, 15, 0)))

            print(ggplot(data=image$state$df, aes(y=y, x=x)) +
                      geom_point(aes(x=x,y=y), colour='#333333') +
                      xlab(image$state$xlab) +
                      ylab("Residuals") +
                      the)

            TRUE
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
        .coefPlot = function(image, ...) {

            if (is.null(image$state))
                return(FALSE)

            the <- theme(
                text=element_text(size=16, colour='#333333'),
                plot.background=element_rect(fill='transparent', color=NA),
                panel.background=element_rect(fill='#E8E8E8'),
                plot.margin=margin(15, 15, 15, 15),
                axis.text.x=element_text(margin=margin(5,0,0,0)),
                axis.text.y=element_text(margin=margin(0,5,0,0)),
                axis.title.x=element_text(margin=margin(10,0,0,0)),
                axis.title.y=element_text(margin=margin(0,10,0,0)),
                plot.title=element_text(margin=margin(0, 0, 15, 0)),
                legend.position = 'right',
                legend.background = element_rect("transparent"),
                legend.title = element_blank(),
                legend.key = element_blank(),
                legend.text = element_text(size=16, colour='#333333'))

            errorType <- paste0(self$options$ciWidth, '% CI')

            p <- ggplot(data=image$state) +
                geom_hline(yintercept=0, linetype="dotted", colour='#333333', size=1.2) +
                geom_errorbar(aes(x=term, ymin=conf.low, ymax=conf.high, width=.1, colour='colour'), size=.8) +
                geom_point(aes(x=term, y=estimate, colour='colour'), shape=21, fill='white', size=3) +
                scale_colour_manual(name='', values=c(colour='#333333'), labels=paste("", errorType)) +
                labs(x="Predictor", y="Standardized Estimate") +
                coord_flip() +
                the

            print(p)

            TRUE
        },

        #### Helper functions ----
        .modelTerms = function() {

            blocks <- self$options$blocks
            modelNo <- seq_along(blocks)

            terms <- list(); termsLabels <- list()
            for (i in seq_along(blocks)) {
                terms[[length(terms) + 1]] <- list(model=as.character(modelNo[i]), terms=c('1', jmvcore::toB64(unlist(blocks[1:i]))))
                termsLabels[[length(termsLabels) + 1]] <- list(model=as.character(modelNo[i]), terms=c('Intercept', unlist(blocks[1:i])))
            }

            return(list(modelTerms = terms, modelTermsLabels = termsLabels))
        },
        .cleanData = function() {

            dep <- self$options$dep
            covs <- unlist(self$options$blocks)

            data <- list()
            for (var in c(dep, covs))
                data[[jmvcore::toB64(var)]] <- jmvcore::toNumeric(self$data[[var]])

            attr(data, 'row.names') <- seq_len(length(data[[1]]))
            attr(data, 'class') <- 'data.frame'
            data <- jmvcore::naOmit(data)

            return(data)
        },
        .defineModelTitle = function() {

            blocks <- self$options$blocks

            if (length(blocks) > 1) {

                model <- private$modelSelected

                self$results$assump$setTitle(jmvcore::format('Assumption Checks \u2013 Model {}', model))
                self$results$dataSummary$setTitle(jmvcore::format('Data Summary \u2013 Model {}', model))
                self$results$coefPlot$setTitle(jmvcore::format('Coefficient Plot \u2013 Model {}', model))

            }
        },
        .stdEst = function(model) {

            # From 'QuantPsyc' R package
            b <- summary(model)$coef[-1,1]
            sx <- sapply(model$model[-1], sd)
            sy <- sapply(model$model[1], sd)
            beta <- b * sx /  sy

            CI <- stats::confint(model, level = self$options$ciWidth / 100)[-1,]
            betaCI <- CI * sx / sy

            if (is.matrix(betaCI))
                r <- list(beta=beta, lower=betaCI[,1], upper=betaCI[,2])
            else
                r <- list(beta=beta, lower=betaCI[1], upper=betaCI[2])

            return(r)
        })
)
