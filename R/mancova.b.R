
#' @importFrom jmvcore .
mancovaClass <- R6::R6Class(
    "mancovaClass",
    inherit = mancovaBase,
    #### Active bindings ----
    active = list(
        dataProcessed = function() {
            if (is.null(private$.dataProcessed))
                private$.dataProcessed <- private$.cleanData()

            return(private$.dataProcessed)
        },
        model = function() {
            if (is.null(private$.model))
                private$.model <- private$.computeModel()

            return(private$.model)
        },
        pillai = function() {
            if (is.null(private$.pillai))
                private$.pillai <- summary(self$model, test="Pillai")

            return(private$.pillai)
        },
        wilks = function() {
            if (is.null(private$.wilks))
                private$.wilks <- summary(self$model, test="Wilks")

            return(private$.wilks)
        },
        hotel = function() {
            if (is.null(private$.hotel))
                private$.hotel <- summary(self$model, test="Hotelling-Lawley")

            return(private$.hotel)
        },
        roy = function() {
            if (is.null(private$.roy))
                private$.roy <- summary(self$model, test="Roy")

            return(private$.roy)
        },
        univar = function() {
            if (is.null(private$.univar))
                private$.univar <- summary.aov(self$model)

            return(private$.univar)
        },
        boxM = function() {
            if (is.null(private$.boxM))
                private$.boxM <- private$.computeBoxM()

            return(private$.boxM)
        },
        shapiro = function() {
            if (is.null(private$.shapiro))
                private$.shapiro <- private$.computeShapiro()

            return(private$.shapiro)
        }
    ),
    private = list(
        #### Member variables ----
        .dataProcessed = NULL,
        .model = NULL,
        .pillai = NULL,
        .wilks = NULL,
        .hotel = NULL,
        .roy = NULL,
        .univar = NULL,
        .boxM = NULL,
        .shapiro = NULL,
        .modelTerms = NULL,
        .modelTermsB64 = NULL,
        .modelFormula = NULL,

        #### Init + run functions ----
        .init = function() {
            private$.initMultivarTable()
            private$.initUnivarTables()
        },
        .run = function() {
            if (is.null(self$options$deps) || (is.null(self$options$factors) && is.null(self$options$covs)))
                return()

            private$.checkData()

            private$.populateMultivarTable()
            private$.populateUnivarTable()
            private$.populateBoxMTable()
            private$.populateShapiroTable()
            private$.prepareQQPlot()
        },

        #### Compute results ----
        .computeModel = function() {
            model <- stats::manova(private$.getFormula(), data=self$dataProcessed)
            return(model)
        },
        .computeBoxM = function() {
            deps <- self$options$deps
            factors <- self$options$factors

            dataDeps <- self$dataProcessed[jmvcore::toB64(deps)]

            if (length(factors) == 0) {
                return(
                    list(
                        chiSq = NaN,
                        df = NaN,
                        p = NaN,
                        warning = .("No factors defined. Box's M test is only relevant when model contains factors.")
                    )
                )
            }

            dataFactors <- self$dataProcessed[jmvcore::toB64(factors)]

            grouping <- apply(dataFactors, 1 , function(x) {
                if (any(is.na(x)))
                    return(NA)
                else
                    paste0(x, collapse = "")
            })

            grouping <- as.factor(as.vector(grouping))

            # from biotools package

            p <- ncol(dataDeps)
            nlev <- nlevels(grouping)
            lev <- levels(grouping)
            dfs <- tapply(grouping, grouping, length) - 1

            warning <- NULL
            if (any(dfs < p))
                warning <- .("Too few observations to calculate statistic. Each (sub)group must have at least as many observations as there are dependent variables.")

            mats <- list(); aux <- list()
            for(i in 1:nlev) {
                mats[[i]] <- cov(dataDeps[grouping == lev[i], ])
                aux[[i]] <- mats[[i]] * dfs[i]
            }

            names(mats) <- lev
            pooled <- Reduce("+", aux) / sum(dfs)
            logdet <- log(unlist(lapply(mats, det)))
            minus2logM <- sum(dfs) * log(det(pooled)) - sum(logdet * dfs)
            sum1 <- sum(1 / dfs)
            Co <- (((2 * p^2) + (3 * p) - 1) / (6 * (p + 1) * (nlev - 1))) * (sum1 - (1 / sum(dfs)))
            chiSq <- minus2logM * (1 - Co)
            df <- (choose(p, 2) + p) * (nlev - 1)
            pval <- pchisq(chiSq, df, lower.tail = FALSE)

            return(list(chiSq = chiSq, df = df, p = pval, warning = warning))
        },
        .computeShapiro = function() {
            deps <- self$options$deps
            dataDeps <- t(as.matrix(self$dataProcessed[jmvcore::toB64(deps)]))
            shapiro <- try(mvnormtest::mshapiro.test(dataDeps))

            return(shapiro)
        },

        #### Init tables/plots functions ----
        .initMultivarTable = function() {
            table <- self$results$multivar

            for (modelTerm in private$.getModelTerms()) {
                term <- jmvcore::stringifyTerm(modelTerm)

                row <- list()
                row[["term[pillai]"]] <- term
                row[["term[wilks]"]] <- term
                row[["term[hotel]"]] <- term
                row[["term[roy]"]] <- term

                table$addRow(rowKey=term, values=row)
            }
        },
        .initUnivarTables = function() {
            table <- self$results$univar

            deps <- self$options$deps
            terms <- c(private$.getModelTerms(), "Residuals")
            termTitles <- c(private$.getModelTerms(), .("Residuals"))

            for (i in seq_along(terms)) {
                for (dep in deps) {
                    termString <- jmvcore::stringifyTerm(termTitles[[i]])
                    key <- paste0(paste0(terms[[i]], collapse=""), dep)
                    table$addRow(rowKey=key, values=list(term = termString, dep = dep))

                    if (dep == deps[1])
                        table$addFormat(rowKey=key, col=1, Cell.BEGIN_GROUP)
                }
            }
        },

        #### Populate tables/plots functions ----
        .populateMultivarTable = function() {
            table <- self$results$multivar

            pillai <- self$pillai
            wilks <- self$wilks
            hotel <- self$hotel
            roy <- self$roy

            modelTerms <- private$.getModelTerms()
            modelTermsB64 <- private$.getModelTermsB64()

            for (i in seq_along(modelTerms)) {
                term <- jmvcore::stringifyTerm(modelTerms[[i]])
                termResult <- jmvcore::composeTerm(modelTermsB64[[i]])
                index <- which(pillai$row.names %in% termResult)

                row <- list()
                row[["stat[pillai]"]] <-  pillai$stats[index, 2]
                row[["f[pillai]"]] <-  pillai$stats[index, 3]
                row[["df1[pillai]"]] <-  pillai$stats[index, 4]
                row[["df2[pillai]"]] <-  pillai$stats[index, 5]
                row[["p[pillai]"]] <-  pillai$stats[index, 6]

                row[["stat[wilks]"]] <-  wilks$stats[index, 2]
                row[["f[wilks]"]] <-  wilks$stats[index, 3]
                row[["df1[wilks]"]] <-  wilks$stats[index, 4]
                row[["df2[wilks]"]] <-  wilks$stats[index, 5]
                row[["p[wilks]"]] <-  wilks$stats[index, 6]

                row[["stat[hotel]"]] <-  hotel$stats[index, 2]
                row[["f[hotel]"]] <-  hotel$stats[index, 3]
                row[["df1[hotel]"]] <-  hotel$stats[index, 4]
                row[["df2[hotel]"]] <-  hotel$stats[index, 5]
                row[["p[hotel]"]] <-  hotel$stats[index, 6]

                row[["stat[roy]"]] <-  roy$stats[index, 2]
                row[["f[roy]"]] <-  roy$stats[index, 3]
                row[["df1[roy]"]] <-  roy$stats[index, 4]
                row[["df2[roy]"]] <-  roy$stats[index, 5]
                row[["p[roy]"]] <-  roy$stats[index, 6]

                table$setRow(rowKey=term, values=row)
            }
        },
        .populateUnivarTable = function() {
            deps <- self$options$deps
            depsB64 <- jmvcore::toB64(deps)

            modelTerms <- private$.getModelTerms()
            modelTermsB64 <- private$.getModelTermsB64()
            terms <- c(modelTerms, "Residuals")
            termsB64 <- c(modelTermsB64, "Residuals")

            r <- self$univar

            table <- self$results$univar

            termsResult <- gsub(" ", "", rownames(r[[1]]), fixed = TRUE)

            for (i in seq_along(terms)) {
                index <- which(termsResult %in% jmvcore::composeTerm(termsB64[[i]]))

                for (j in seq_along(deps)) {
                    key <- paste0(paste0(terms[[i]], collapse=""), deps[[j]])

                    row <- list()
                    row[["ss"]] <-  r[[j]][index,2]
                    row[["df"]] <-  r[[j]][index,1]
                    row[["ms"]] <-  r[[j]][index,3]
                    row[["F"]] <-  if (is.na(r[[j]][index,4])) "" else r[[j]][index,4]
                    row[["p"]] <-  if (is.na(r[[j]][index,5])) "" else r[[j]][index,5]

                    table$setRow(rowKey=key, values=row)
                }
            }
        },
        .populateBoxMTable = function() {
            table <- self$results$assump$boxM
            boxM <- self$boxM

            table$setRow(rowNo=1, values=list(chi=boxM$chiSq, df=boxM$df, p=boxM$p))

            if ( ! is.null(boxM$warning)) {
                table$addFootnote(rowNo=1, 'chi', boxM$warning)
                table$addFootnote(rowNo=1, 'p', boxM$warning)
            }
        },
        .populateShapiroTable = function() {
            table <- self$results$assump$shapiro
            shapiro <- self$shapiro

            if (jmvcore::isError(shapiro)) {
                fn <- .('Not available due to too large number of cases (> 5000).')

                table$setRow(rowNo=1, values=list(w=NaN, p=NaN))
                table$addFootnote(rowNo=1, col='w', note=fn)
                table$addFootnote(rowNo=1, col='p', note=fn)
            } else {
                table$setRow(rowNo=1,
                             values=list(w=as.numeric(shapiro$statistic),
                                         p=as.numeric(shapiro$p.value)))
            }
        },

        #### Plot functions ----
        .prepareQQPlot = function() {
            image <- self$results$assump$get('qqPlot')

            deps <- self$options$deps
            dataDeps <- as.matrix(self$dataProcessed[jmvcore::toB64(deps)])

            center <- colMeans(dataDeps)
            nSubjs <- nrow(dataDeps)
            nVars <- ncol(dataDeps)
            cov <- cov(dataDeps)

            dist <- sort(stats::mahalanobis(dataDeps, center, cov))
            chi <- stats::qchisq(stats::ppoints(nSubjs), df=nVars)

            df <- data.frame(dist=dist, chi=chi)

            image$setState(df)
        },
        .qqPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            p <- ggplot(data=image$state, aes(x=chi, y=dist)) +
                      geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
                      geom_point(aes(x=chi,y=dist), size=2, colour=theme$color[1]) +
                      xlab(.("Chi-Square Quantiles")) +
                      ylab(.("Squared Mahalanobis Distance")) +
                      ggtheme

            return(p)
        },

        #### Helper functions ----
        .cleanData = function() {
            deps <- self$options$deps
            factors <- self$options$factors
            covs <- self$options$covs

            data <- list()
            for (var in c(deps, covs))
                data[[jmvcore::toB64(var)]] <- jmvcore::toNumeric(self$data[[var]])

            for (var in factors)
                data[[jmvcore::toB64(var)]] <- as.factor(self$data[[var]])

            attr(data, 'row.names') <- rownames(self$data)
            attr(data, 'class') <- 'data.frame'
            data <- jmvcore::naOmit(data)

            return(data)
        },
        .checkData = function() {
            if (nrow(self$dataProcessed) == 0) {
                jmvcore::reject(
                    .("The dataset contains 0 rows (after removing rows with missing values)"),
                    code=exceptions$dataError
                )
            }
        },
        .constructModelTerms = function(B64 = FALSE) {
            factors <- self$options$factors
            covs <- self$options$covs

            if (length(factors) > 1) {
                formula <- as.formula(paste('~', paste(paste0('`', factors, '`'), collapse='*')))
                terms   <- attr(stats::terms(formula), 'term.labels')
                modelTerms <- sapply(terms, function(x) as.list(strsplit(x, ':')), USE.NAMES=FALSE)
            } else {
                modelTerms <- as.list(factors)
            }

            if (! is.null(covs)) {
                for (cov in covs)
                    modelTerms[[length(modelTerms) + 1]] <- cov
            }

            for (i in seq_along(modelTerms)) {
                term <- modelTerms[[i]]
                quoted <- grepl('^`.*`$', term)
                term[quoted] <- substring(term[quoted], 2, nchar(term[quoted])-1)
                modelTerms[[i]] <- term
            }

            if (B64) {
                modelTermsB64 <- list()
                for (i in seq_along(modelTerms))
                    modelTermsB64[[i]] <- jmvcore::toB64(modelTerms[[i]])

                modelTerms = modelTermsB64
            }

            return(modelTerms)
        },
        .getModelTerms = function() {
            if (is.null(private$.modelTerms))
                private$.modelTerms <- private$.constructModelTerms()

            return(private$.modelTerms)
        },
        .getModelTermsB64 = function() {
            if (is.null(private$.modelTermsB64))
                private$.modelTermsB64 <- private$.constructModelTerms(B64=TRUE)

            return(private$.modelTermsB64)
        },
        .getFormula = function() {
            if (is.null(private$.modelFormula)) {
                deps <- self$options$deps

                modelTerms <- jmvcore::composeTerms(private$.getModelTermsB64())
                formula <- as.formula(paste(paste0("cbind(", paste0(jmvcore::toB64(deps), collapse=","), ")"), paste0(modelTerms, collapse ="+"), sep="~"))

                private$.modelFormula <- formula
            }

            return(private$.modelFormula)
        })
)
