
mancovaClass <- R6::R6Class(
    "mancovaClass",
    inherit = mancovaBase,
    private = list(
        #### Init + run functions ----
        .init = function() {

            modelTerms <- private$.modelTerms()
            private$.initMultivarTable(modelTerms)
            private$.initUnivarTables(modelTerms)

        },
        .run = function() {

            ready <- TRUE
            if (is.null(self$options$deps) || (is.null(self$options$factors) && is.null(self$options$covs)))
                ready <- FALSE

            if (ready) {

                data <- private$.cleanData()
                results <- private$.compute(data)

                private$.populateMultivarTable(results)
                private$.populateUnivarTable(results)
                private$.populateBoxMTable(results)
                private$.populateShapiroTable(results)
                private$.prepareQQPlot(data)

            }
        },

        #### Compute results ----
        .compute = function(data) {

            deps <- self$options$deps
            factors <- self$options$factors
            covs <- self$options$covs

            modelTerms <- jmvcore::composeTerms(private$.modelTerms(B64=TRUE))
            formula <- as.formula(paste(paste0("cbind(", paste0(jmvcore::toB64(deps), collapse=","), ")"), paste0(modelTerms, collapse ="+"), sep="~"))

            model <- stats::manova(formula, data=data)

            pillai <- summary(model, test = "Pillai")
            wilks <- summary(model, test = "Wilks")
            hotel <- summary(model, test = "Hotelling-Lawley")
            roy <- summary(model, test = "Roy")

            univar <- summary.aov(model)

            boxM <- private$.boxM(data)

            dataDeps <- t(as.matrix(data[jmvcore::toB64(deps)]))
            shapiro <- mvnormtest::mshapiro.test(dataDeps)

            return(list(pillai=pillai, wilks=wilks, hotel=hotel, roy=roy, univar=univar,
                        boxM=boxM, shapiro=shapiro))
        },

        #### Init tables/plots functions ----
        .initMultivarTable = function(modelTerms) {

            table <- self$results$multivar

            for (i in seq_along(modelTerms)) {

                term <- jmvcore::stringifyTerm(modelTerms[[i]])

                row <- list()
                row[["term[pillai]"]] <- term
                row[["term[wilks]"]] <- term
                row[["term[hotel]"]] <- term
                row[["term[roy]"]] <- term

                table$addRow(rowKey=term, values=row)

            }
        },
        .initUnivarTables = function(modelTerms) {

            deps <- self$options$deps

            table <- self$results$univar

            terms <- c(modelTerms, "Residuals")

            for (term in terms) {
                for (dep in deps) {

                    termString <- jmvcore::stringifyTerm(term)
                    key <- paste0(paste0(term, collapse=""), dep)
                    table$addRow(rowKey=key, values=list(term = termString, dep = dep))

                    if (dep == deps[1])
                        table$addFormat(rowKey=key, col=1, Cell.BEGIN_GROUP)
                }
            }
        },

        #### Populate tables/plots functions ----
        .populateMultivarTable = function(results) {

            table <- self$results$multivar

            pillai <- results$pillai
            wilks <- results$wilks
            hotel <- results$hotel
            roy <- results$roy

            modelTerms <- private$.modelTerms()
            modelTermsB64 <- private$.modelTerms(B64=TRUE)

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
        .populateUnivarTable = function(results) {

            deps <- self$options$deps
            depsB64 <- jmvcore::toB64(deps)

            modelTerms <- private$.modelTerms()
            modelTermsB64 <- private$.modelTerms(B64 = TRUE)
            terms <- c(modelTerms, "Residuals")
            termsB64 <- c(modelTermsB64, "Residuals")

            r <- results$univar

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
        .populateBoxMTable = function(results) {

            table <- self$results$assump$boxM
            boxM <- results$boxM

            table$setRow(rowNo=1, values=list(chi=boxM$chiSq, df=boxM$df, p=boxM$p))

            if ( ! is.null(boxM$warning)) {
                table$addFootnote(rowNo=1, 'chi', boxM$warning)
                table$addFootnote(rowNo=1, 'p', boxM$warning)
            }
        },
        .populateShapiroTable = function(results) {

            table <- self$results$assump$shapiro
            shapiro <- results$shapiro

            w <- as.numeric(shapiro$statistic)
            p <- as.numeric(shapiro$p.value)

            table$setRow(rowNo=1, values=list(w=w, p=p))

        },

        #### Plot functions ----
        .prepareQQPlot = function(data, model) {

            image <- self$results$assump$get('qqPlot')

            deps <- self$options$deps
            dataDeps <- as.matrix(data[jmvcore::toB64(deps)])

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

            print(ggplot(data=image$state, aes(x=chi, y=dist)) +
                      geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
                      geom_point(aes(x=chi,y=dist), size=2, colour=theme$color[1]) +
                      xlab("Chi-Square Quantiles") +
                      ylab("Squared Mahalanobis Distance") +
                      ggtheme)

            TRUE
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

            attr(data, 'row.names') <- seq_len(length(data[[1]]))
            attr(data, 'class') <- 'data.frame'
            data <- jmvcore::naOmit(data)

            return(data)
        },
        .modelTerms = function(B64 = FALSE) {

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

                modelTermsB64

            } else {

                modelTerms

            }
        },
        .boxM = function(data) {

            deps <- self$options$deps
            factors <- self$options$factors

            dataDeps <- data[jmvcore::toB64(deps)]
            dataFactors <- data[jmvcore::toB64(factors)]

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
                warning <- "Too few observations to calculate statistic. Each (sub)group must have at least as many observations as there are dependent variables."

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
        })
)
