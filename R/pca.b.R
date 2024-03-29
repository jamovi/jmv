
#' @importFrom jmvcore .
pcaClass <- R6::R6Class(
    "pcaClass",
    inherit = pcaBase,
    #### Active bindings ----
    active = list(
        dataProcessed = function() {
            if (is.null(private$.dataProcessed))
                private$.dataProcessed <- private$.cleanData()

            return(private$.dataProcessed)
        },
        nFactors = function() {
            if (is.null(private$.nFactors))
                private$.nFactors <- private$.computeNFactors()

            return(private$.nFactors)
        },
        eigen = function() {
            if (is.null(private$.eigen))
                private$.eigen <- private$.computeEigen()

            return(private$.eigen)
        },
        loadings = function() {
            if (is.null(private$.loadings))
                private$.loadings <- private$.getPsychResult()$loadings

            return(private$.loadings)
        },
        uniqueness = function() {
            if (is.null(private$.uniqueness))
                private$.uniqueness <- private$.getPsychResult()$uniqueness

            return(private$.uniqueness)
        },
        factorCor = function() {
            if (is.null(private$.factorCor))
                private$.factorCor <- private$.getPsychResult()$Phi

            return(private$.factorCor)
        },
        SS = function() {
            if (is.null(private$.SS)) {
                devnull <- '/dev/null'
                if (Sys.info()['sysname'] == 'Windows')
                    devnull <- 'nul'

                sink(file=devnull)
                SS <- print(private$.getPsychResult())$Vaccounted
                sink()

                private$.SS <- SS
            }

            return(private$.SS)
        },
        modelFit = function() {
            if (is.null(private$.modelFit) && private$analysis == 'efa') {
                r <- private$.getPsychResult()
                private$.modelFit <- list(
                    'tli'=r$TLI, 'bic'=r$BIC, 'rmsea'=r$RMSEA,
                    'chi'=r$STATISTIC, 'df'=r$dof, 'p'=r$PVAL
                )
            }

            return(private$.modelFit)
        },
        kmo = function() {
            if (is.null(private$.kmo))
                private$.kmo <- private$.computeKmo()

            return(private$.kmo)
        },
        bartlett = function() {
            if (is.null(private$.bartlett))
                private$.bartlett <- private$.computeBartlett()

            return(private$.bartlett)
        },
        scores = function() {
            if (is.null(private$.scores))
                private$.scores <- private$.computeFactorScores()

            return(private$.scores)
        }
    ),
    private = list(
        #### Member variables ----
        analysis = 'pca',
        .dataProcessed = NULL,
        .dataRowNums = NULL,
        .nFactors = NULL,
        .eigen = NULL,
        .simEigenCI = NULL,
        .corMatrix = NULL,
        .psychResult = NULL,
        .loadings = NULL,
        .uniqueness = NULL,
        .factorCor = NULL,
        .SS = NULL,
        .modelFit = NULL,
        .kmo = NULL,
        .bartlett = NULL,
        .scores = NULL,

        #### Init + run functions ----
        .init = function() {
            private$.initLoadingsTable()
            private$.initModelFitTable()
            private$.initEigenTable()
            private$.initKMOTable()
            private$.initFactorCor()
        },
        .run = function() {
            if (is.null(self$options$vars) || length(self$options$vars) < 2)
                return()

            private$.errorCheck()

            private$.populateLoadingsTable()
            private$.populateEigenTable()
            private$.populateFactorSummaryTable()
            private$.populateFactorCorTable()
            private$.populateModelFitTable()
            private$.populateKMOTable()
            private$.populateBartlettTable()
            private$.prepareScreePlot()
            private$.populateOutputs()
        },

        #### Compute results ----
        .computeNFactors = function() {
            method <- self$options$nFactorMethod

            if (method == "parallel") {
                nFactors <- private$.parallel(data)
            } else if (method == "eigen") {
                nFactors <- sum(self$eigen > self$options$minEigen)

                if (nFactors <= 0) {
                    jmvcore::reject(
                        jmvcore::format(
                            .('No components have an eigenvalue greater than {value}'),
                            value=self$options$minEigen
                        ),
                        code=''
                    )
                }
            } else {
                nFactors <- self$options$nFactors
            }

            return(nFactors)
        },
        .computeEigen = function() {
            # Eigenvalues for PCA and EFA
            if (private$analysis == 'pca')
                eigen <- eigen(private$.getCorMatrix())$values
            else
                eigen<- psych::fa(private$.getCorMatrix(), fm=self$options$extraction,
                                  warnings=FALSE)$values

            return(eigen)
        },
        .computeKmo = function() {
            suppressWarnings({
                kmo <- psych::KMO(private$.getCorMatrix())
            })

            return(kmo)
        },
        .computeBartlett = function() {
            suppressWarnings({
                N <- nrow(self$dataProcessed)
                bartlett <- psych::cortest.bartlett(private$.getCorMatrix(), n = N)
            })

            return(bartlett)
        },
        .computeFactorScores = function() {
            if (private$analysis == 'pca') {
                scores <- private$.getPsychResult()$scores
            } else {
                scores <- psych::factor.scores(
                    self$dataProcessed,
                    private$.getPsychResult(),
                    method = self$options$factorScoreMethod
                )$scores
            }

            return(scores)
        },

        #### Init tables/plots functions ----
        .initLoadingsTable = function() {
            table <- self$results$loadings

            rotation <- self$options$rotation
            if (rotation == 'varimax')
                rotationName <- .("varimax")
            else if (rotation == 'quartimax')
                rotationName <- .("quartimax")
            else if (rotation == 'promax')
                rotationName <- .("promax")
            else if (rotation == 'oblimin')
                rotationName <- .("oblimin")
            else if (rotation == 'simplimax')
                rotationName <- .("simplimax")
            else
                rotationName <- .("none")

            if (private$analysis == 'pca') {
                table$setNote(
                    "note",
                    jmvcore::format(
                        .("'{rotation}' rotation was used"),
                        rotation=rotationName
                    )
                )
            } else {
                extr <- self$options$extraction
                if (extr == 'pa')
                    extrName <- .('Principal axis factoring')
                else if (extr == 'ml')
                    extrName <- .('Maximum likelihood')
                else
                    extrName <- .('Minimum residual')

                table$setNote(
                    "note",
                    jmvcore::format(
                        .("'{method}' extraction method was used in combination with a '{rotation}' rotation"),
                        method=extrName,
                        rotation=rotationName
                    )
                )
            }
        },
        .initModelFitTable = function() {
            if (private$analysis == 'efa' && self$options$modelFit) {
                table <- self$results$modelFit$fit
                table$setVisible(TRUE)
            }
        },
        .initEigenTable = function() {
            table <- self$results$eigen$initEigen

            for (i in seq_along(self$options$vars))
                table$addRow(rowKey=i, values=list(comp = as.character(i)))
        },
        .initKMOTable = function() {
            table <- self$results$assump$kmo
            vars <- self$options$vars

            table$addRow(rowKey=1, values=list(name = .("Overall")))
            table$addFormat(rowKey=1, col=1, Cell.END_GROUP)

            for (i in seq_along(vars))
                table$addRow(rowKey=i+1, values=list(name = vars[[i]]))
        },
        .initFactorCor = function() {
            if (private$analysis == 'efa') {
                table <- self$results$factorStats$factorCor
                table$setTitle(.("Inter-Factor Correlations"))
            }
        },

        #### Populate tables/plots functions ----
        .populateLoadingsTable = function() {
            table <- self$results$loadings
            loadings <- self$loadings
            uniqueness <- self$uniqueness
            nFactors <- self$nFactors

            vars <- self$options$vars
            hide <- self$options$hideLoadings

            if (private$analysis == 'pca')
                type <- .('Component')
            else
                type <- .('Factor [specific factor]')

            if (nFactors > 1) {
                for (i in 2:nFactors) {
                    table$addColumn(
                        name = paste0("pc",i),
                        title = as.character(i),
                        type = 'number',
                        superTitle = jmvcore::format('{}', type),
                        index = i+1
                    )
                }
            }

            class(loadings) <- "matrix"
            loadings <- as.data.frame(loadings)

            if (self$options$sortLoadings) {
                absLoadings <- abs(loadings)
                max <- apply(absLoadings, 1, max)
                whichMax <- apply(absLoadings, 1, which.max)
                loadings$uniqueness <- uniqueness
                loadings <- loadings[order(whichMax, -max), ]
            } else {
                varOrder <- match(jmvcore::toB64(vars), rownames(loadings))
                loadings$uniqueness <- uniqueness
                loadings <- loadings[order(varOrder), ]
            }

            rowNames <- jmvcore::fromB64(rownames(loadings))

            for (i in seq_along(vars)) {
                row <- list()

                row[['name']] <- rowNames[i]
                for (j in 1:nFactors) {
                    l <- loadings[i, j]
                    row[[paste0("pc", j)]] <- if (abs(l) < hide) "" else l
                }
                row[["uniq"]] <- as.numeric(loadings$uniqueness[i])

                table$setRow(rowNo=i, values=row)
            }
        },
        .populateEigenTable = function() {

            table <- self$results$eigen$initEigen
            eigen <- self$eigen

            eigenTotal <- sum(abs(eigen))
            varProp <- (abs(eigen) / eigenTotal) * 100
            varCum <- cumsum(varProp)

            for (i in seq_along(eigen)) {
                row <- list()
                row[["eigen"]] <- eigen[i]

                if (private$analysis == 'pca') {
                    row[["varProp"]] <- varProp[i]
                    row[["varCum"]] <- varCum[i]
                }

                table$setRow(rowNo=i, values=row)
            }
        },
        .populateFactorSummaryTable = function() {
            table <- self$results$factorStats$factorSummary

            SS <- self$SS
            nFactors <- self$nFactors

            loadings <- SS[1,]
            varProp <- SS[2,]
            varCum <- if (dim(SS)[1] <= 2) SS[2,] else SS[3,]

            for (i in 1:nFactors) {
                row <- list()

                row[["comp"]] <- as.character(i)
                row[["loadings"]] <- loadings[i]
                row[["varProp"]] <- varProp[i] * 100
                row[["varCum"]] <- varCum[i] * 100

                table$addRow(rowKey=i, values=row)
            }
        },
        .populateFactorCorTable = function() {
            table <- self$results$factorStats$factorCor
            nFactors <- self$nFactors
            factorCor <- self$factorCor

            vars <- self$options$vars

            if (nFactors > 1) {
                for (i in 2:nFactors)
                    table$addColumn(name=paste0("pc",i), title=as.character(i), type='number')
            }

            for (i in 1:nFactors) {
                row <- list()
                row[["comp"]] <- i

                for (j in 1:nFactors) {
                    if (i == j)
                        row[[paste0("pc", j)]] <- "\u2014"
                    else if (j < i)
                        row[[paste0("pc", j)]] <- ""
                    else
                        row[[paste0("pc", j)]] <- ifelse(is.null(factorCor), 0, factorCor[i, j])
                }

                table$addRow(rowKey=i, values=row)
            }
        },
        .populateModelFitTable = function() {
            if (private$analysis == 'efa') {
                r <- self$modelFit

                table <- self$results$modelFit$fit

                row <- list()
                row['tli'] <- r$tli
                row['bic'] <- r$bic
                row['rmsea'] <- r$rmsea[1]
                row['rmseaLower'] <- r$rmsea[2]
                row['rmseaUpper'] <- r$rmsea[3]
                row['chi'] <- r$chi
                row['df'] <- r$df
                row['p'] <- r$p

                table$setRow(rowNo=1, values=row)
            }
        },
        .populateKMOTable = function() {
            if (! self$options$kmo)
                return()

            table <- self$results$assump$kmo
            vars <- self$options$vars
            kmo <- self$kmo

            table$setRow(rowNo=1, values=list(msa=kmo$MSA))

            for (i in seq_along(vars)) {
                msa <- kmo$MSAi[ jmvcore::toB64(vars[[i]]) ]
                table$setRow(rowNo=i+1, values=list(msa = msa))
            }
        },
        .populateBartlettTable = function() {
            if (! self$options$bartlett)
                return()

            table <- self$results$assump$bartlett
            r <- self$bartlett

            table$setRow(rowNo=1, values=list(chi=r$chisq, df=r$df, p=r$p.value))
        },
        .populateOutputs = function(n) {
            if (self$options$factorScoresOV && self$results$factorScoresOV$isNotFilled()) {
                keys <- 1:self$nFactors
                measureTypes <- rep("continuous", self$nFactors)

                if (private$analysis == 'pca') {
                    titles <- paste(.("Score Component"), keys)
                    descriptions <- paste(.("Score for component"), keys)
                } else {
                    titles <- paste(.("Score Factor"), keys)
                    descriptions <- character(length(keys))
                    for (i in keys) {
                        descriptions[i] = jmvcore::format(
                            .("Score for factor {i}. Estimated using the '{fsMethod}' method."),
                            i=i,
                            fsMethod=self$options$factorScoreMethod
                        )
                    }
                }

                self$results$factorScoresOV$set(
                    keys=keys,
                    titles=titles,
                    descriptions=descriptions,
                    measureTypes=measureTypes
                )

                self$results$factorScoresOV$setRowNums(private$.getDataRowNums())

                for (i in 1:self$nFactors) {
                    scores <- as.numeric(self$scores[, i])
                    self$results$factorScoresOV$setValues(index=i, scores)
                }
            }
        },

        #### Plot functions ----
        .prepareScreePlot = function() {
            image <- self$results$eigen$screePlot

            df <- list()
            df[["eigen"]] <- c(private$.getSimEigenCI(), self$eigen)
            df[["comp"]] <- factor(c(1:length(self$eigen), 1:length(self$eigen)))
            df[["type"]] <- c(rep(.("Simulations"), length(self$eigen)), rep(.("Data"), length(self$eigen)))

            attr(df, 'row.names') <- seq_len(length(df[[1]]))
            attr(df, 'class') <- 'data.frame'

            image$setState(df)
        },
        .screePlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            themeSpec <- theme(
                legend.position = c(1, 1),
                legend.justification = c(1, 1),
                legend.background = element_rect("transparent"),
                legend.title = element_blank(),
                legend.key = element_blank())

            nFactorMethod <- self$options$nFactorMethod

            data <- image$state

            if (nFactorMethod != "parallel")
                data <- subset(data, type == .("Data"))

            if (private$analysis == 'pca')
                type <- .('Component')
            else
                type <- .('Factor [specific factor]')

            p <- ggplot(data=data, aes(x=comp, y=eigen, group=type, linetype=factor(type))) +
                        geom_line(size=.8, colour=theme$color[1]) +
                        geom_point(aes(fill=factor(type), colour=factor(type)), shape=21, size=3) +
                        xlab(type) + ylab(.("Eigenvalue")) +
                        ggtheme + themeSpec

            if (nFactorMethod != "parallel")
                p <- p + theme(legend.position="none")

            if (nFactorMethod == "eigen")
                p <- p + geom_hline(aes(yintercept=self$options$minEigen), linetype = 2, colour=theme$color[1])

            return(p)
        },

        #### Helper functions ----
        .getDataRowNums = function() {
            if (is.null(private$.dataRowNums))
                private$.dataRowNums <- rownames(self$dataProcessed)

            return(private$.dataRowNums)
        },
        .getSimEigenCI = function(nIter = 20) {
            if (is.null(private$.simEigenCI)) {
                nSub <- dim(self$dataProcessed)[1]
                nVar <- dim(self$dataProcessed)[2]

                # Simulate eigen values
                simEigenList <- parallel::mclapply(1:nIter, function(XX) {
                    simData <- matrix(rnorm(nSub*nVar), nrow=nSub, ncol=nVar)
                    simCor <- cor(simData)

                    if (private$analysis == 'pca')
                        eigen(simCor)$values
                    else
                        psych::fa(simCor, fm=self$options$extraction, warnings=FALSE)$values
                })

                simEigen <- t(matrix(unlist(simEigenList), ncol=nIter))
                simEigenCI = apply(simEigen, 2, function(x) quantile(x,.95))

                private$.simEigenCI <- simEigenCI
            }

            return(private$.simEigenCI)
        },
        .getCorMatrix = function() {
            if (is.null(private$.corMatrix))
                private$.corMatrix <- cor(self$dataProcessed, use = "pairwise")

            return(private$.corMatrix)
        },
        .getPsychResult = function() {
            if (is.null(private$.psychResult)) {
                suppressWarnings({
                    if (private$analysis == 'pca') {
                        r <- psych::principal(
                            self$dataProcessed,
                            nfactors = self$nFactors,
                            rotate = self$options$rotation
                        )
                    } else {
                        r <- psych::fa(
                            self$dataProcessed,
                            nfactors = self$nFactors,
                            rotate = self$options$rotation,
                            fm = self$options$extraction
                        )
                    }
                })

                private$.psychResult <- r
            }

            return(private$.psychResult)
        },
        .errorCheck = function() {
            nFactorMethod <- self$options$nFactorMethod
            nFactors <- self$options$nFactors
            vars <- self$options$vars

            if (private$analysis == 'pca')
                type <- .('components')
            else
                type <- .('factors')

            if (nFactorMethod == "fixed" && nFactors > length(vars)) {
                jmvcore::reject(
                    jmvcore::format(
                        'Number of {factors} cannot be bigger than number of variables',
                        factors=type
                    ),
                    code=''
                )
            }
        },
        .cleanData = function() {
            vars <- self$options$vars

            data <- list()
            for (var in vars)
                data[[jmvcore::toB64(var)]] <- jmvcore::toNumeric(self$data[[var]])

            attr(data, 'row.names') <- rownames(self$data)
            attr(data, 'class') <- 'data.frame'
            data <- jmvcore::naOmit(data)

            return(data)
        },
        .parallel = function(nIter = 20) {
            nFactors <- max(which(! (self$eigen > private$.getSimEigenCI(nIter = 20)))[1] - 1, 1)

            if (is.na(nFactors))
                nFactors <- length(self$eigen)

            return(nFactors)
        })
)
