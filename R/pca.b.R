
pcaClass <- R6::R6Class(
    "pcaClass",
    inherit = pcaBase,
    private = list(
        #### Member variables ----
        analysis = 'pca',
        eigen = NULL,
        simEigen = NULL,

        #### Init + run functions ----
        .init = function() {

            private$.initLoadingsTable()
            private$.initModelFitTable()
            private$.initEigenTable()
            private$.initKMOTable()

        },
        .run = function() {

            ready <- TRUE
            if (is.null(self$options$vars) || length(self$options$vars) < 2)
                ready <- FALSE

            private$.errorCheck()

            if (ready) {

                data <- private$.cleanData()
                results <- private$.compute(data)

                private$.populateLoadingsTable(results)
                private$.populateEigenTable(results)
                private$.populateFactorSummaryTable(results)
                private$.populateFactorCorTable(results)
                private$.populateModelFitTable(results)
                private$.populateKMOTable(results)
                private$.populateBartlettTable(results)
                private$.prepareScreePlot()

            }
        },

        #### Compute results ----
        .compute = function(data) {

            nFactors <- private$.nFactors(data)

            suppressWarnings({

                if (private$analysis == 'pca')
                    r <- psych::principal(data, nfactors = nFactors, rotate = self$options$rotation)
                else
                    r <- psych::fa(data, nfactors = nFactors, rotate = self$options$rotation)

                devnull <- '/dev/null'
                if (Sys.info()['sysname'] == 'Windows')
                    devnull <- 'nul'

                sink(file=devnull)

                SS <- print(r)$Vaccounted

                sink()

                corMatrix <- cor(data, use = "pairwise")
                N <- nrow(data)
                kmo <- psych::KMO(corMatrix)
                bartlett <- psych::cortest.bartlett(corMatrix, n = N)

            }) # suppressWarnings

            factorCor <- r$r.scores
            loadings <- r$loadings
            uniqueness <- r$uniqueness

            modelFit <- NULL
            if (private$analysis == 'efa')
                modelFit <- list('tli'=r$TLI, 'bic'=r$BIC, 'rmsea'=r$RMSEA[1], 'chi'=r$STATISTIC, 'df'=r$dof, 'p'=r$PVAL)

            return(list(loadings=loadings, uniqueness=uniqueness, factorCor=factorCor, SS=SS, nFactors=nFactors,
                        modelFit=modelFit, kmo=kmo, bartlett=bartlett))
        },

        #### Init tables/plots functions ----
        .initLoadingsTable = function() {

            table <- self$results$loadings
            table$setNote("note", jmvcore::format("\'{}\' rotation was used", self$options$rotation))

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

            table$addRow(rowKey=1, values=list(name = "Overall"))
            table$addFormat(rowKey=1, col=1, Cell.END_GROUP)

            for (i in seq_along(vars))
                table$addRow(rowKey=i+1, values=list(name = vars[[i]]))

        },

        #### Populate tables/plots functions ----
        .populateLoadingsTable = function(results) {

            table <- self$results$loadings
            loadings <- results$loadings
            uniqueness <- results$uniqueness
            nFactors <- results$nFactors

            vars <- self$options$vars
            hide <- self$options$hideLoadings

            if (private$analysis == 'pca')
                type <- 'Component'
            else
                type <- 'Factor'

            if (nFactors > 1) {
                for (i in 2:nFactors)
                    table$addColumn(name=paste0("pc",i), title=as.character(i), type='number', superTitle=jmvcore::format('{}', type), index=i+1)
            }

            for (var in vars) {

                row <- list()
                for (j in 1:nFactors) {

                    l <- loadings[jmvcore::toB64(var), j]
                    row[[paste0("pc", j)]] <- if (abs(l) < hide) "" else l
                }

                row[["uniq"]] <- as.numeric(uniqueness[jmvcore::toB64(var)])

                table$setRow(rowKey=var, values=row)
            }
        },
        .populateEigenTable = function(results) {

            table <- self$results$eigen$initEigen
            eigen <- private$eigen

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
        .populateFactorSummaryTable = function(results) {

            table <- self$results$factorStats$factorSummary

            SS <- results$SS
            nFactors <- results$nFactors

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
        .populateFactorCorTable = function(results) {

            table <- self$results$factorStats$factorCor
            nFactors <- results$nFactors
            factorCor <- results$factorCor

            vars <- self$options$vars

            if (nFactors > 1) {
                for (i in 2:nFactors)
                    table$addColumn(name=paste0("pc",i), title=as.character(i), type='number')
            }

            colNames <- sub('.', '', colnames(factorCor))

            for (i in 1:nFactors) {

                row <- list()
                row[["comp"]] <- i

                for (j in 1:nFactors) {

                    if (i == j)
                        row[[paste0("pc", j)]] <- "\u2014"
                    else if (j < i)
                        row[[paste0("pc", j)]] <- ""
                    else
                        row[[paste0("pc", j)]] <- factorCor[i, j]
                }

                table$addRow(rowKey=i, values=row)
            }
        },
        .populateModelFitTable = function(results) {

            if (private$analysis == 'efa') {

                r <- results$modelFit

                table <- self$results$modelFit$fit

                row <- list()
                row['tli'] <- r$tli
                row['bic'] <- r$bic
                row['rmsea'] <- r$rmsea
                row['chi'] <- r$chi
                row['df'] <- r$df
                row['p'] <- r$p

                table$setRow(rowNo=1, values=row)

            }
        },
        .populateKMOTable = function(results) {

            table <- self$results$assump$kmo
            vars <- self$options$vars
            kmo <- results$kmo

            table$setRow(rowNo=1, values=list(msa=kmo$MSA))

            for (i in seq_along(vars)) {

                msa <- kmo$MSAi[ jmvcore::toB64(vars[[i]]) ]

                table$setRow(rowNo=i+1, values=list(msa = msa))
            }
        },
        .populateBartlettTable = function(results) {

            table <- self$results$assump$bartlett
            r <- results$bartlett

            table$setRow(rowNo=1, values=list(chi=r$chisq, df=r$df, p=r$p.value))

        },

        #### Plot functions ----
        .prepareScreePlot = function() {

            image <- self$results$eigen$screePlot

            df <- list()
            df[["eigen"]] <- c(private$simEigen, private$eigen)
            df[["comp"]] <- factor(c(1:length(private$eigen), 1:length(private$eigen)))
            df[["type"]] <- c(rep("Simulations", length(private$eigen)), rep("Data", length(private$eigen)))

            attr(df, 'row.names') <- seq_len(length(df[[1]]))
            attr(df, 'class') <- 'data.frame'

            image$setState(df)

        },
        .screePlot = function(image, theme, ...) {

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
                data <- subset(data, type == "Data")

            if (private$analysis == 'pca')
                type <- 'Component'
            else
                type <- 'Factor'

            p <- ggplot(data=data, aes(x=comp, y=eigen, group=type, linetype=factor(type))) +
                        geom_line(size=.8) +
                        geom_point(aes(fill=factor(type)), shape=21, size=3) +
                        # scale_fill_manual(values=c("black", "white")) +
                        xlab(type) +
                        ylab("Eigenvalue") +
                        theme + themeSpec

            if (nFactorMethod != "parallel")
                p <- p + theme(legend.position="none")

            if (nFactorMethod == "eigen")
                p <- p + geom_hline(aes(yintercept=self$options$minEigen), linetype = 2)

            print(p)

            TRUE
        },

        #### Helper functions ----
        .errorCheck = function() {

            nFactorMethod <- self$options$nFactorMethod
            nFactors <- self$options$nFactors
            vars <- self$options$vars

            if (private$analysis == 'pca')
                type <- 'components'
            else
                type <- 'factors'

            if (nFactorMethod == "fixed" && nFactors > length(vars))
                jmvcore::reject(jmvcore::format('Number of {} cannot be bigger than number of variables', type), code='')

        },
        .cleanData = function() {

            vars <- self$options$vars

            data <- list()
            for (var in vars)
                data[[jmvcore::toB64(var)]] <- jmvcore::toNumeric(self$data[[var]])

            attr(data, 'row.names') <- seq_len(length(data[[1]]))
            attr(data, 'class') <- 'data.frame'
            data <- jmvcore::naOmit(data)

            return(data)
        },
        .nFactors = function(data) {

            method <- self$options$nFactorMethod

            nFactorsPar <- private$.parallel(data)

            if (method == "parallel") {

                nFactors <- nFactorsPar

            } else if (method == "eigen") {

                nFactors <- sum(private$eigen > self$options$minEigen)

                if (nFactors <= 0)
                    jmvcore::reject(jmvcore::format('No components have an eigenvalue greater than {}'), self$options$minEigen, code='')

            } else {

                nFactors <- self$options$nFactors

            }

            return(nFactors)
        },
        .parallel = function(data, nIter = 20) {

            corMatrix <- cor(data, use="pairwise")

            # Eigenvalues for PCA and EFA
            if (private$analysis == 'pca')
                eigen <- eigen(corMatrix)$values
            else
                eigen<- psych::fa(corMatrix, fm="minres", warnings=FALSE)$values

            nSub <- dim(data)[1]
            nVar <- dim(data)[2]

            # Simulate eigen values
            simEigenList <- parallel::mclapply(1:nIter, function(XX) {

                # bad <- TRUE
                # while(bad) {
                #
                #     samplesData <- matrix(apply(data, 2, function(y) sample(y, nSub, replace=TRUE)), ncol=nVar)
                #     samplesCor <- cor(samples, use="pairwise")
                #
                #     bad <- any(is.na(samplesCor))
                # }

                simData <- matrix(rnorm(nSub*nVar), nrow=nSub, ncol=nVar)
                simCor <- cor(simData)

                if (private$analysis == 'pca')
                    eigen(simCor)$values
                else
                    psych::fa(simCor, fm="minres", warnings=FALSE)$values

            })

            simEigen <- t(matrix(unlist(simEigenList), ncol=nIter))
            simEigenCI = apply(simEigen, 2, function(x) quantile(x,.95))

            nFactors <- which(!(eigen > simEigenCI))[1]-1

            if (is.na(nFactors))
                nFactors <- length(eigen)

            private$eigen <- eigen
            private$simEigen <- simEigenCI

            return(nFactors)

        })
)
