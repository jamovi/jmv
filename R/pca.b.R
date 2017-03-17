
pcaClass <- R6::R6Class(
    "pcaClass",
    inherit = pcaBase,
    private = list(
        #### Member variables ----
        eigen = NULL,
        simEigen = NULL,

        #### Init + run functions ----
        .init = function() {

            private$.initLoadingsTable()
            private$.initEigenTable()

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
                private$.populateFactorCorTable(results)
                private$.prepareScreePlot()

            }
        },

        #### Compute results ----
        .compute = function(data) {

            nFactors <- private$.nFactors(data)

            suppressWarnings({

                r <- psych::principal(data, nfactors = nFactors, rotate = self$options$rotation)

                devnull <- '/dev/null'
                if (Sys.info()['sysname'] == 'Windows')
                    devnull <- 'NUL'
                    sink(file=devnull)

                SS <- print(r)$Vaccounted

                sink()

            }) # suppressWarnings

            factorCor <- r$r.scores
            loadings <- r$loadings
            uniqueness <- r$uniqueness

            return(list(loadings=loadings, uniqueness=uniqueness, factorCor=factorCor, SS=SS, nFactors=nFactors))
        },

        #### Init tables/plots functions ----
        .initLoadingsTable = function() {

            table <- self$results$loadings
            table$setNote("note", jmvcore::format("\'{}\' rotation was used", self$options$rotation))

        },
        .initEigenTable = function() {

            table <- self$results$eigen

            for (i in seq_along(self$options$vars))
                table$addRow(rowKey=i, values=list(comp = as.character(i)))

        },

        #### Populate tables/plots functions ----
        .populateLoadingsTable = function(results) {

            table <- self$results$loadings
            loadings <- results$loadings
            uniqueness <- results$uniqueness
            nFactors <- results$nFactors

            vars <- self$options$vars
            hide <- self$options$hideLoadings

            if (nFactors > 1) {
                for (i in 2:nFactors)
                    table$addColumn(name=paste0("pc",i), title=as.character(i), type='number', superTitle='Component', index=i+1)
            }

            colNames <- sub('.', '', colnames(loadings))

            for (var in vars) {

                row <- list()
                for (j in 1:nFactors) {

                    index <- which(colNames %in% paste0("C",j))
                    l <- loadings[jmvcore::toB64(var), index]

                    row[[paste0("pc", j)]] <- if (abs(l) < hide) "" else l
                }

                row[["uniq"]] <- as.numeric(uniqueness[jmvcore::toB64(var)])

                table$setRow(rowKey=var, values=row)
            }
        },
        .populateEigenTable = function(results) {

            table <- self$results$eigen
            eigen <- private$eigen
            SS <- results$SS

            eigenTotal <- sum(eigen)
            varProp <- (eigen / eigenTotal) * 100
            varCum <- cumsum(varProp)
            eigenSS <- SS[1,]
            varPropSS <- SS[2,]
            varCumSS <- if (dim(SS)[1] <= 2) SS[2,] else SS[3,]

            for (i in seq_along(eigen)) {

                row <- list()
                row[["eigen"]] <- eigen[i]
                row[["varProp"]] <- varProp[i]
                row[["varCum"]] <- varCum[i]

                index <- which(sub('.', '', colnames(SS)) %in% paste0("C",i))

                row[["eigenSS"]] <- if (length(index) == 0) "" else eigenSS[index]
                row[["varPropSS"]] <- if (length(index) == 0) "" else varPropSS[index] * 100
                row[["varCumSS"]] <- if (length(index) == 0) "" else varCumSS[index] * 100

                table$setRow(rowNo=i, values=row)
            }
        },
        .populateFactorCorTable = function(results) {

            table <- self$results$factorCor
            nFactors <- results$nFactors
            factorCor <- results$factorCor

            vars <- self$options$vars

            if (nFactors > 1) {
                for (i in 2:nFactors)
                    table$addColumn(name=paste0("pc",i), title=as.character(i), type='number')
            }

            colNames <- sub('.', '', colnames(factorCor))

            for (i in 1:nFactors) {

                index1 <- which(colNames %in% paste0("C",i))
                row <- list()
                row[["comp"]] <- i

                for (j in 1:nFactors) {

                    index2 <- which(colNames %in% paste0("C",j))

                    if (i == j)
                        row[[paste0("pc", j)]] <- "—"
                    else if (j < i)
                        row[[paste0("pc", j)]] <- ""
                    else
                        row[[paste0("pc", j)]] <- factorCor[index1, index2]
                }

                table$addRow(rowKey=i, values=row)
            }
        },
        .prepareScreePlot = function() {

            image <- self$results$get('screePlot')

            df <- list()
            df[["eigen"]] <- c(private$simEigen, private$eigen)
            df[["comp"]] <- factor(c(1:length(private$eigen), 1:length(private$eigen)))
            df[["type"]] <- c(rep("Simulations", length(private$eigen)), rep("Data", length(private$eigen)))

            attr(df, 'row.names') <- seq_len(length(df[[1]]))
            attr(df, 'class') <- 'data.frame'

            image$setState(df)

        },
        .screePlot = function(image, ...) {

            if (is.null(image$state))
                return(FALSE)

            the <- theme(
                text = element_text(size=16, colour='#333333'),
                plot.background = element_rect(fill='transparent', color=NA),
                panel.background = element_rect(fill='#E8E8E8'),
                plot.margin = margin(15, 15, 15, 15),
                axis.text.x = element_text(margin=margin(5,0,0,0)),
                axis.text.y = element_text(margin=margin(0,5,0,0)),
                axis.title.x = element_text(margin=margin(10,0,0,0)),
                axis.title.y = element_text(margin=margin(0,10,0,0)),
                plot.title = element_text(margin=margin(0, 0, 15, 0)),
                legend.position = c(1, 1),
                legend.justification = c(1, 1),
                legend.background = element_rect("transparent"),
                legend.title = element_blank(),
                legend.key = element_blank())

            nFactorMethod <- self$options$nFactorMethod

            data <- image$state

            if (nFactorMethod != "parallel")
                data <- subset(data, type == "Data")

            p <- ggplot(data=data, aes(x=comp, y=eigen, group=type, linetype=type)) +
                      geom_line(size=.8) +
                      geom_point(aes(fill=type), shape=21, size=3) +
                      scale_colour_manual(values=c("white", "gray")) +
                      xlab("Component") +
                      ylab("Eigenvalue") +
                      the

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

            if (nFactorMethod == "fixed" && nFactors > length(vars))
                jmvcore::reject('Number of components cannot be bigger than number of variables', code='')

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

                corMatrix <- cor(data, use="pairwise")
                eigen <- eigen(corMatrix)$values

                nFactors <- sum(eigen > self$options$minEigen)

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
            eigen <- eigen(corMatrix)$values
            # eigenValuesEFA <- psych::fa(rx, fm="minres", warnings=FALSE)$values

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
                eigen(simCor)$values
                # psych::fa(simCor, fm="minres", SMC=FALSE, warnings=FALSE)$values

            })

            simEigen <- t(matrix(unlist(simEigenList), ncol=nIter))
            simEigenCI = apply(simEigen, 2, function(x) quantile(x,.95))

            nFactors <- which(!(eigen > simEigenCI))[1]-1

            private$eigen <- eigen
            private$simEigen <- simEigenCI

            return(nFactors)

        })
)
