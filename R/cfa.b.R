
cfaClass <- R6::R6Class(
    "cfaClass",
    inherit = cfaBase,
    private = list(
        #### Member variables ----
        estResCov = NULL,

        #### Init + run functions ----
        .init = function() {

            private$.initFactorLoadingsTable()
            private$.initFactorCovTable()
            private$.initFactorInterceptTable()
            private$.initResCovTable()
            private$.initResInterceptTable()
            private$.initCorResTable()
            private$.initResCovModTable()
            private$.initFactorLoadingsModTable()

            syntax <- private$.lavaanify(FALSE)
            self$results$.setModelSyntax(syntax)

        },
        .run = function() {

            ready <- private$.ready()

            if (ready) {

                data <- private$.cleanData()
                results <- private$.compute(data)

                private$.populateFactorLoadingsTable(results)
                private$.populateFactorCovTable(results)
                private$.populateFactorInterceptTable(results)
                private$.populateResCovTable(results)
                private$.populateResInterceptTable(results)
                private$.populateFitMeasuresTable(results)
                private$.populateTestTable(results)
                private$.populateCorResTable(results)
                private$.populateResCovModTable(results)
                private$.populateFactorLoadingsModTable(results)
                private$.preparePathDiagram(results)

            }
        },

        #### Compute results ----
        .compute = function(data) {

            model <- private$.lavaanify()
            std.lv <- self$options$constrain == "facVar"
            missing <- self$options$miss

            suppressWarnings({

                fit <- lavaan::cfa(model = model, data=data, std.lv=std.lv, missing=missing, meanstructure=TRUE)
                estimates <- lavaan::parameterestimates(fit, standardized = TRUE, level = self$options$ciWidth / 100)
                fitMeasures <- lavaan::fitMeasures(fit)
                residuals <- lavaan::residuals(fit, type = "cor")$cor
                modIndices <- lavaan::modificationIndices(fit, sort.=FALSE, minimum.value=0)

            }) # suppressWarnings

            return(list('fit'=fit, 'estimates'=estimates, 'fitMeasures'=fitMeasures,
                        'residuals'=residuals, 'modIndices'=modIndices))
        },

        #### Init tables/plots functions ----
        .initFactorLoadingsTable = function() {

            table <- self$results$factorLoadings
            factors <- self$options$factors
            rowNo <- 1

            ciWidth <- self$options$ciWidth
            table$getColumn('lower')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
            table$getColumn('upper')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))

            for (i in seq_along(factors)) {

                vars <- factors[[i]]$vars

                for (j in seq_along(vars)) {

                    factorName <- factors[[i]]$label
                    var <- vars[j]

                    if ( ! self$options$constrain == "facVar" && j == 1) {

                        row <- list("factor"=factorName, "indicator"=var, "est"=1, "se"='',
                                    "z"='', "p"='', "lower"='', "upper"='', "stdEst"='')

                        table$addRow(rowKey=rowNo, values=row)
                        table$addFootnote(rowKey=rowNo, 'est', 'fixed parameter')

                    } else {

                        row <- list("factor"=factorName, "indicator"=var)
                        table$addRow(rowKey=rowNo, values=row)

                    }

                    if (j == 1)
                        table$addFormat(rowNo=rowNo, col=1, Cell.BEGIN_GROUP)

                    rowNo <- rowNo + 1
                }
            }
        },
        .initFactorCovTable = function() {

            table <- self$results$factorEst$factorCov
            factors <- sapply(self$options$factors, function(x) x$label)
            rowNo <- 1

            ciWidth <- self$options$ciWidth
            table$getColumn('lower')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
            table$getColumn('upper')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))

            if (length(factors) == 0)
                return()

            for (i in 1:length(factors)) {

                for (j in i:length(factors)) {

                    row <- list("factor1"=factors[i], "factor2"=factors[j])

                    if (self$options$constrain == "facVar" && j == i) {

                        row <- list("factor1"=factors[i], "factor2"=factors[j], "est"=1, "se"='',
                                    "z"='', "p"='', "lower"='', "upper"='', "stdEst"='')

                        table$addRow(rowKey=rowNo, values=row)
                        table$addFootnote(rowKey=rowNo, 'est', 'fixed parameter')

                    } else {

                        row <- list("factor1"=factors[i], "factor2"=factors[j])
                        table$addRow(rowKey=rowNo, values=row)

                    }

                    if (i == j)
                        table$addFormat(rowNo=rowNo, col=1, Cell.BEGIN_GROUP)

                    rowNo <- rowNo + 1
                }
            }
        },
        .initFactorInterceptTable = function() {

            table <- self$results$factorEst$factorIntercept
            factors <- sapply(self$options$factors, function(x) x$label)

            ciWidth <- self$options$ciWidth
            table$getColumn('lower')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
            table$getColumn('upper')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))

            for (i in 1:length(factors)) {

                row <- list("factor"=factors[i], "est"=1, "se"='', "z"='',
                            "p"='', "lower"='', "upper"='', "stdEst"='')

                table$addRow(rowKey=i, values=row)
                table$addFootnote(rowKey=i, 'est', 'fixed parameter')
            }
        },
        .initResCovTable = function() {

            table <- self$results$resEst$resCov
            vars <- unique(unlist(lapply(self$options$factors, function(x) x$vars)))
            resCov <- self$options$resCov
            rowNo <- 1

            ciWidth <- self$options$ciWidth
            table$getColumn('lower')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
            table$getColumn('upper')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))

            if (length(vars) == 0)
                return()

            varsMatrix <- matrix(0, nrow = length(vars), ncol = length(vars))
            diag(varsMatrix) <- 1

            for (i in seq_along(resCov)) {

                index1 <- which(vars == resCov[[i]][[1]])
                index2 <- which(vars == resCov[[i]][[2]])

                if (index1 > index2) {
                    index1Temp <- index1
                    index1 <- index2
                    index2 <- index1Temp
                }

                varsMatrix[index1,index2] <- 1
            }

            private$estResCov <- varsMatrix

            for (i in 1:nrow(varsMatrix)) {

                for (j in i:ncol(varsMatrix)) {

                    if (varsMatrix[i,j] == 1) {

                        row <- list("var1"=vars[i], "var2"=vars[j])
                        table$addRow(rowKey=rowNo, values=row)

                        if (i == j)
                            table$addFormat(rowNo=rowNo, col=1, Cell.BEGIN_GROUP)

                        rowNo <- rowNo + 1

                    }
                }
            }
        },
        .initResInterceptTable = function() {

            table <- self$results$resEst$resIntercept
            vars <- unique(unlist(lapply(self$options$factors, function(x) x$vars)))

            ciWidth <- self$options$ciWidth
            table$getColumn('lower')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
            table$getColumn('upper')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))

            for (i in seq_along(vars))
                table$addRow(rowKey=i, values=list('var'=vars[i]))

        },
        .initCorResTable = function() {

            table <- self$results$modelPerformance$corRes
            vars <- unique(unlist(lapply(self$options$factors, function(x) x$vars)))

            for (i in seq_along(vars))
                table$addColumn(name=jmvcore::toB64(vars[i]), title=vars[i], type='number', format='zto')

            for (i in seq_along(vars))
                table$addRow(rowKey=jmvcore::toB64(vars[i]), values=list(var=vars[i]))

            for (i in seq_along(vars)) {
                row <- list()
                for (j in 1:i) {
                    row[[jmvcore::toB64(vars[j])]] <- ''
                }
                table$setRow(rowNo=i, values=row)
            }

        },
        .initResCovModTable = function() {

            table <- self$results$modelPerformance$modIndices$resCovMod
            vars <- unique(unlist(lapply(self$options$factors, function(x) x$vars)))

            for (i in seq_along(vars)) {
                table$addColumn(name=jmvcore::toB64(vars[i]), title=vars[i], type='number')
                table$addColumn(name=jmvcore::toB64(vars[i]), title=vars[i], type='number')
            }

            for (i in seq_along(vars)) {
                table$addRow(rowKey=jmvcore::toB64(vars[i]), values=list('var'=vars[i]))
            }

            varsMatrix <- private$estResCov
            varsMatrix <- abs(varsMatrix - 1)
            varsMatrix[lower.tri(varsMatrix)] <- 0

            for (i in seq_along(vars)) {
                row <- list()
                for (j in seq_along(vars)) {
                    if (varsMatrix[i,j] == 0) {
                        row[[jmvcore::toB64(vars[j])]] <- ''
                    }
                }
                table$setRow(rowNo=i, values=row)
            }
        },
        .initFactorLoadingsModTable = function() {

            table <- self$results$modelPerformance$modIndices$factorLoadingsMod
            vars <- unique(unlist(lapply(self$options$factors, function(x) x$vars)))
            factors <- sapply(self$options$factors, function(x) x$label)

            if (length(factors) <= 1)
                table$setVisible(visible=FALSE)

            if (length(vars) == 0)
                return()

            for (i in seq_along(factors)) {
                table$addColumn(name=jmvcore::toB64(factors[i]), title=factors[i], type='number')
                table$addColumn(name=jmvcore::toB64(factors[i]), title=factors[i], type='number')
            }

            for (i in seq_along(vars)) {
                table$addRow(rowKey=jmvcore::toB64(vars[i]), values=list('var'=vars[i]))
            }

            for (i in seq_along(vars)) {

                row <- list()

                for (j in seq_along(factors)) {

                    v <- self$options$factors[[j]]$vars

                    if (vars[i] %in% v) {
                        row[[jmvcore::toB64(factors[j])]] <- ''
                    }
                }
                table$setRow(rowNo=i, values=row)
            }
        },

        #### Populate tables ----
        .populateFactorLoadingsTable = function(results) {

            table <- self$results$factorLoadings

            factors <- self$options$factors
            r <- results$estimates

            rowNo <- 1

            for (i in seq_along(factors)) {

                vars <- factors[[i]]$vars

                for (j in seq_along(vars)) {

                    if (self$options$constrain == "facVar" || j != 1) {

                        factorName <- jmvcore::toB64(factors[[i]]$label)
                        var <- jmvcore::toB64(vars[j])

                        index <- which(r$lhs == factorName & r$rhs == var)

                        row <- list()
                        row[['est']] <- r[index, 'est']
                        row[['se']] <- r[index, 'se']
                        row[['z']] <- if (is.na(r[index, 'z'])) '' else r[index, 'z']
                        row[['p']] <- if (is.na(r[index, 'pvalue'])) '' else r[index, 'pvalue']
                        row[['lower']] <- if (is.na(r[index, 'ci.lower'])) '' else r[index, 'ci.lower']
                        row[['upper']] <- if (is.na(r[index, 'ci.upper'])) '' else r[index, 'ci.upper']
                        row[['stdEst']] <- if (is.na(r[index, 'std.all'])) '' else r[index, 'std.all']

                        table$setRow(rowNo=rowNo, values=row)

                    }

                    rowNo <- rowNo + 1
                }
            }
        },
        .populateFactorCovTable = function(results) {

            table <- self$results$factorEst$factorCov
            factors <- sapply(self$options$factors, function(x) x$label)

            r <- results$estimates
            rowNo <- 1

            for (i in 1:length(factors)) {

                for (j in i:length(factors)) {

                    if ( ! self$options$constrain == "facVar" || j != i) {

                        factor1 <- jmvcore::toB64(factors[i])
                        factor2 <- jmvcore::toB64(factors[j])

                        index <- which(r$lhs == factor1 & r$rhs == factor2)

                        row <- list()
                        row[['est']] <- r[index, 'est']
                        row[['se']] <- r[index, 'se']
                        row[['z']] <- if (is.na(r[index, 'z'])) '' else r[index, 'z']
                        row[['p']] <- if (is.na(r[index, 'pvalue'])) '' else r[index, 'pvalue']
                        row[['lower']] <- if (is.na(r[index, 'ci.lower'])) '' else r[index, 'ci.lower']
                        row[['upper']] <- if (is.na(r[index, 'ci.upper'])) '' else r[index, 'ci.upper']
                        row[['stdEst']] <- if (is.na(r[index, 'std.all'])) '' else r[index, 'std.all']

                        table$setRow(rowNo=rowNo, values=row)
                    }

                    rowNo <- rowNo + 1

                }
            }
        },
        .populateFactorInterceptTable = function(results) {

            table <- self$results$factorEst$factorIntercept
            factors <- sapply(self$options$factors, function(x) x$label)

            r <- results$estimates

            # for (i in 1:length(factors)) {
            #
            #         factor <- jmvcore::toB64(factors[i])
            #
            #         index <- which(r$lhs == factor & r$rhs == '')
            #
            #         row <- list()
            #         row[['est']] <- r[index, 'est']
            #         row[['se']] <- r[index, 'se']
            #         row[['z']] <- if (is.na(r[index, 'z'])) '' else r[index, 'z']
            #         row[['p']] <- if (is.na(r[index, 'pvalue'])) '' else r[index, 'pvalue']
            #         row[['lower']] <- if (is.na(r[index, 'ci.lower'])) '' else r[index, 'ci.lower']
            #         row[['upper']] <- if (is.na(r[index, 'ci.upper'])) '' else r[index, 'ci.upper']
            #         row[['stdEst']] <- if (is.na(r[index, 'std.all'])) '' else r[index, 'std.all']
            #
            #         table$setRow(rowNo=i, values=row)
            # }
        },
        .populateResCovTable = function(results) {

            table <- self$results$resEst$resCov
            vars <- unique(unlist(lapply(self$options$factors, function(x) x$vars)))

            r <- results$estimates
            varsMatrix <- private$estResCov
            rowNo <- 1

            for (i in 1:nrow(varsMatrix)) {

                for (j in i:ncol(varsMatrix)) {

                    if (varsMatrix[i,j] == 1) {

                        var1 <- jmvcore::toB64(vars[i])
                        var2 <- jmvcore::toB64(vars[j])

                        index <- which(r$lhs == var1 & r$rhs == var2)

                        row <- list()
                        row[['est']] <- r[index, 'est']
                        row[['se']] <- r[index, 'se']
                        row[['z']] <- if (is.na(r[index, 'z'])) '' else r[index, 'z']
                        row[['p']] <- if (is.na(r[index, 'pvalue'])) '' else r[index, 'pvalue']
                        row[['lower']] <- if (is.na(r[index, 'ci.lower'])) '' else r[index, 'ci.lower']
                        row[['upper']] <- if (is.na(r[index, 'ci.upper'])) '' else r[index, 'ci.upper']
                        row[['stdEst']] <- if (is.na(r[index, 'std.all'])) '' else r[index, 'std.all']

                        table$setRow(rowNo=rowNo, values=row)

                        rowNo <- rowNo + 1

                    }
                }
            }
        },
        .populateResInterceptTable = function(results) {

            table <- self$results$resEst$resIntercept
            vars <- unique(unlist(lapply(self$options$factors, function(x) x$vars)))

            r <- results$estimates

            for (i in 1:length(vars)) {

                var <- jmvcore::toB64(vars[i])

                index <- which(r$lhs == var & r$rhs == '')

                row <- list()
                row[['est']] <- r[index, 'est']
                row[['se']] <- r[index, 'se']
                row[['z']] <- if (is.na(r[index, 'z'])) '' else r[index, 'z']
                row[['p']] <- if (is.na(r[index, 'pvalue'])) '' else r[index, 'pvalue']
                row[['lower']] <- if (is.na(r[index, 'ci.lower'])) '' else r[index, 'ci.lower']
                row[['upper']] <- if (is.na(r[index, 'ci.upper'])) '' else r[index, 'ci.upper']
                row[['stdEst']] <- if (is.na(r[index, 'std.all'])) '' else r[index, 'std.all']

                table$setRow(rowNo=i, values=row)
            }
        },
        .populateFitMeasuresTable = function(results) {

            table <- self$results$modelFit$fitMeasures
            r <- results$fitMeasures

            row <- list()
            row[['cfi']] <- as.numeric(r['cfi'])
            row[['tli']] <- as.numeric(r['tli'])
            row[['aic']] <- as.numeric(r['aic'])
            row[['bic']] <- as.numeric(r['bic'])
            row[['srmr']] <- as.numeric(r['srmr'])
            row[['rmsea']] <- as.numeric(r['rmsea'])
            row[['rmseaLower']] <- as.numeric(r['rmsea.ci.lower'])
            row[['rmseaUpper']] <- as.numeric(r['rmsea.ci.upper'])

            table$setRow(rowNo=1, values=row)
        },
        .populateTestTable = function(results) {

            table <- self$results$modelFit$test
            r <- results$fitMeasures

            row <- list()
            row[['chi']] <- as.numeric(r['chisq'])
            row[['df']] <- as.numeric(r['df'])
            row[['p']] <- as.numeric(r['pvalue'])

            table$setRow(rowNo=1, values=row)
        },
        .populateCorResTable = function(results) {

            table <- self$results$modelPerformance$corRes
            vars <- unique(unlist(lapply(self$options$factors, function(x) x$vars)))
            highlight <- self$options$hlCorRes

            r <- results$residuals

            for (i in 1:(length(vars) - 1)) {
                row <- list()
                highVars <- c()
                for (j in (i+1):length(vars)) {
                    row[[jmvcore::toB64(vars[j])]] <- r[i,j]

                    if (abs(r[i,j]) > highlight)
                        highVars <- c(highVars, jmvcore::toB64(vars[j]))
                }
                table$setRow(rowNo=i, values=row)

                for (highVar in highVars)
                    table$addFormat(col=highVar, rowNo=i, Cell.NEGATIVE)
            }
        },
        .populateResCovModTable = function(results) {

            table <- self$results$modelPerformance$modIndices$resCovMod
            vars <- unique(unlist(lapply(self$options$factors, function(x) x$vars)))
            highlight <- self$options$hlMI

            r <- results$modIndices

            varsMatrix <- private$estResCov
            varsMatrix <- abs(varsMatrix - 1)
            varsMatrix[lower.tri(varsMatrix)] <- 0

            for (i in seq_along(vars)) {

                row <- list()
                highVars <- c()
                for (j in seq_along(vars)) {

                    if (varsMatrix[i,j] == 1) {

                        var1 <- jmvcore::toB64(vars[i])
                        var2 <- jmvcore::toB64(vars[j])

                        index <- which(r$lhs == var1 & r$rhs == var2)
                        row[[jmvcore::toB64(vars[j])]] <- r[index, 'mi']

                        if (length(index) == 0) {
                            row[[jmvcore::toB64(vars[j])]] <- ''
                        } else {
                            row[[jmvcore::toB64(vars[j])]] <- r[index, 'mi']

                            if (r[index, 'mi'] > highlight)
                                highVars <- c(highVars, jmvcore::toB64(vars[j]))
                        }
                    }
                }
                table$setRow(rowNo=i, values=row)

                for (highVar in highVars)
                    table$addFormat(col=highVar, rowNo=i, Cell.NEGATIVE)
            }
        },
        .populateFactorLoadingsModTable = function(results) {

            table <- self$results$modelPerformance$modIndices$factorLoadingsMod
            vars <- unique(unlist(lapply(self$options$factors, function(x) x$vars)))
            factors <- sapply(self$options$factors, function(x) x$label)
            highlight <- self$options$hlMI

            r <- results$modIndices

            for (i in seq_along(vars)) {

                row <- list()
                highVars <- c()

                for (j in seq_along(factors)) {

                    v <- self$options$factors[[j]]$vars

                    if ( ! (vars[i] %in% v)) {

                        fact <- jmvcore::toB64(factors[j])
                        var <- jmvcore::toB64(vars[i])

                        index <- which(r$lhs == fact & r$rhs == var)

                        if (length(index) == 0) {
                            row[[jmvcore::toB64(factors[j])]] <- ''
                        } else {
                            row[[jmvcore::toB64(factors[j])]] <- r[index, 'mi']

                            if (r[index, 'mi'] > highlight)
                                highVars <- c(highVars, jmvcore::toB64(factors[j]))
                        }
                    }
                }
                table$setRow(rowNo=i, values=row)

                for (highVar in highVars)
                    table$addFormat(col=highVar, rowNo=i, Cell.NEGATIVE)
            }
        },

        #### Plot functions ----
        .preparePathDiagram = function(results) {

            fit <- results$fit

            fit@ParTable$rhs <- jmvcore::fromB64(fit@ParTable$rhs)
            fit@ParTable$lhs <- jmvcore::fromB64(fit@ParTable$lhs)

            semPlotModel <- try(semPlot::semPlotModel(fit), silent = TRUE)

            image <- self$results$pathDiagram
            image$setState(list(semPlotModel=semPlotModel))

        },
        .pathDiagram = function(image, theme, ...) {

            if (is.null(image$state))
                return(FALSE)

            semPlotModel <- image$state$semPlotModel

            vars <- unique(unlist(lapply(self$options$factors, function(x) x$vars)))
            factors <- sapply(self$options$factors, function(x) x$label)

            colors <- c(rep(theme$fill[1], length(vars)), rep(theme$fill[1], length(factors)))
            edgeColor <- theme$color[1]

            if (requireNamespace('semPlot')) {

                suppressWarnings({

                    semPlot::semPaths(semPlotModel, intercepts = FALSE, residuals = FALSE, rotation = 4,
                                      whatLabels = 'omit', mar = c(2,6,2,6), curve = .1, curvature = 30,
                                      color = colors, edge.color = edgeColor, edge.width = 2,
                                      bg = "transparent", sizeMan = 6,
                                      sizeLat = 10, nCharNodes = 3, arrows = TRUE)

                }) # suppressWarnings

            } else {

                warning("In order to get the path diagram you need to install the R package \"semplot\"")

            }

            TRUE
        },

        #### Helper functions ----
        .ready = function() {

            factors <- self$options$factors

            if (length(factors) < 1)
                return(FALSE)

            nVars <- lapply(factors, function(x) length(x$vars))

            if (any(nVars < 1))
                return(FALSE)

            if (length(factors) == 1 && nVars <= 2)
                return(FALSE)

            return(TRUE)

        },
        .cleanData = function() {

            vars <- unlist(self$options$factors)

            data <- list()
            for (var in vars)
                data[[jmvcore::toB64(var)]] <- jmvcore::toNumeric(self$data[[var]])

            attr(data, 'row.names') <- seq_len(length(data[[1]]))
            attr(data, 'class') <- 'data.frame'

            return(data)

        },
        .lavaanify = function(B64 = TRUE) {

            factors <- self$options$factors
            resCov <- self$options$resCov

            model <- '# Latent variable definitions'
            for (i in seq_along(factors)) {

                if (B64) {
                    vars <- jmvcore::toB64(factors[[i]]$vars)
                    factorName <- jmvcore::toB64(factors[[i]]$label)
                } else {
                    vars <- factors[[i]]$vars
                    factorName <- factors[[i]]$label
                }

                term <- paste(factorName, paste(vars, collapse = ' + '), sep=' =~ ')
                model <- paste(model, term, sep='\n')
            }

            if (length(resCov) > 0) {

                model <- paste(model,  model <- '\n\n# Residual covariances')

                for (i in seq_along(resCov)) {

                    pair <- resCov[[i]]

                    if (B64) {
                        pair1 <- jmvcore::toB64(pair[[1]])
                        pair2 <- jmvcore::toB64(pair[[2]])
                    } else {
                        pair1 <- pair[[1]]
                        pair2 <- pair[[2]]
                    }

                    cov <- paste(pair1, pair2 , sep=' ~~ ')
                    model <- paste(model, cov, sep='\n')
                }
            }

            return(model)
        })
)
