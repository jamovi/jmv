
corrPartClass <- R6::R6Class(
    "corrPartClass",
    inherit = corrPartBase,
    private = list(
        #### Init + run functions ----
        .init=function() {

            private$.initCorrTable()

        },
        .run=function() {

            results <- private$.compute()

            private$.populateCorrTable(results)

        },

        #### Compute results ----
        .compute = function() {

            vars <- self$options$vars
            nVars <- length(vars)
            hyp <- self$options$hypothesis
            controls <- self$options$controls
            type <- self$options$type

            if (hyp == 'pos')
                hyp <- 'greater'
            else if (hyp == 'neg')
                hyp <- 'less'
            else
                hyp <- 'equal'

            results <- list()
            if (nVars > 1) {
                for (i in 1:nVars) {
                    rowVar <- vars[[i]]
                    for (j in 1:nVars) {
                        if (i == j || (type != 'semi' && j > i))
                            next
                        colVar <- vars[[j]]
                        results[[rowVar]][[colVar]] <- private$.test(rowVar, colVar, controls, type)
                    }
                }
            }

            return(results)
        },

        #### Init tables/plots functions ----
        .initCorrTable = function() {

            matrix <- self$results$matrix
            vars <- self$options$vars
            nVars <- length(vars)
            type <- self$options$type

            for (i in seq_along(vars)) {
                var <- vars[[i]]

                matrix$addColumn(name=paste0(var, '[r]'), title=var,
                                 type='number', format='zto', visible='(pearson)')
                matrix$addColumn(name=paste0(var, '[rp]'), title=var,
                                 type='number', format='zto,pvalue', visible='(pearson && sig)')
                matrix$addColumn(name=paste0(var, '[rho]'), title=var,
                                 type='number', format='zto', visible='(spearman)')
                matrix$addColumn(name=paste0(var, '[rhop]'), title=var,
                                 type='number', format='zto,pvalue', visible='(spearman && sig)')
                matrix$addColumn(name=paste0(var, '[tau]'), title=var,
                                 type='number', format='zto', visible='(kendall)')
                matrix$addColumn(name=paste0(var, '[taup]'), title=var,
                                 type='number', format='zto,pvalue', visible='(kendall && sig)')
                matrix$addColumn(name=paste0(var, '[n]'), title=var,
                                 type='integer', visible='(n)')
            }

            for (i in seq_along(vars)) {

                var <- vars[[i]]
                values <- list()

                if (type != 'semi') {
                    for (j in seq(i, nVars)) {
                        v <- vars[[j]]
                        values[[paste0(v, '[r]')]] <- ''
                        values[[paste0(v, '[rp]')]] <- ''
                        values[[paste0(v, '[rho]')]] <- ''
                        values[[paste0(v, '[rhop]')]] <- ''
                        values[[paste0(v, '[tau]')]] <- ''
                        values[[paste0(v, '[taup]')]] <- ''
                        values[[paste0(v, '[n]')]] <- ''
                    }
                }

                values[[paste0(var, '[r]')]] <- '\u2014'
                values[[paste0(var, '[rp]')]] <- '\u2014'
                values[[paste0(var, '[rho]')]] <- '\u2014'
                values[[paste0(var, '[rhop]')]] <- '\u2014'
                values[[paste0(var, '[tau]')]] <- '\u2014'
                values[[paste0(var, '[taup]')]] <- '\u2014'
                values[[paste0(var, '[n]')]] <- '\u2014'

                matrix$setRow(rowKey=var, values)
            }

            hyp <- self$options$hypothesis
            flag <- self$options$flag
            controls <- self$options$controls

            if (length(controls) > 0 && type != 'zero') {
                matrix$setNote('controls', jmvcore::format('controlling for {}', listItems(controls)))
            }

            if (hyp == 'pos') {
                matrix$setNote('hyp', 'H\u2090 is positive correlation')
                hyp <- 'greater'
                if (flag)
                    matrix$setNote('flag', '* p < .05, ** p < .01, *** p < .001, one-tailed')
            }
            else if (hyp == 'neg') {
                matrix$setNote('hyp', 'H\u2090 is negative correlation')
                hyp <- 'less'
                if (flag)
                    matrix$setNote('flag', '* p < .05, ** p < .01, *** p < .001, one-tailed')
            }
            else {
                matrix$setNote('hyp', NULL)
                hyp <- 'two.sided'
                if (flag)
                    matrix$setNote('flag', '* p < .05, ** p < .01, *** p < .001')
            }

            if ( ! flag)
                matrix$setNote('flag', NULL)

            if (type == 'part') {
                titleMatrix <- "Partial Correlation"
            } else if (type == 'semi') {
                titleMatrix <- "Semipartial Correlation"
            } else {
                titleMatrix <- "Zero-order Correlation"
            }

            pearson <- self$options$pearson
            spearman <- self$options$spearman
            kendall <- self$options$kendall
            n <- self$options$n
            sig <- self$options$sig

            if ( ! sum(pearson, spearman, kendall) > 1  && ! n && ! sig) {
                if (pearson)
                    titleMatrix <- jmvcore::format("{} - Pearson's r", titleMatrix)
                else if (spearman)
                    titleMatrix <- jmvcore::format("{} - Spearman's rho", titleMatrix)
                else if (kendall)
                    titleMatrix <- jmvcore::format("{} - Kendall's Tau B", titleMatrix)
            }

            matrix$setTitle(titleMatrix)
        },

        #### Populate tables functions ----
        .populateCorrTable = function(results) {

            matrix <- self$results$matrix
            vars <- self$options$vars
            nVars <- length(vars)
            flag <- self$options$flag
            type <- self$options$type

            if (nVars > 1) {
                for (i in 1:nVars) {
                    rowVarName <- vars[[i]]
                    for (j in 1:nVars) {

                        if (i == j || (type != 'semi' && j > i))
                            next

                        values <- list()
                        colVarName <- vars[[j]]
                        result = results[[rowVarName]][[colVarName]]

                        values[[paste0(colVarName, '[r]')]] <- result$r
                        values[[paste0(colVarName, '[rp]')]] <- result$rp
                        values[[paste0(colVarName, '[rho]')]] <- result$rho
                        values[[paste0(colVarName, '[rhop]')]] <- result$rhop
                        values[[paste0(colVarName, '[tau]')]] <- result$tau
                        values[[paste0(colVarName, '[taup]')]] <- result$taup

                        values[[paste0(colVarName, '[n]')]] <- result$n

                        matrix$setRow(rowNo=i, values)

                        if (flag) {
                            if (result$rp < .001)
                                matrix$addSymbol(rowNo=i, paste0(colVarName, '[r]'), '***')
                            else if (result$rp < .01)
                                matrix$addSymbol(rowNo=i, paste0(colVarName, '[r]'), '**')
                            else if (result$rp < .05)
                                matrix$addSymbol(rowNo=i, paste0(colVarName, '[r]'), '*')

                            if (result$rhop < .001)
                                matrix$addSymbol(rowNo=i, paste0(colVarName, '[rho]'), '***')
                            else if (result$rhop < .01)
                                matrix$addSymbol(rowNo=i, paste0(colVarName, '[rho]'), '**')
                            else if (result$rhop < .05)
                                matrix$addSymbol(rowNo=i, paste0(colVarName, '[rho]'), '*')

                            if ( ! self$options$kendall)
                            {}  # do nothing
                            else if (result$taup < .001)
                                matrix$addSymbol(rowNo=i, paste0(colVarName, '[tau]'), '***')
                            else if (result$taup < .01)
                                matrix$addSymbol(rowNo=i, paste0(colVarName, '[tau]'), '**')
                            else if (result$taup < .05)
                                matrix$addSymbol(rowNo=i, paste0(colVarName, '[tau]'), '*')
                        }
                    }
                }
            }
        },

        #### Helper functions ----
        .cleanData = function(var1, var2) {

            dataRaw <- self$data
            controls <- self$options$controls
            type <- self$options$type

            data <- list()
            data[[var1]] <- jmvcore::toNumeric(dataRaw[[var1]])
            data[[var2]] <- jmvcore::toNumeric(dataRaw[[var2]])

            if (type != 'zero') {
                for (control in controls) {
                    data[[control]] <- jmvcore::toNumeric(dataRaw[[control]])
                }
            }

            attr(data, 'row.names') <- seq_len(length(data[[1]]))
            attr(data, 'class') <- 'data.frame'
            data <- jmvcore::naOmit(data)

            return(data)
        },
        .test = function(var1, var2, controls, type) {

            data <- private$.cleanData(var1, var2)
            var1 <- data[[var1]]
            var2 <- data[[var2]]

            if (type != 'zero' && length(controls) > 0)
                controls <- data[, controls]

            results <- list()

            suppressWarnings({

                if (type == 'zero' || length(controls) == 0) {

                    res1 <- try(cor.test(var1, var2, method='pearson'))
                    res2 <- try(cor.test(var1, var2, method='spearman'))
                    res3 <- list()
                    if (self$options$kendall)
                        res3 <- try(cor.test(var1, var2, method='kendall'))

                } else if (type == "part") {

                    res1 <- try(ppcor::pcor.test(var1, var2, controls, method='pearson'))
                    res2 <- try(ppcor::pcor.test(var1, var2, controls, method='spearman'))
                    res3 <- list()
                    if (self$options$kendall)
                        res3 <- try(ppcor::pcor.test(var1, var2, controls, method='kendall'))

                } else {

                    res1 <- try(ppcor::spcor.test(var1, var2, controls, method='pearson'))
                    res2 <- try(ppcor::spcor.test(var1, var2, controls, method='spearman'))
                    res3 <- list()
                    if (self$options$kendall)
                        res3 <- try(ppcor::spcor.test(var1, var2, controls, method='kendall'))

                }

                if ( ! base::inherits(res1, 'try-error')) {
                    results$r  <- res1$estimate
                    results$rp <- res1$p.value
                } else {
                    results$r <- NaN
                    results$rp <- NaN
                }

                if ( ! base::inherits(res2, 'try-error')) {
                    results$rho <- res2$estimate
                    results$rhop <- res2$p.value
                } else {
                    results$rho <- NaN
                    results$rhop <- NaN
                }

                if ( ! base::inherits(res3, 'try-error')) {
                    results$tau <- res3$estimate
                    results$taup <- res3$p.value
                } else {
                    results$tau <- NaN
                    results$taup <- NaN
                }

                results$n <- length(data[[1]])
            }) # suppressWarnings

            results
        })
)
