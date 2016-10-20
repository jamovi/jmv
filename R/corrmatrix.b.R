
# This file is an automatically generated template, it will not be subsequently
# overwritten by the compiler, and may be edited

CorrMatrixClass <- R6::R6Class(
  "CorrMatrixClass",
  inherit=jmvcore::Analysis,
  private=list(
    .init=function() {
        matrix <- self$results$get('matrix')
        vars <- self$options$get('vars')
        nVars <- length(vars)
        ciw <- self$options$get('ciWidth')
        
        for (i in seq_along(vars)) {
            var <- vars[[i]]
            
            matrix$addColumn(paste0(var, '[r]'), def=list(
                title=var, type='number', format='zto', visible='(pearson)'))
            matrix$addColumn(paste0(var, '[rp]'), def=list(
                title=var, type='number', format='zto,pvalue', visible='(pearson && sig)'))
            matrix$addColumn(paste0(var, '[rciu]'), def=list(
                title=var, type='number', format='zto', visible='(pearson && ci)'))
            matrix$addColumn(paste0(var, '[rcil]'), def=list(
                title=var, type='number', format='zto', visible='(pearson && ci)'))
            matrix$addColumn(paste0(var, '[rho]'), def=list(
                title=var, type='number', format='zto', visible='(spearman)'))
            matrix$addColumn(paste0(var, '[rhop]'), def=list(
                title=var, type='number', format='zto,pvalue', visible='(spearman && sig)'))
            matrix$addColumn(paste0(var, '[tau]'), def=list(
                title=var, type='number', format='zto', visible='(kendall)'))
            matrix$addColumn(paste0(var, '[taup]'), def=list(
                title=var, type='number', format='zto,pvalue', visible='(kendall && sig)'))
            
            values <- list()
            
            for (j in seq(1, i)) {
                v <- vars[[j]]
                values[[paste0(v, '[r]')]] <- ''
                values[[paste0(v, '[rp]')]] <- ''
                values[[paste0(v, '[rciu]')]] <- ''
                values[[paste0(v, '[rcil]')]] <- ''
                values[[paste0(v, '[rho]')]] <- ''
                values[[paste0(v, '[rhop]')]] <- ''
                values[[paste0(v, '[tau]')]] <- ''
                values[[paste0(v, '[taup]')]] <- ''
            }
            
            values[[paste0(var, '[r]')]] <- '\u2014'
            values[[paste0(var, '[rp]')]] <- '\u2014'
            values[[paste0(var, '[rciu]')]] <- '\u2014'
            values[[paste0(var, '[rcil]')]] <- '\u2014'
            values[[paste0(var, '[rho]')]] <- '\u2014'
            values[[paste0(var, '[rhop]')]] <- '\u2014'
            values[[paste0(var, '[tau]')]] <- '\u2014'
            values[[paste0(var, '[taup]')]] <- '\u2014'
            
            values[['.stat[rciu]']] <- paste0(ciw, '% CI Upper')
            values[['.stat[rcil]']] <- paste0(ciw, '% CI Lower')
            
            matrix$setRow(rowKey=var, values)
        }
        
        hyp <- self$options$get('hypothesis')
        flag <- self$options$get('flag')
        
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
        
    },
    .run=function() {

        matrix <- self$results$get('matrix')
        vars <- self$options$get('vars')
        nVars <- length(vars)
        
        hyp <- self$options$get('hypothesis')
        flag <- self$options$get('flag')
        
        if (hyp == 'pos')
            hyp <- 'greater'
        else if (hyp == 'neg')
            hyp <- 'less'
        else
            hyp <- 'two.sided'
        
        if (nVars > 1) {
            for (i in 1:(nVars-1)) {
                rowVarName <- vars[[i]]
                rowVar <- jmvcore::toNumeric(self$data[[rowVarName]])
                for (j in seq(i+1, nVars)) {
                    values <- list()
                    
                    colVarName <- vars[[j]]
                    colVar <- jmvcore::toNumeric(self$data[[colVarName]])
                    
                    result <- private$.test(rowVar, colVar, hyp)
                    
                    values[[paste0(colVarName, '[r]')]] <- result$r
                    values[[paste0(colVarName, '[rp]')]] <- result$rp
                    values[[paste0(colVarName, '[rciu]')]] <- result$rciu
                    values[[paste0(colVarName, '[rcil]')]] <- result$rcil
                    values[[paste0(colVarName, '[rho]')]] <- result$rho
                    values[[paste0(colVarName, '[rhop]')]] <- result$rhop
                    values[[paste0(colVarName, '[tau]')]] <- result$tau
                    values[[paste0(colVarName, '[taup]')]] <- result$taup
                    
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
                        
                        if (result$taup < .001)
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
    .test=function(var1, var2, hyp) {
        results <- list()
        
        res <- try(cor.test(var1, var2, alternative=hyp, method='pearson'))
        if ( ! base::inherits(res, 'try-error')) {
            results$r  <- res$estimate
            results$rp <- res$p.value
            results$rciu <- res$conf.int[2]
            results$rcil <- res$conf.int[1]
        }
        else {
            results$r <- NaN
            results$rp <- NaN
            results$rciu <- NaN
            results$rcil <- NaN
        }
        
        res <- try(cor.test(var1, var2, alternative=hyp, method='spearman'))
        if ( ! base::inherits(res, 'try-error')) {
            results$rho <- res$estimate
            results$rhop <- res$p.value
        }
        else {
            results$rho <- NaN
            results$rhop <- NaN
        }
        
        res <- try(cor.test(var1, var2, alternative=hyp, method='kendall'))
        if ( ! base::inherits(res, 'try-error')) {
            results$tau <- res$estimate
            results$taup <- res$p.value
        }
        else {
            results$tau <- NaN
            results$taup <- NaN
        }
        
        results
    },
    .plot=function(image, ...) {
        
        columns <- unlist(self$options$get('vars'))
        
        if (length(columns) == 0)
            return(FALSE)
        
        smooth <- GGally::wrap(GGally::ggally_smooth, size = 1, alpha=.3)
        
        lower <- list(
            continuous=smooth,
            combo="facethist",
            discrete="facetbar",
            na = "na")
        
        if (self$options$get('plotStats'))
            upper <- list(
                continuous="cor",
                combo="box",
                discrete="facetbar",
                na="na")
        else
            upper <- NULL
        
        if (self$options$get('plotDens'))
            diag <- list(
                continuous="densityDiag",
                discrete="barDiag",
                na="naDiag")
        else
            diag <- NULL
        
        data <- jmvcore::select(self$data, columns)
        names(data) <- paste0('f', seq_along(columns))
        
        for (i in seq_along(columns))
            data[[i]] <- jmvcore::toNumeric(data[[i]])
        
        print(GGally::ggpairs(
            data,
            columnLabels=columns,
            lower=lower,
            upper=upper,
            diag=diag))
        
        TRUE
    })
)

