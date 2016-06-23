
AnovaClass <- R6::R6Class(
    "AnovaClass",
    inherit=silkycore::Analysis,
    private=list(
        .model=NA,
        .init=function() {

            fixedFactors  <- self$options$get('fixedFactors')
            
            anovaTable    <- self$results$get('anova')
            postHocTables <- self$results$get('postHoc')
            
            modelTerms <- private$.modelTerms()
            if (length(modelTerms) > 0) {
                for (term in modelTerms)
                    anovaTable$addRow(rowKey=term, list(name=paste(term, collapse=' \u273B ')))
            } else {
                anovaTable$addRow(rowKey='.', list(name='.'))
            }
            anovaTable$addRow(rowKey='', list(name='Residuals'))
            
            for (postHocVar in self$options$get('postHoc')) {
                table <- postHocTables$get(postHocVar)
                levels <- base::levels(self$data[[postHocVar]])
                combs <- utils::combn(levels, 2)
                apply(combs, 2, function(comb) {
                    table$addRow(rowKey=comb, list(
                        var1=comb[1], var2=comb[2]
                    ))
                })
            }
        },
        .run=function() {
            
            dependentName <- self$options$get('dependent')
            fixedFactors <- self$options$get('fixedFactors')
            modelTerms <- private$.modelTerms()
            
            if (is.null(dependentName) || length(fixedFactors) == 0 || length(modelTerms) == 0)
                return()
            
            data <- self$data
            
            for (factorName in fixedFactors)
                data[[factorName]] <- as.factor(data[[factorName]])
            
            data[[dependentName]] <- silkycore::toNumeric(data[[dependentName]])
            
            for (contrast in self$options$get('contrasts')) {
                base::options(contrasts=c("contr.sum","contr.poly"))
                levels <- base::levels(data[[contrast$var]])
                stats::contrasts(data[[contrast$var]]) <- private$.createContrasts(levels, contrast$type)
            }
            
            formula <- silkycore::constructFormula(self$options$get('dependent'), private$.modelTerms())
            formula <- stats::as.formula(formula)
            private$.model <- stats::aov(formula, data)
            
            if (self$options$get('sumOfSqu') == "Type I") {
                results <- stats::anova(private$.model)
            } else if (self$options$get('sumOfSqu') == "Type II") {
                results <- car::Anova(private$.model, type='2', singular.ok=TRUE)
            } else {
                results <- car::Anova(private$.model, type='3', singular.ok=TRUE)
                results <- results[-1,]
            }
            
            anovaTable <- self$results$get('anova')
            rowCount <- dim(results)[1]
            rowNames <- dimnames(results)[[1]]
            
            for (i in 1:rowCount) {
                rowName <- rowNames[i]
                
                ss <- results[i,'Sum Sq']
                df <- results[i,'Df']
                ms <- results[i,'Sum Sq'] / results[i,'Df']
                F  <- results[i,'F value']
                p  <- results[i,'Pr(>F)']
                
                if ( ! is.finite(ss))
                    ss <- ''
                if (df == 0)
                    df <- ''
                if ( ! is.finite(ms))
                    ms <- ''
                if ( ! is.finite(F))
                    F <- ''
                if ( ! is.finite(p))
                    p <- ''
                
                tableRow <- list(ss=ss, df=df, ms=ms, F=F, p=p)
                anovaTable$setRow(rowNo=i, tableRow)
            }
            
            #private$.populateContrasts(data)
            private$.populateLevenes(data)
            private$.populatePostHoc(data)
        },
        .populatePostHoc=function(data) {
            
            depName <- self$options$get('dependent')
            phNames <- self$options$get('postHoc')
            dep <- data[[depName]]
            
            postHocTables <- self$results$get('postHoc')
            
            mcpArgs <- list()
            
            factorNames <- self$options$get('fixedFactors')
            for (factorName in factorNames)
                mcpArgs[[factorName]] <- 'Tukey'
            
            mcp <- do.call(multcomp::mcp, mcpArgs)
            results <- summary(multcomp::glht(private$.model, mcp))$test

            i <- 1
            for (factorName in factorNames) {
                
                factor <- data[[factorName]]
                levels <- base::levels(factor)
                nCombn <- dim(combn(levels, 2))[2]
                
                if (factorName %in% phNames) {
                    table <- postHocTables$get(factorName)
                    for (j in seq_len(nCombn)) {
                        index <- i + j - 1
                        table$setRow(rowNo=j, list(
                            md=results$coefficients[index],
                            se=results$sigma[index],
                            t=results$tstat[index],
                            p=results$pvalues[index]
                        ))
                    }
                }
                
                i <- i + nCombn
            }
        },
        .populateContrasts=function(data) {
            
            contrResults <- stats::summary.lm(private$.model)[["coefficients"]]
            contrasts <- self$options$get('contrasts')
            
            for (contrast in contrasts) {
                
                var <- contrast$var
                type <- contrast$type
                levels <- base::levels(data[[var]])
                labels <- private$.contrastLabels(levels, type)
                
                table <- self$results$get('contrasts')$get(var)
                
                for (i in seq_along(labels)) {
                    label <- labels[[i]]
                    name <- paste0(var, i)
                    table$addRow(rowKey=i, list(
                        contrast=label,
                        est=contrResults[name, "Estimate"],
                        se=contrResults[name, "Std. Error"],
                        t=contrResults[name, "t value"],
                        p=contrResults[name, "Pr(>|t|)"]
                    ))
                }
            }
        },
        .populateLevenes=function(data) {
            
            if ( ! self$options$get('homoTests'))
                return()
            
            dep <- self$options$get('dependent')
            factors <- self$options$get('fixedFactors')
            rhs <- paste0('`', factors, '`', collapse=':')
            formula <- as.formula(paste0('`', dep, '`', '~', rhs))
            
            result <- car::leveneTest(formula, data, center="mean")
            
            table <- self$results$get('assump')$get('eqVar')

            table$setRow(rowNo=1, values=list(
                F=result[1,'F value'],
                df1=result[1,'Df'],
                df2=result[2,'Df'],
                p=result[1,'Pr(>F)']))
        },
        .contrastLabels=function(levels, type) {
            
            nLevels <- length(levels)
            labels <- list()
            
            if (length(levels) <= 1) {
                
                # do nothing
                
            } else if (type == 'simple') {
            
                for (i in seq_len(nLevels-1))
                    labels[[i]] <- paste(levels[i+1], '-', levels[1])
                
            } else if (type == 'deviation') {
                
                all <- paste(levels, collapse=', ')
                for (i in seq_len(nLevels-1))
                    labels[[i]] <- paste(levels[i+1], '-', all)
                
            } else if (type == 'difference') {
                
                for (i in seq_len(nLevels-1)) {
                    rhs <- paste0(levels[1:i], collapse=', ')
                    labels[[i]] <- paste(levels[i + 1], '-', rhs)
                }
                
            } else if (type == 'helmert') {
                
                for (i in seq_len(nLevels-1)) {
                    rhs <- paste(levels[(i+1):nLevels], collapse=', ')
                    labels[[i]] <- paste(levels[i], '-', rhs)
                }
                
            } else if (type == 'polynomial') {
                
                names <- c('linear', 'quadratic', 'cubic', 'quartic', 'quintic', 'sextic', 'septic', 'octic')
                
                for (i in seq_len(nLevels-1)) {
                    if (i <= length(names)) {
                        labels[[i]] <- names[i]
                    } else {
                        labels[[i]] <- paste('degree', i, 'polynomial')
                    }
                }
            }
            
            labels
        },
        .createContrasts=function(levels, type) {
            
            nLevels <- length(levels)
            
            if (type == 'simple') {
                
                dummy <- contr.treatment(levels)
                dimnames(dummy) <- NULL
                coding <- matrix(rep(1/nLevels, prod(dim(dummy))), ncol=nLevels-1)
                contrast <- (dummy - coding) * nLevels
                
            } else if (type == 'deviation') {
                
                contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)
                
                for (i in seq_len(nLevels-1)) {
                    contrast[i+1, i] <- -1
                    contrast[1, i] <- 1
                }
                
            } else if (type == 'difference') {
                
                contrast <- stats::contr.helmert(levels)
                dimnames(contrast) <- NULL
                
            } else if (type == 'helmert') {
                
                contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)
                
                for (i in seq_len(nLevels-1)) {
                    p <- (1 / (nLevels - i + 1))
                    contrast[i,i] <- p * (nLevels - i)
                    contrast[(i+1):nLevels,i] <- -p
                }
                
                contrast
                
            } else if (type == 'polynomial') {
                
                contrast <- stats::contr.poly(levels)
                dimnames(contrast) <- NULL
            } else {
                
                contrast <- NULL
            }
            
            contrast
        },
        .modelTerms=function() {
            modelTerms <- self$options$get('modelTerms')
            if (length(modelTerms) == 0) {
                fixedFactors <- self$options$get('fixedFactors')
                if (length(fixedFactors) > 0) {
                    formula <- as.formula(paste('~', paste(paste0('`', fixedFactors, '`'), collapse='*')))
                    terms <- attr(stats::terms(formula), 'term.labels')
                    modelTerms <- sapply(terms, function(x) as.list(strsplit(x, ':')), USE.NAMES=FALSE)
                } else {
                    modelTerms <- list()
                }
            }
            return(modelTerms)
        },
        .plot=function(name, key) {
            
            depName <- self$options$get('dependent')
            groupName <- self$options$get('descPlotsHAxis')
            linesName <- self$options$get('descPlotsSepLines')
            
            if (length(depName) == 0 || length(groupName) == 0)
                return(FALSE)
            
            data <- self$options$dataset()
            dep  <- silkycore::toNumeric(data[[depName]])
            group <- as.factor(data[[groupName]])
            
            by <- list()
            by[['group']] <- group
            
            if ( ! is.null(linesName)) {
                lines <- as.factor(data[[linesName]])
                by[['lines']] <- lines
            }
            
            means <- aggregate(dep, by=by, mean, simplify=FALSE)
            ses   <- aggregate(dep, by=by, function(x) { sd(x) / sqrt(length(x)) }, simplify=FALSE)
            
            plotData <- data.frame(group=means$group)
            if ( ! is.null(linesName))
                plotData <- cbind(plotData, lines=means$lines)
            plotData <- cbind(plotData, mean=unlist(means$x))
            plotData <- cbind(plotData, se=unlist(ses$x))
            
            the <- theme(
                legend.justification=c(1,0),
                legend.position=c(1,0),
                text=element_text(size=16),
                plot.background=element_rect(fill='transparent', color=NA),
                panel.background=element_rect(fill='#E8E8E8'))
            
            dodge <- position_dodge(0.1)
            
            if ( ! is.null(linesName)) {
                
                print(ggplot(data=plotData, aes(x=group, y=mean, group=lines, color=lines)) +
                    geom_errorbar(aes(x=group, ymin=mean-se, ymax=mean+se, width=.1, group=lines), size=.8, position=dodge) +
                    geom_line(size=.8, position=dodge) +
                    geom_point(shape=21, fill='white', size=3, position=dodge) +
                    ylab(depName) +
                    xlab(groupName) +
                    labs(colour=linesName) +
                    the)
                
            } else {
                
                print(ggplot(data=plotData, aes(x=group, y=mean, group=group)) +
                    geom_errorbar(aes(x=group, ymin=mean-se, ymax=mean+se, width=.1), size=.8) +
                    geom_point(shape=21, fill='white', size=3) +
                    ylab(depName) +
                    xlab(groupName) +
                    the)
            }
            
            TRUE
        })
)

