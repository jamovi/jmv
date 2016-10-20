
AnovaClass <- R6::R6Class(
    "AnovaClass",
    inherit=jmvcore::Analysis,
    private=list(
        .model=NA,
        .init=function() {
            
            dependentName <- self$options$get('dependent')
            fixedFactors <- self$options$get('fixedFactors')
            modelTerms <- private$.modelTerms()
            
            if (is.null(dependentName) || length(fixedFactors) == 0 || length(modelTerms) == 0)
                return()
            
            data <- self$data
            for (varName in fixedFactors)
                data[[varName]] <- as.factor(data[[varName]])
            data[[dependentName]] <- jmvcore::toNumeric(data[[dependentName]])
            
            anovaTable    <- self$results$get('main')
            postHocTables <- self$results$get('postHoc')
            contrastsTables <- self$results$get('contrasts')
            
            # main table
            
            modelTerms <- private$.modelTerms()
            if (length(modelTerms) > 0) {
                for (term in modelTerms)
                    anovaTable$addRow(rowKey=term, list(name=stringifyTerm(term)))
                anovaTable$addFormat(col=1, rowNo=1,                  format=Cell.BEGIN_GROUP)
                anovaTable$addFormat(col=1, rowNo=length(modelTerms), format=Cell.END_GROUP)
            } else {
                anovaTable$addRow(rowKey='.', list(name='.'))
                anovaTable$addFormat(col=1, rowKey='.', format=Cell.BEGIN_END_GROUP)
            }
            
            anovaTable$addRow(rowKey='', list(name='Residuals'))
            anovaTable$addFormat(col=1, rowKey='', format=Cell.BEGIN_END_GROUP)
            
            
            # contrasts
            
            for (contrast in self$options$get('contrasts')) {
                if (contrast$type == 'none')
                    next()
                table <- contrastsTables$addItem(contrast)
                
                var <- data[[contrast$var]]
                levels <- base::levels(var)
                labels <- private$.contrastLabels(levels, contrast$type)
                
                for (label in labels)
                    table$addRow(rowKey=label, list(contrast=label))
            }
            
            
            # post hoc
            
            for (postHocVar in self$options$postHoc) {
                
                if (length(postHocVar) != 1)
                    next()
                
                table <- postHocTables$get(postHocVar)
                
                postHocVar <- postHocVar[[1]]
                levels <- base::levels(data[[postHocVar]])
                combs <- utils::combn(levels, 2)
                apply(combs, 2, function(comb) {
                    table$addRow(rowKey=comb, list(
                        var1=comb[1], var2=comb[2]
                    ))
                })
            }
            
            
            # descriptives
            
            descTable <- self$results$get('desc')
            factorNames <- self$options$get('fixedFactors')
            
            if (length(factorNames) > 0) {
            
                data <- select(data, rev(factorNames))
                al <- as.list(data)
                names(al) <- rev(paste0('f', seq_len(length(al))))
                ll <- sapply(al, base::levels, simplify=FALSE)
                ll$stringsAsFactors <- FALSE
                grid <- do.call(base::expand.grid, ll)
                grid <- rev(grid)
                
                for (i in seq_len(ncol(grid))) {
                    colName <- colnames(grid)[[i]]
                    descTable$addColumn(name=colName, title=factorNames[[i]], index=i)
                }
                
                for (rowNo in seq_len(nrow(grid))) {
                    row <- grid[rowNo,]
                    if ( ! is.list(row))
                        row <- list(f1=row)
                    descTable$addRow(rowKey=row, values=row)
                }
            }
            
        },
        .run=function() {
            
            suppressWarnings({
            
            dependentName <- self$options$get('dependent')
            fixedFactors <- self$options$get('fixedFactors')
            modelTerms <- private$.modelTerms()
            
            if (is.null(dependentName) || length(fixedFactors) == 0 || length(modelTerms) == 0)
                return()
            
            base::options(contrasts = c("contr.sum","contr.poly"))
            
            data <- naOmit(self$data)
            for (varName in fixedFactors)
                data[[varName]] <- as.factor(data[[varName]])
            data[[dependentName]] <- jmvcore::toNumeric(data[[dependentName]])
            
            if (is.factor(data[[dependentName]]))
                reject('Dependent variable must be numeric')
            
            for (factorName in fixedFactors) {
                lvls <- base::levels(data[[factorName]])
                if (length(lvls) == 1)
                    reject("Factor '{}' contains only a single level", factorName=factorName)
                else if (length(lvls) == 0)
                    reject("Factor '{}' contains no data", factorName=factorName)
            }
            
            for (contrast in self$options$get('contrasts')) {
               levels <- base::levels(data[[contrast$var]])
               stats::contrasts(data[[contrast$var]]) <- private$.createContrasts(levels, contrast$type)
            }
            
            formula <- jmvcore::constructFormula(dependentName, modelTerms)
            formula <- stats::as.formula(formula)
            
            private$.model <- stats::aov(formula, data)
            
            singular <- NULL
            
            if (self$options$get('ss') == '1') {
                
                results <- try(stats::anova(private$.model), silent=TRUE)
                
            } else if (self$options$get('ss') == '2') {
                
                results <- try(car::Anova(private$.model, type=2, singular.ok=FALSE), silent=TRUE)
                if (isError(results)) {
                    message <- extractErrorMessage(results)
                    if (message == 'there are aliased coefficients in the model')
                        singular <- 'Singular fit encountered; one or more predictor variables are a linear combination of other predictor variables'
                    results <- try(car::Anova(private$.model, type=2, singular.ok=TRUE), silent=TRUE)
                }
                
            } else {
                
                results <- try({
                    r <- car::Anova(private$.model, type=3, singular.ok=FALSE, silent=TRUE)
                    r <- r[-1,]
                })
                
                if (isError(results)) {
                    message <- extractErrorMessage(results)
                    if (message == 'there are aliased coefficients in the model')
                        singular <- 'Singular fit encountered; one or more predictor variables are a linear combination of other predictor variables'
                    results <- try({
                        r <- car::Anova(private$.model, type=3, singular.ok=TRUE, silent=TRUE)
                        r <- r[-1,]
                    })
                }
            }
            
            if (isError(results)) {
                message <- extractErrorMessage(results)
                if (message == 'residual df = 0')
                    reject('Residual sum of squares and/or degrees of freedom is zero, indicating a perfect fit')
            }
            
            if (results['Residuals', 'Sum Sq'] == 0 || results['Residuals', 'Df'] == 0)
                reject('Residual sum of squares and/or degrees of freedom is zero, indicating a perfect fit')
            
            anovaTable <- self$results$get('main')
            
            if ( ! is.null(singular))
                anovaTable$setNote('singular', singular)
            
            rowCount <- dim(results)[1]
            rowNames <- dimnames(results)[[1]]
            
            errIndex <- nrow(results)
            errSS <- results[errIndex,'Sum Sq']
            errDF <- results[errIndex,'Df']
            errMS <- errSS / errDF
            totalSS <- sum(results[['Sum Sq']], na.rm=TRUE)
            
            ss <- as.integer(self$options$get('ss'))
            es <- lsr::etaSquared(private$.model, ss)
            
            for (i in seq_len(rowCount)) {
                rowName <- rowNames[i]
                
                ss <- results[i,'Sum Sq']
                df <- results[i,'Df']
                ms <- ss / df
                F  <- results[i,'F value']
                p  <- results[i,'Pr(>F)']
                
                if (i <= nrow(es)) {
                    e <- es[i, 'eta.sq']
                    ep <- es[i, 'eta.sq.part']
                    w <- (df*(ms-errMS))/(totalSS+errMS)
                } else {
                    e <- ''
                    ep <- ''
                    w <- ''
                }
                
                if ( ! is.finite(ss))
                    ss <- 0
                if ( ! is.finite(ms))
                    ms <- ''
                if ( ! is.finite(F))
                    F <- ''
                if ( ! is.finite(p))
                    p <- ''
                
                tableRow <- list(ss=ss, df=df, ms=ms, F=F, p=p, etaSq=e, etaSqP=ep, omegaSq=w)
                
                if (i < rowCount) {
                    anovaTable$setRow(rowNo=i, tableRow)
                }
                else {
                    if (rowCount < anovaTable$rowCount) {
                        blankRow <- list(ss=0, df=0, ms='', F='', p='', etaSq='', etaSqP='', omegaSq='')
                        for (j in seq(i, anovaTable$rowCount-1))
                            anovaTable$setRow(rowNo=j, blankRow)
                    }
                    anovaTable$setRow(rowKey='', tableRow) # residual
                }
            }
            
            private$.populateContrasts(data)
            private$.populateLevenes(data)
            private$.prepareQQPlot(data)
            #private$.populatePostHoc(data)
            private$.prepareDescPlots(data)
            private$.populateDescriptives(data)
            
            }) # suppressWarnings
        },
        .populatePostHoc=function(data) {
            
            depName <- self$options$get('dependent')
            phNames <- self$options$get('postHoc')
            
            if (length(phNames) == 0)
                return()
            
            dep <- data[[depName]]
            
            postHocTables <- self$results$get('postHoc')
            
            mcpArgs <- list()
            
            factorNames <- self$options$get('fixedFactors')
            for (factorName in factorNames)
                mcpArgs[[factorName]] <- 'Tukey'
            
            results <- try(suppressWarnings({
                mcp <- do.call(multcomp::mcp, mcpArgs)
                r <- multcomp::glht(private$.model, mcp)
                s <- summary(r, test=multcomp::adjusted('none'))$test
                md <- s$coefficients
                se <- s$sigma
                t  <- s$tstat
                p  <- s$pvalues
                
                ptukey <- summary(r)$test$pvalues
                pbonf <- summary(r, test=multcomp::adjusted('bonferroni'))$test$pvalues
                pholm <- summary(r, test=multcomp::adjusted('holm'))$test$pvalues
                
                # from https://sites.google.com/site/tukeyhsdrcodetutorial/
                k <- private$.model$rank
                v <- private$.model$df.residual
                pscheffe <- 1-stats::pf(t**2/(k-1),k-1,v)
                
                list(md=md, se=se, t=t, p=p, ptukey=ptukey, pscheffe=pscheffe, pbonf=pbonf, pholm=pholm)
                
            }), silent=TRUE)
            
            if ( ! isError(results)) {
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
                                md=results$md[[index]],
                                se=results$se[[index]],
                                t=results$t[[index]],
                                p=results$p[[index]],
                                ptukey=results$ptukey[[index]],
                                pscheffe=results$pscheffe[[index]],
                                pbonf=results$pbonf[[index]],
                                pholm=results$pholm[[index]]
                            ))
                        }
                    }
                    
                    i <- i + nCombn
                }
            }
        },
        .populateContrasts=function(data) {
            
            contrResults <- stats::summary.lm(private$.model)[["coefficients"]]
            contrasts <- self$options$get('contrasts')
            
            for (contrast in contrasts) {
                                
                var <- contrast$var
                type <- contrast$type
                
                if (type == 'none')
                    next()
                
                levels <- base::levels(data[[var]])
                labels <- private$.contrastLabels(levels, type)
                
                table <- self$results$get('contrasts')$get(contrast)
                
                for (i in seq_along(labels)) {
                    label <- labels[[i]]
                    name <- paste0(var, i)
                    table$setRow(rowNo=i, list(
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
            
            if ( ! self$options$get('homo'))
                return()
            
            dep <- self$options$get('dependent')
            factors <- self$options$get('fixedFactors')
            rhs <- paste0('`', factors, '`', collapse=':')
            formula <- as.formula(paste0('`', dep, '`', '~', rhs))
            
            result <- car::leveneTest(formula, data, center="mean")
            
            table <- self$results$get('assump')$get('homo')

            table$setRow(rowNo=1, values=list(
                F=result[1,'F value'],
                df1=result[1,'Df'],
                df2=result[2,'Df'],
                p=result[1,'Pr(>F)']))
        },
        .populateDescriptives=function(data) {
            
            if ( ! self$options$get('descStats'))
                return()
            
            descTable <- self$results$get('desc')
            dependentName <- self$options$get('dependent')
            dependent <- data[[dependentName]]
            factorNames <- rev(self$options$get('fixedFactors'))
            factors <- as.list(select(data, factorNames))
            
            means <- aggregate(dependent, by=factors, base::mean, drop=FALSE)
            sds    <- aggregate(dependent, by=factors, stats::sd, drop=FALSE)
            ns <- aggregate(dependent, by=factors, base::length, drop=FALSE)
            
            stat <- data.frame(mean=means$x, sd=sds$x, n=ns$x)
            
            for (i in seq_len(nrow(stat))) {
                values <- stat[i,]
                values[is.na(values)] <- NaN
                descTable$setRow(rowNo=i, values)
            }
            
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
                
            } else if (type == 'repeated') {
                
                for (i in seq_len(nLevels-1))
                    labels[[i]] <- paste(levels[i], '-', levels[i+1])
                
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
            
            } else if (type == 'repeated') {
                
                contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)
                for (i in seq_len(nLevels-1)) {
                    contrast[i,  i] <- 1
                    contrast[i+1,i] <- -1
                }
                
            } else {
                
                contrast <- NULL
            }
            
            contrast
        },
        .modelTerms=function() {
            modelTerms <- self$options$get('modelTerms')
            if (length(modelTerms) == 0)
                modelTerms <- private$.ff()
            modelTerms
        },
        .ff=function() {
            fixedFactors <- self$options$get('fixedFactors')
            if (length(fixedFactors) > 1) {
                formula <- as.formula(paste('~', paste(paste0('`', fixedFactors, '`'), collapse='*')))
                terms   <- attr(stats::terms(formula), 'term.labels')
                modelTerms <- sapply(terms, function(x) as.list(strsplit(x, ':')), USE.NAMES=FALSE)
            } else {
                modelTerms <- as.list(fixedFactors)
            }
            
            for (i in seq_along(modelTerms)) {
                term <- modelTerms[[i]]
                quoted <- grepl('^`.*`$', term)
                term[quoted] <- substring(term[quoted], 2, nchar(term[quoted])-1)
                modelTerms[[i]] <- term
            }
            
            modelTerms
        },
        .prepareQQPlot=function(data) {
            
            residuals <- rstandard(private$.model)
            df <- as.data.frame(qqnorm(residuals, plot.it=FALSE))
            
            image <- self$results$get('assump')$get('qq')
            image$setState(df)
        },
        .prepareDescPlots=function(data) {
            
            depName <- self$options$get('dependent')
            groupName <- self$options$get('descPlotsHAxis')
            linesName <- self$options$get('descPlotsSepLines')
            ciWidth   <- self$options$get('ciWidth')
            
            if (length(depName) == 0 || length(groupName) == 0)
                return()
            
            by <- list()
            by[['group']] <- data[[groupName]]
            
            if ( ! is.null(linesName))
                by[['lines']] <- data[[linesName]]
            
            dep <- data[[depName]]
            
            ciMult <- qt(ciWidth / 200 + .5, nrow(data)-1)
            
            means <- aggregate(dep, by=by, mean, simplify=FALSE)
            ses   <- aggregate(dep, by=by, function(x) { sd(x) / sqrt(length(x)) }, simplify=FALSE)
            cis   <- aggregate(dep, by=by, function(x) { sd(x) / sqrt(length(x)) * ciMult }, simplify=FALSE)
            
            plotData <- data.frame(group=means$group)
            if ( ! is.null(linesName))
                plotData <- cbind(plotData, lines=means$lines)
            plotData <- cbind(plotData, mean=unlist(means$x))
            
            if (self$options$get('plotError') == 'ci')
                plotData <- cbind(plotData, err=unlist(cis$x))
            else
                plotData <- cbind(plotData, err=unlist(ses$x))
            
            image <- self$results$get('plots')
            image$setState(plotData)
            
        },
        .descPlot=function(image, ...) {
            
            if (is.null(image$state))
                return(FALSE)
            
            depName <- self$options$get('dependent')
            groupName <- self$options$get('descPlotsHAxis')
            linesName <- self$options$get('descPlotsSepLines')
            
            the <- theme(
                text=element_text(size=16, colour='#333333'),
                plot.background=element_rect(fill='transparent', color=NA),
                panel.background=element_rect(fill='#E8E8E8'),
                plot.margin=margin(15, 15, 15, 15),
                axis.text.x=element_text(margin=margin(5,0,0,0)),
                axis.text.y=element_text(margin=margin(0,5,0,0)),
                axis.title.x=element_text(margin=margin(10,0,0,0)),
                axis.title.y=element_text(margin=margin(0,10,0,0)))
            
            dodge <- position_dodge(0.1)
            
            if (self$options$get('plotError') == 'ci') {
                ciWidth <- self$options$get('ciWidth')
                errorType <- paste0('(', ciWidth, '% CI)')
            } else {
                errorType <- '(SE)'
            }
            
            if ( ! is.null(linesName)) {
                
                print(ggplot(data=image$state, aes(x=group, y=mean, group=lines, colour=lines)) +
                    geom_errorbar(aes(x=group, ymin=mean-err, ymax=mean+err, width=.1, group=lines), size=.8, position=dodge) +
                    geom_line(size=.8, position=dodge) +
                    geom_point(shape=21, fill='white', size=3, position=dodge) +
                    labs(x=groupName, y=depName, colour=paste(linesName, errorType)) +
                    the)
                
            } else {
                
                print(ggplot(data=image$state) +
                    geom_errorbar(aes(x=group, ymin=mean-err, ymax=mean+err, colour='colour', width=.1), size=.8) +
                    geom_point(aes(x=group, y=mean, colour='colour'), shape=21, fill='white', size=3) +
                    labs(x=groupName, y=depName, colour=paste(depName, errorType)) +
                    scale_colour_manual(name=paste(depName, errorType), values=c(colour='#333333'), labels='') +
                    the
                )
            }
            
            TRUE
        },
        .qqPlot=function(image, ...) {
            
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
            
            print(ggplot2::ggplot(data=image$state, aes(y=y, x=x)) +
                geom_point(aes(x=x,y=y), colour='#333333') +
                geom_abline(slope=1, intercept=0, colour='#333333') +
                ggtitle("Q-Q Plot") +
                xlab("Theoretical Quantiles") +
                ylab("Standardized Residuals") +
                the)
            
            TRUE
        },
        .sourcifyOption = function(option) {
            
            name <- option$name
            value <- option$value
            
            if (name == 'contrasts') {
                i <- 1
                while (i <= length(value)) {
                    item <- value[[i]]
                    if (item$type == 'none')
                        value[[i]] <- NULL
                    else
                        i <- i + 1
                }
                if (length(value) == 0)
                    return('')
            } else if (name == 'modelTerms') {
                if (base::identical(as.list(value), private$.ff()))
                    return('')
            }
            
            super$.sourcifyOption(option)
        })
)

