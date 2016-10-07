
TTestPSClass <- R6Class("TTestPSClass",
    inherit=jmvcore::Analysis,
    private=list(
        .run=function() {
            
            data <- self$data
            
            ttestTable <- self$results$get('ttest')
            descTable <- self$results$get('desc')
            normTable <- self$results$get('norm')
            plots     <- self$results$get('plots')
            
            if (self$options$get('miss') == 'listwise')
                data <- naOmit(data)
            
            if (self$options$get("hypothesis") == "oneGreater")
                Ha <- "greater"
            else if (self$options$get("hypothesis") == "twoGreater")
                Ha <- "less"
            else
                Ha <- "two.sided"
            
            confInt <- self$options$get("ciWidth")/100
            
            pairs <- self$options$get('pairs')
            
            for (i in seq_along(pairs)) {
                
                pair <- pairs[[i]]
                
                name1 <- pair$i1
                name2 <- pair$i2
                
                if (is.null(name1) || is.null(name2))
                    next()
                
                data[[name1]] <- jmvcore::toNumeric(data[[name1]])
                data[[name2]] <- jmvcore::toNumeric(data[[name2]])
                
                if (self$options$get('miss') == "perAnalysis") {
                    pairsData <- naOmit(data[c(name1, name2)])
                    column1 <- pairsData[[name1]]
                    column2 <- pairsData[[name2]]
                } else {
                    column1 <- data[[name1]]
                    column2 <- data[[name2]]
                }
                
                var1 <- tryNaN(var(column1))
                var2 <- tryNaN(var(column2))
                n    <- length(column1)
                m1   <- tryNaN(mean(column1))
                m2   <- tryNaN(mean(column2))
                med1 <- tryNaN(median(column1))
                med2 <- tryNaN(median(column2))
                se1  <- sqrt(var1/n)
                se2  <- sqrt(var2/n)
                sd1  <- tryNaN(stats::sd(column1))
                sd2  <- tryNaN(stats::sd(column2))
                
                pooledSD <- tryNaN(stats::sd(column1-column2))
                sediff <- pooledSD/sqrt(n)
                d <- (m1-m2)/pooledSD #Cohen's d
                
                if (is.factor(column1) || is.factor(column2)) {
                    stud <- createError('One or both variables are not numeric')
                    wilc <- createError('One or both variables are not numeric')
                }
                else {
                    stud <- try(t.test(column1, column2, paired=TRUE, conf.level=confInt, alternative=Ha), silent=TRUE)
                    wilc <- try(suppressWarnings(wilcox.test(column1, column2, alternative=Ha, paired=TRUE, conf.int=TRUE, conf.level=confInt)), silent=TRUE)
                }
                
                if ( ! isError(stud)) {
                    
                    ttestTable$setRow(rowKey=pair, list(
                        'stat[stud]'=stud$statistic,
                        'df[stud]'=stud$parameter,
                        'p[stud]'=stud$p.value,
                        'md[stud]'=stud$estimate,
                        'sed[stud]'=sediff,
                        'es[stud]'=d,
                        'cil[stud]'=stud$conf.int[1],
                        'ciu[stud]'=stud$conf.int[2]))
                        
                } else {
                    ttestTable$setRow(rowKey=pair, list(
                        'stat[stud]'=NaN,
                        'df[stud]'='',
                        'p[stud]'='',
                        'md[stud]'='',
                        'sed[stud]'='',
                        'es[stud]'='',
                        'cil[stud]'='',
                        'ciu[stud]'=''))
                    
                    message <- extractErrorMessage(stud)
                    if (message == "not enough 'x' observations")
                        message <- 'One or both variables do not contain enough observations'
                    else if (message == 'missing value where TRUE/FALSE needed')
                        message <- 'One or both variables contain infinite values'
                    
                    ttestTable$addFootnote(rowKey=pair, 'stat[stud]', message)
                }
                
                if ( ! isError(wilc)) {
                
                    ttestTable$setRow(rowKey=pair, list(
                        'stat[wilc]'=wilc$statistic,
                        'df[wilc]'=wilc$parameter,
                        'p[wilc]'=wilc$p.value,
                        'md[wilc]'=wilc$estimate,
                        'sed[wilc]'=sediff,
                        'es[wilc]'=d,
                        'cil[wilc]'=wilc$conf.int[1],
                        'ciu[wilc]'=wilc$conf.int[2]))
                    
                } else {
                    
                    ttestTable$setRow(rowKey=pair, list(
                        'stat[wilc]'=NaN,
                        'df[wilc]'='',
                        'p[wilc]'='',
                        'md[wilc]'='',
                        'sed[wilc]'='',
                        'es[wilc]'='',
                        'cil[wilc]'='',
                        'ciu[wilc]'=''))
                    
                    message <- extractErrorMessage(wilc)
                    if (message == "not enough 'x' observations")
                        message <- 'One or both variables do not contain enough observations'
                    else if (message == 'missing value where TRUE/FALSE needed')
                        message <- 'One or both variables contain infinite values'
                    else if (message == 'cannot compute confidence interval when all observations are tied')
                        message <- 'All observations are tied'
                    else if (message == "'y' must be numeric")
                        message <- 'One or both variables contain no observations'
                    
                    ttestTable$addFootnote(rowKey=pair, 'stat[wilc]', message)
                }
                
            
                ## Normality test table
                 
                w <- NaN
                p <- ''
                 
                if (n < 3) {
                    normTable$addFootnote(rowKey=pair, 'w', "Too few observations (N < 3) to compute statistic")
                }
                else if (n > 5000) {
                    normTable$addFootnote(rowKey=pair, 'w', "Too many observations (N > 5000) to compute statistic")
                }
                else {
                    res <- try(shapiro.test(column1-column2), silent=TRUE)
                    if ( ! base::inherits(res, 'try-error')) {
                        w <- res$statistic
                        p <- res$p.value
                    }
                }
                
                normTable$setRow(rowKey=pair, list(
                    name1=name1, name2=name2, w=w, p=p))
                
                row1Key <- paste0(pair$i1, i)
                row2Key <- paste0(pair$i2, i)
                
                descTable$setRow(rowKey=row1Key, list(
                    "name"=name1,
                    "num"=n,
                    "m"=m1,
                    "med"=med1,
                    "sd"=sd1,
                    "se"=se1))
                
                descTable$setRow(rowKey=row2Key, list(
                    "name"=name2,
                    "num"=n,
                    "m"=m2,
                    "med"=med2,
                    "sd"=sd2,
                    "se"=se2))
                
                descTable$addFormat(col='name', rowKey=row1Key, Cell.BEGIN_GROUP)
                descTable$addFormat(col='name', rowKey=row2Key, Cell.END_GROUP)
                
                if (self$options$get('bf')) {
                    
                    if (is.factor(column1) || is.factor(column2)) {
                        res <- createError('One or both variables are not numeric')
                    } else if (any(is.infinite(column1)) || any(is.infinite(column2))) {
                        res <- createError('One or both variables contain infinite values')
                    } else {
                        
                        if (self$options$get('hypothesis') == 'oneGreater') {
                            nullInterval <- c(0, Inf)
                        } else if (self$options$get('hypothesis') == 'twoGreater') {
                            nullInterval <- c(-Inf, 0)
                        } else {
                            nullInterval <- NULL
                        }
                        
                        rscale <- self$options$get('bfPrior')
                        
                        res <- try(BayesFactor::ttestBF(x=column1, y=column2, paired=TRUE, nullInterval=nullInterval, rscale=rscale), silent=TRUE)
                    }
                    
                    if (isError(res)) {
                        
                        ttestTable$setRow(rowKey=pair, list(
                            "stat[bf]"=NaN,
                            "err[bf]"=''))
                        
                        message <- extractErrorMessage(res)
                        if (message == 'not enough observations')
                            message = 'One or both variables do not contain enough observations'
                        ttestTable$addFootnote(rowKey=pair, 'stat[bf]', message)
                        
                    } else {
                        
                        extracted <- BayesFactor::extractBF(res)
                        error <- extracted$error[1]
                        bf    <- extracted$bf[1]
                        if (is.na(error))
                            error <- NaN
                        if ( ! is.numeric(bf))
                            bf <- NaN
                        
                        ttestTable$setRow(rowKey=pair, list(
                            "stat[bf]"=extracted$bf[1],
                            "err[bf]"=error))
                        
                        if (! is.na(bf) && bf < 1)
                            ttestTable$addFormat(col='stat[bf]', rowKey=pair, Cell.NEGATIVE)
                    }
                }
                
                if (self$options$get('plots')) {
                    
                    image <- plots$get(key=pair)
                    
                    ciWidth <- self$options$get('ciWidth')
                    tail <- qnorm(1 - (100 - ciWidth) / 200)
                    
                    means <- c(tryNaN(mean(column1)), tryNaN(mean(column2)))
                    means[is.nan(means)] <- NA
                    
                    cies  <- c(tail * tryNaN(sd(column1)) / sqrt(length(column1)), tail * tryNaN(sd(column2)) / sqrt(length(column2)))
                    medians <- c(tryNaN(median(column1)), tryNaN(median(column2)))
                    
                    meanPlotData <- data.frame(group=c(name1, name2))
                    meanPlotData <- cbind(meanPlotData, stat=means)
                    meanPlotData <- cbind(meanPlotData, cie=cies)
                    meanPlotData <- cbind(meanPlotData, type='mean')
                    
                    medianPlotData <- data.frame(group=c(name1, name2))
                    medianPlotData <- cbind(medianPlotData, stat=medians)
                    medianPlotData <- cbind(medianPlotData, cie=NA)
                    medianPlotData <- cbind(medianPlotData, type='median')
                    
                    plotData <- rbind(meanPlotData, medianPlotData)
                    plotData$group <- factor(plotData$group, levels=unique(plotData$group))
                    
                    if (all(is.na(plotData$stat)))
                        image$setState(NULL)
                    else
                        image$setState(plotData)
                }
                
            }
        },
        .init=function() {
            
            hypothesis <- self$options$get('hypothesis')
            ttestTable <- self$results$get('ttest')
            
            ciTitle <- paste0(self$options$get('ciWidth'), '% Confidence Interval')
            
            ttestTable$getColumn('ciu[stud]')$setSuperTitle(ciTitle)
            ttestTable$getColumn('cil[stud]')$setSuperTitle(ciTitle)
            ttestTable$getColumn('ciu[bf]')$setSuperTitle(ciTitle)
            ttestTable$getColumn('cil[bf]')$setSuperTitle(ciTitle)
            ttestTable$getColumn('ciu[wilc]')$setSuperTitle(ciTitle)
            ttestTable$getColumn('cil[wilc]')$setSuperTitle(ciTitle)
            
            if (hypothesis == 'oneGreater')
                ttestTable$setNote("hyp", "H\u2090 Measure 1 > Measure 2")
            else if (hypothesis == 'twoGreater')
                ttestTable$setNote("hyp", "H\u2090 Measure 1 < Measure 2")
            else
                ttestTable$setNote("hyp", NULL)
            
            
            pairs <- self$options$get('pairs')
            descTable <- self$results$get('desc')
            
            for (i in seq_along(pairs)) {
                
                pair <- pairs[[i]]
                
                ttestTable$setRow(rowKey=pair, list(
                    name1=pair$i1,
                    name2=pair$i2))
                
                row1Key <- paste0(pair$i1, i)
                row2Key <- paste0(pair$i2, i)
                descTable$addRow(row1Key)
                descTable$addRow(row2Key)
            }
        },
        .plot=function(image, ...) {
            
            if (is.null(image$state))
                return(FALSE)
            
            groupName <- self$options$get('group')
            ciw <- self$options$get('ciWidth')
            
            pd <- ggplot2::position_dodge(0.2)
            
            plot <- ggplot(data=image$state, aes(x=group, y=stat, shape=type)) +
                geom_errorbar(aes(x=group, ymin=stat-cie, ymax=stat+cie, shape=type, width=.2), size=.8, colour='#333333', position=pd) +
                geom_point(aes(x=group, y=stat, colour=type, shape=type), fill='white', size=3, colour='#333333', position=pd) +
                labs(x=groupName, y=NULL) +
                scale_shape_manual(name='', values=c(mean=21, median=22), labels=c(mean=paste0('Mean (', ciw, '% CI)'), median='Median')) +
                theme(
                    text=element_text(size=16, colour='#333333'),
                    plot.background=element_rect(fill='transparent', color=NA),
                    panel.background=element_rect(fill='#E8E8E8'),
                    axis.text.x=element_text(margin=margin(5,0,0,0)),
                    axis.text.y=element_text(margin=margin(0,5,0,0)),
                    axis.title.x=element_text(margin=margin(10,0,0,0)),
                    axis.title.y=element_text(margin=margin(0,10,0,0)))
            
            suppressWarnings(print(plot))
            
            return(TRUE)
        }
    )
)

