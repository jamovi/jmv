
TTestOneSClass <- R6Class("TTestOneSClass",
    inherit=silkycore::Analysis,
    private=list(
        .run=function() {
            
            variables <- self$options$get("vars")
            
            dataset <- select(self$data, variables)
            naHandling <- self$options$get("miss")
            
            ttest <- self$results$get("ttest")
            desc <- self$results$get("descriptives")
            normality <- self$results$get("normality")
            
            testValue <- self$options$get("testValue")
            cl <- self$options$get("ciWidth")/100
            
            ## Listwise NA cleanup
            if (naHandling == "listwise")
                dataset <- naOmit(dataset)
            
            ## Hypothesis options checking
            if (self$options$get("hypothesis") == "gt") {
                Ha <- "greater"
                # Footnote message TBC
            } else if (self$options$get("hypothesis") == "lt") {
                Ha <- "less"
                # Footnote message TBC
            } else {
                Ha <- "two.sided"
            }
            
            plotData <- data.frame(stat=numeric(), cie=numeric(), type=character(), var=character())
            
            for (i in seq_along(variables)) {
                
                name <- variables[[i]]
                column <- silkycore::toNumeric(dataset[[name]])
                column <- naOmit(column)
                
                n <- length(column)
                
                m <- tryNaN(mean(column))
                med <- tryNaN(median(column))
                stdDev <- tryNaN(sd(column))
                se <- stdDev/sqrt(n)
                d <- (m-testValue)/stdDev #Cohen's d
                
                if (is.na(m))
                    m <- NaN
                if (is.na(med))
                    med <- NaN
                if (is.na(stdDev))
                    stdDev <- NaN
                if (is.na(se))
                    se <- NaN
                
                if (self$options$get("students")) {
                    
                    if (is.factor(column))
                        res <- createError('Variable is not numeric')
                    else if (any(is.infinite(column)))
                        res <- createError('Variable contains infinite values')
                    else
                        res <- try(t.test(column, mu=testValue, paired=FALSE, conf.level=cl, alternative=Ha), silent=TRUE)
                    
                    if ( ! isError(res)) {
                    
                        ttest$setRow(rowNo=i, list(
                            "stat[stud]"=res$statistic,
                            "df[stud]"=res$parameter,
                            "p[stud]"=res$p.value,
                            "md[stud]"=res$estimate - testValue,
                            "es[stud]"=d,
                            "lci[stud]"=res$conf.int[1],
                            "uci[stud]"=res$conf.int[2]))
                        
                    } else {
                        
                        ttest$setRow(rowNo=i, list(
                            "stat[stud]"=NaN,
                            "df[stud]"='',
                            "p[stud]"='',
                            "md[stud]"='',
                            "es[stud]"='',
                            "lci[stud]"='',
                            "uci[stud]"=''))
                        
                        message <- extractErrorMessage(res)
                        if (message == "not enough 'x' observations")
                            message <- 'Variable does not contain enough observations'
                        else if (message == 'data are essentially constant')
                            message <- 'All observations are tied'
                        ttest$addFootnote(rowNo=i, 'stat[stud]', message)
                    }
                }
                
                if (self$options$get("mann")) {
                    
                    if (is.factor(column))
                        res <- createError('Variable is not numeric')
                    else if (length(column) == 0)
                        res <- createError('Variable does not contain enough observations')
                    else
                        res <- try(suppressWarnings(wilcox.test(column, mu=testValue, alternative=Ha, paired=FALSE, conf.int=TRUE, conf.level=cl)), silent=TRUE)
                    
                    if ( ! isError(res)) {
                    
                        ttest$setRow(rowNo=i, list(
                            "stat[mann]"=res$statistic,
                            "p[mann]"=res$p.value,
                            "md[mann]"=res$estimate - testValue,
                            "es[mann]"=d,
                            "lci[mann]"=res$conf.int[1],
                            "uci[mann]"=res$conf.int[2]))
                        
                    } else {
                        
                        ttest$setRow(rowNo=i, list(
                            "stat[mann]"=NaN,
                            "p[mann]"='',
                            "md[mann]"='',
                            "es[mann]"='',
                            "lci[mann]"='',
                            "uci[mann]"=''))
                            
                        message <- extractErrorMessage(res)
                        if (message == 'cannot compute confidence interval when all observations are tied')
                            message <- 'All observations are tied'
                        ttest$addFootnote(rowNo=i, 'stat[mann]', message)
                    }
                }
                
                # Normality test table
                if (n < 3) {

                    normality$addFootnote(rowNo=i, "w", "Too few observations (N < 3) to compute statistic")
                    res <- list(statistic=NaN, p.value='')

                } else if (n > 5000) {

                    normality$addFootnote(rowNo=i, "w", "Too many observations (N > 5000) to compute statistic")
                    res <- list(statistic=NaN, p.value='')

                }
                else {

                    res <- try(shapiro.test(column - testValue), silent=TRUE)
                    if (isError(res))
                        res <- list(statistic=NaN, p.value='')
                }
                
                normality$setRow(rowNo=i, list(
                    w=res$statistic,
                    p=res$p.value))
                
                ## Descriptives table
                desc$setRow(rowNo=i, list(num=n, mean=m, median=med, sd=stdDev, se=se))
                
                if (self$options$get('plots')) {
                    
                    image <- self$results$get('plots')
                    
                    ciWidth <- self$options$get('ciWidth')
                    tail <- qnorm(1 - (100 - ciWidth) / 200)
                    
                    plotData <- rbind(plotData, list(
                        stat=m,
                        cie=tail * tryNaN(sd(column)) / sqrt(length(column)),
                        type='mean',
                        var=name), stringsAsFactors=FALSE)
                    plotData <- rbind(plotData, list(stat=med, cie=NA, type='median', var=name), stringsAsFactors=FALSE)
                    
                    if (all(is.na(plotData$stat)))
                        image$setState(NULL)
                    else
                        image$setState(plotData)
                }
                
                if (self$options$get('bf')) {
                    
                    if (is.factor(column)) {
                        res <- createError('Variable is not numeric')
                    } else if (any(is.infinite(column))) {
                        res <- createError('Variable contains infinite values')
                    } else {

                        if (self$options$get('hypothesis') == 'gt') {
                            nullInterval <- c(0, Inf)
                        } else if (self$options$get('hypothesis') == 'lt') {
                            nullInterval <- c(-Inf, 0)
                        } else {
                            nullInterval <- NULL
                        }
                        
                        data <- column - testValue
                        rscale <- self$options$get('bfPrior')

                        res <- try(BayesFactor::ttestBF(x=data, nullInterval=nullInterval, rscale=rscale), silent=TRUE)
                    }
                    
                    if (isError(res)) {
                        
                        ttest$setRow(rowKey=name, list(
                            "stat[bf]"=NaN,
                            "err[bf]"=''))
                        
                        message <- extractErrorMessage(res)
                        if (message == 'not enough observations')
                            message = 'Variable does not contain enough observations'
                        else if (message == 'data are essentially constant')
                            message <- 'All observations are tied'
                        ttest$addFootnote(rowKey=name, 'stat[bf]', message)
                        
                    } else {
                        
                        extracted <- BayesFactor::extractBF(res)
                        error <- extracted$error[1]
                        if (is.na(error))
                            error <- NaN
                        
                        ttest$setRow(rowKey=name, list(
                            "stat[bf]"=extracted$bf[1],
                            "err[bf]"=error))
                    }
                }
            }
        },
        .init=function() {
            
            hypothesis <- self$options$get('hypothesis')
            testValue  <- self$options$get('testValue')
            table <- self$results$get("ttest")
            
            if (hypothesis == 'dt' && testValue == 0)
                table$setNote("hyp", NULL)
            else if (hypothesis == 'dt')
                table$setNote("hyp", silkycore::format("H\u2090 population mean \u2260 {}", testValue))
            else if (hypothesis == 'gt')
                table$setNote("hyp", silkycore::format("H\u2090 population mean > {}", testValue))
            else if (hypothesis == 'lt')
                table$setNote("hyp", silkycore::format("H\u2090 population mean < {}", testValue))
        },
        .plot=function(image, ...) {
            
            if (is.null(image$state))
                return(FALSE)
            
            ciw <- self$options$get('ciWidth')
            
            plot <- ggplot(data=image$state, aes(x=var, y=stat, shape=type)) +
                geom_errorbar(aes(x=var, ymin=stat-cie, ymax=stat+cie, shape=type, width=.1), size=.8, colour='#333333') +
                geom_point(aes(x=var, y=stat, colour=type, shape=type), fill='white', size=3, colour='#333333') +
                labs(x='', y='') +
                expand_limits(y=0) +
                scale_shape_manual(name='', values=c(mean=21, median=22), labels=c(mean=paste0('Mean (', ciw, '% CI)'), median='Median')) +
                theme(
                    text=element_text(size=16, colour='#333333'),
                    plot.background=element_rect(fill='transparent', color=NA),
                    panel.background=element_rect(fill='#E8E8E8'))
            
            suppressWarnings(print(plot))
            
            return(TRUE)
        }
    )
)

