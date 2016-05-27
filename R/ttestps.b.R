
TTestPSClass <- R6Class("TTestPSClass",
    inherit=silkycore::Analysis,
    private=list(
        .run=function() {
            
            data <- self$options$dataset()
            
            ttestTable <- self$results$get('ttest')
            descTable <- self$results$get('desc')
            normTable <- self$results$get('norm')
            
            if (self$options$get('miss') == 'listwise')
                data <- na.omit(data)
            
            if (self$options$values()$hypothesis == "oneGreater")
                Ha <- "greater"
            else if (self$options$values()$hypothesis == "twoGreater")
                Ha <- "less"
            else
                Ha <- "two.sided"
            
            confInt <- self$options$values()$ciWidth/100
            
            pairs <- self$options$get('pairs')
            
            for (pair in pairs) {
                
                name1 <- pair$i1
                name2 <- pair$i2
                
                if (is.null(name1) || is.null(name2))
                    next()
                
                if (self$options$get('miss') == "perAnalysis") {
                    pairsData <- na.omit(data[c(name1, name2)])
                    column1 <- pairsData[[name1]]
                    column2 <- pairsData[[name2]]
                } else {
                    column1 <- data[[name1]]
                    column2 <- data[[name2]]
                }
                
                var1 <- var(column1)
                var2 <- var(column2)
                n    <- length(column1)
                m1   <- mean(column1)
                m2   <- mean(column2)
                se1  <- sqrt(var1/n)
                se2  <- sqrt(var2/n)
                
                pooledSD <- stats::sd(column1-column2)
                sediff <- pooledSD/sqrt(n)
                d <- (m1-m2)/pooledSD #Cohen's d
                
                stud <- t.test(column1, column2, paired=TRUE, conf.level=confInt, alternative=Ha)
                wilc <- suppressWarnings(wilcox.test(column1, column2, alternative=Ha, paired=TRUE, conf.int=TRUE, conf.level=confInt))
                
                ttestTable$setRow(rowKey=pair, list(
                    name1=name1,
                    name2=name2,
                    'stat[stud]'=stud$statistic,
                    'df[stud]'=stud$parameter,
                    'p[stud]'=stud$p.value,
                    'md[stud]'=stud$estimate,
                    'sed[stud]'=sediff,
                    'es[stud]'=d,
                    'cil[stud]'=stud$conf.int[1],
                    'ciu[stud]'=stud$conf.int[2],
                    'stat[wilc]'=wilc$statistic,
                    'df[wilc]'=wilc$parameter,
                    'p[wilc]'=wilc$p.value,
                    'md[wilc]'=wilc$estimate,
                    'sed[wilc]'=sediff,
                    'es[wilc]'=d,
                    'cil[wilc]'=wilc$conf.int[1],
                    'ciu[wilc]'=wilc$conf.int[2]))
                
            
                 ## Normality test table
                
                 res <- NULL
                 
                 w <- NaN
                 p <- NA
                 
                 if (n < 3 || n > 5000) {
                     
                    if (n > 5000)
                        normTable$addFootnote(rowKey=pair, 'w', "Too many observations (N > 5000) to compute statistic")
                    else
                        normTable$addFootnote(rowKey=pair, 'w', "Too few observations (N < 3) to compute statistic")
                    
                 } else if ((column1[n]-column2[n])-(column1[1L]-column2[1L]) == 0) {
                     
                     normTable$addFootnote(rowKey=pair, 'w', "Unable to compute: variance is zero (variables likely contain all the same value)")
                 }
                 else {
                     res <- shapiro.test(column1-column2)
                     normTable$setRow(rowKey=pair, list(
                         name1=name1, name2=name2, w=res$statistic, p=res$p.value))
                 }
                
                descTable$setRow(rowKey=pair, list(
                    name1=name1,
                    name2=name2,
                    num=n,
                    m1=base::mean(column1),
                    m2=base::mean(column2),
                    sd1=stats::sd(column1),
                    sd2=stats::sd(column2),
                    se1=stats::var(column1),
                    se2=stats::var(column2)))
            }
        }
    )
)

