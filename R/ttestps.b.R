
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
                
                data[[name1]] <- silkycore::toNumeric(data[[name1]])
                data[[name2]] <- silkycore::toNumeric(data[[name2]])
                
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
                sd1  <- sd(column1)
                sd2  <- sd(column2)
                
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
                 
                w <- NaN
                p <- NA
                 
                if (n < 3) {
                    normTable$addFootnote(rowKey=pair, 'w', "Too few observations (N < 3) to compute statistic")
                }
                else if (n > 5000) {
                    normTable$addFootnote(rowKey=pair, 'w', "Too many observations (N > 5000) to compute statistic")
                }
                else {
                    res <- shapiro.test(column1-column2)
                    w <- res$statistic
                    p <- res$p.value
                }
                
                normTable$setRow(rowKey=pair, list(
                    name1=name1, name2=name2, w=w, p=p))
                
                
                
                row1Key <- paste0(pair$i1, i)
                row2Key <- paste0(pair$i2, i)
                
                descTable$setRow(rowKey=row1Key, list(
                    "name"=name1,
                    "num"=n,
                    "m"=m1,
                    "sd"=sd1,
                    "se"=se1))
                
                descTable$setRow(rowKey=row2Key, list(
                    "name"=name2,
                    "num"=n,
                    "m"=m2,
                    "sd"=sd2,
                    "se"=se2))
            }
        },
        .init=function() {
            descTable <- self$results$get('desc')
            pairs <- self$options$get('pairs')
            for (i in seq_along(pairs)) {
                pair <- pairs[[i]]
                row1Key <- paste0(pair$i1, i)
                row2Key <- paste0(pair$i2, i)
                descTable$addRow(row1Key)
                descTable$addRow(row2Key)
            }
        }
    )
)

