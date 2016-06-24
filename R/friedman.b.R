
FriedmanClass <- R6::R6Class(
  "FriedmanClass",
  inherit=silkycore::Analysis,
  private=list(
    .run=function() {

        measureNames <- self$options$get('measures')
        
        if (length(measureNames) < 2)
            return()
        
        data <- select(self$data, measureNames)
        data <- silkycore::naOmit(data)
        mat <- matrix(nrow=nrow(data), ncol=length(measureNames), dimnames=list(NULL, measureNames))
        
        for (i in seq_along(measureNames)) {
            name <- measureNames[[i]]
            mat[,i] <- silkycore::toNumeric(data[[name]])
        }

        result <- friedman.test(mat)
        
        table <- self$results$get('table')
        table$setRow(rowNo=1, list(
            stat=unname(result$statistic),
            df=unname(result$parameter),
            p=unname(result$p.value)))
        
        if (self$options$get('pairs')) {
            
            table  <- self$results$get('comp')
            result <- PMCMR::posthoc.durbin.test(mat, p.adjust='none')
            
            n <- length(measureNames)-1
            rowNo <- 1
            for (j in 1:n) {
                for (k in j:n) {
                    table$setRow(rowNo=rowNo, list(
                        stat=result$statistic[k,j],
                        p=result$p.value[k,j]
                    ))
                    rowNo <- rowNo + 1
                }
            }
        }
        
    },
    .init=function() {
        
        measureNames <- self$options$get('measures')
        
        if (length(measureNames) < 2)
            return()
        
        compTable <- self$results$get('comp')
        
        combns <- combn(measureNames, 2)
        
        for (i in seq_len(ncol(combns))) {
            compTable$addRow(rowKey=combns[,i], values=list(
                i1=combns[1,i],
                i2=combns[2,i]))
        }
    })
)

