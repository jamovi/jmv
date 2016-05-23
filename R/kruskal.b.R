
KruskalClass <- R6::R6Class(
    "KruskalClass",
    inherit=silkycore::Analysis,
    public=list(
        run=function() {
            super$run()

            data <- self$data
            table <- self$results$get('table')
            
            group  <- as.factor(data[[self$options$get('group')]])
            
            for (depName in self$options$get('deps')) {
                dependent <- as.numeric(data[[depName]])
                result <- kruskal.test(x=dependent, g=group)
                table$setRow(rowKey=depName, values=list(
                    chiSq=result$statistic,
                    df=result$parameter,
                    p=result$p.value
                ))
            }
        })
)

