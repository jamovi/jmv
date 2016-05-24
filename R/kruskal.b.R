
KruskalClass <- R6::R6Class(
    "KruskalClass",
    inherit=silkycore::Analysis,
    private=list(
        .run=function() {

            data <- self$data
            deps  <- self$options$get('deps')
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
            
            pairs <- private$.genPairs(data)
            
            for (depName in deps) {
                
                depColumn <- data[[depName]]
                table <- self$results$get('comparisons')$get(depName)
                
                sdata <- base::split(depColumn, group)
                
                for (pair in pairs) {
                    pairData <- list(sdata[[pair[1]]], sdata[[pair[2]]])
                    result <- NSM3::pSDCFlig(pairData, method="Asymptotic")
                    
                    table$setRow(rowKey=pair, list(
                        p1=pair[1],
                        p2=pair[2],
                        W=result$obs.stat,
                        p=result$p.val
                    ))
                }
                
            }
        },
        .init=function() {
            
            data <- self$data
            deps  <- self$options$get('deps')

            pairs <- private$.genPairs(data)
            
            for (depName in deps) {
                
                depColumn <- data[depName]
                depTable <- self$results$get('comparisons')$get(depName)
                
                for (pair in private$.genPairs(data)) {
                    depTable$addRow(rowKey=pair, values=list(
                        p1=pair[1],
                        p2=pair[2]))
                }
            }
            
        },
        .genPairs=function(data) {
            
            group <- self$options$get('group')
            groupColumn <- data[[group]]
            groupLevels <- base::levels(groupColumn)
            
            pairsList <- list()
            
            if (length(groupLevels) > 0) {
                pairsMatrix <- utils::combn(groupLevels, 2)
                for (i in seq_len(dim(pairsMatrix)[2]))
                    pairsList[[i]] <- pairsMatrix[,i]
            }
            
            pairsList
        })
)

