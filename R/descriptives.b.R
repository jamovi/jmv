
DescriptivesClass <- R6Class("DescriptivesClass",
    inherit=silkycore::Analysis,
    public=list(
        run=function() {
            
            super$run()
        
            dataset <- self$options$dataset()
            
            desc <- self$results$get("descriptives")
            freq <- self$results$get("frequencies")
            
            for (i in seq_along(self$options$values()$vars)) {
                
                name   <- self$options$values()$vars[[i]]
                column <- na.omit(dataset[[name]])
                
                if (silkycore::canBeNumeric(column)) {
                    
                    numColumn <- as.numeric(column)
                    
                    desc$setCell(i, "mean", mean(numColumn))
                    desc$setCell(i, "median", median(numColumn))
                    
                    mode <- as.numeric(names(table(numColumn)[table(numColumn)==max(table(numColumn))]))
                    desc$setCell(i, "mode", mode[1])
                    
                    desc$setCell(i, "sum", sum(numColumn))
                    desc$setCell(i, "sd", sd(numColumn))
                    desc$setCell(i, "variance", var(numColumn))
                    desc$setCell(i, "range", max(numColumn)-min(numColumn))
                    desc$setCell(i, "min", min(numColumn))
                    desc$setCell(i, "max", max(numColumn))
                    desc$setCell(i, "se", sqrt(var(numColumn)/length(numColumn)))
                    
                    deviation <- numColumn-mean(numColumn)
                    desc$setCell(i, "skew", sum(deviation^3)/(length(numColumn)*sd(numColumn)^3))
                    desc$setCell(i, "kurt", sum(deviation^4)/(length(numColumn)*var(numColumn)^2))
                    
                    desc$setCell(i, "quart1", quantile(numColumn, c(.25)))
                    desc$setCell(i, "quart2", quantile(numColumn, c(.5)))
                    desc$setCell(i, "quart3", quantile(numColumn, c(.75)))
                    
                } else {
                    
                    mode <- NULL
                    
                    desc$setCell(i, "mean", "")
                    desc$setCell(i, "median", "")
                    
                    # mode <- names(table(column)[table(column)==max(table(column))])
                    # desc$setCell(i, "mode", mode[1])
                    desc$setCell(i, "mode", "")
                    
                    desc$setCell(i, "sum", "")
                    desc$setCell(i, "sd", "")
                    desc$setCell(i, "variance", "")
                    desc$setCell(i, "range", "")
                    desc$setCell(i, "min", "")
                    desc$setCell(i, "max", "")
                    desc$setCell(i, "se", "")
                    desc$setCell(i, "skew", "")
                    desc$setCell(i, "kurt", "")
                    desc$setCell(i, "quart1", "")
                    desc$setCell(i, "quart2", "")
                    desc$setCell(i, "quart3", "")
                }
                
                cumCount <- 0
                levels <- base::levels(column)
                freqTable <- freq$get(name)
                
                for (j in seq_along(levels)) {
                    
                    count <- sum(column == levels[j])
                    cumCount <- cumCount + count
                    
                    freqTable$setCell(j, "counts", count)
                    freqTable$setCell(j, "percentage", 100*count/length(column))
                    freqTable$setCell(j, "cumpercentage", 100*cumCount/length(column))
                }
                
                if (length(mode) > 1)
                    desc$addFootnote(i, "mode", "More than one mode exists, only the first is reported")
                
            }
        }
    )
)

