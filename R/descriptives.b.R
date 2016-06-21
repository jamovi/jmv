
DescriptivesClass <- R6::R6Class("DescriptivesClass",
    inherit=silkycore::Analysis,
    private=list(
        .run=function() {
            
            dataset <- self$options$dataset()
            
            desc <- self$results$get("descriptives")
            freq <- self$results$get("frequencies")
            
            for (i in seq_along(self$options$get("vars"))) {
                
                name   <- self$options$values()$vars[[i]]
                column <- dataset[[name]]
                total <- length(column)
                column <- na.omit(column)
                
                desc$setCell(rowNo=i, "n", length(column))
                desc$setCell(rowNo=i, "missing", total - length(column))
                
                if (silkycore::canBeNumeric(column)) {
                    
                    numColumn <- silkycore::toNumeric(column)
                    
                    desc$setCell(rowNo=i, "mean", mean(numColumn))
                    desc$setCell(rowNo=i, "median", median(numColumn))
                    
                    mode <- as.numeric(names(table(numColumn)[table(numColumn)==max(table(numColumn))]))
                    desc$setCell(rowNo=i, "mode", mode[1])
                    
                    desc$setCell(rowNo=i, "sum", sum(numColumn))
                    desc$setCell(rowNo=i, "sd", sd(numColumn))
                    desc$setCell(rowNo=i, "variance", var(numColumn))
                    desc$setCell(rowNo=i, "range", max(numColumn)-min(numColumn))
                    desc$setCell(rowNo=i, "min", min(numColumn))
                    desc$setCell(rowNo=i, "max", max(numColumn))
                    desc$setCell(rowNo=i, "se", sqrt(var(numColumn)/length(numColumn)))
                    
                    deviation <- numColumn-mean(numColumn)
                    desc$setCell(rowNo=i, "skew", sum(deviation^3)/(length(numColumn)*sd(numColumn)^3))
                    desc$setCell(rowNo=i, "kurt", sum(deviation^4)/(length(numColumn)*var(numColumn)^2))
                    
                    desc$setCell(rowNo=i, "quart1", quantile(numColumn, c(.25)))
                    desc$setCell(rowNo=i, "quart2", quantile(numColumn, c(.5)))
                    desc$setCell(rowNo=i, "quart3", quantile(numColumn, c(.75)))
                    
                } else {
                    
                    mode <- NULL
                    
                    desc$setRow(rowNo=i, values=list(
                        mean='', median='', mode='', sum='', sd='', variance='',
                        range='', min='', max='', se='', skew='', kurt='',
                        quart1='', quart2='', quart3=''))
                }
                
                cumCount <- 0
                levels <- base::levels(column)
                freqTable <- freq$get(name)
                
                for (j in seq_along(levels)) {
                    
                    count <- sum(column == levels[j])
                    cumCount <- cumCount + count
                    
                    freqTable$setCell(rowNo=j, "counts", count)
                    freqTable$setCell(rowNo=j, "percentage", 100*count/length(column))
                    freqTable$setCell(rowNo=j, "cumpercentage", 100*cumCount/length(column))
                }
                
                if (length(mode) > 1)
                    desc$addFootnote(rowNo=i, "mode", "More than one mode exists, only the first is reported")
                
            }
        },
        .plotCorr=function(image, ...) {
            print("rendering")
            hist(rnorm(200))
        }
    )
)

