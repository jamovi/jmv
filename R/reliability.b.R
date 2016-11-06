
# This file is a generated template, your changes will not be overwritten

ReliabilityClass <- R6::R6Class(
    "ReliabilityClass",
    inherit = ReliabilityBase,
    private = list(
        .run = function() {
        	
        	if (is.null(self$options$vars))
        		return()
        	
        	items <- unlist(self$options$vars)
        	data <- list()
      
        	for (item in items) {
        		data[[item]] <- jmvcore::toNumeric(self$data[[item]])
        		if (any(is.infinite(self$data[[item]])))
        			jmvcore::reject("Item '{}' contains infinite values", code='', item)
        		if (all(is.na(self$data[[item]])))
        			jmvcore::reject("Item '{}' contains only missing values", code='', item)
        	}
        	
        	data <- as.data.frame(data)
        	data <- jmvcore::naOmit(data)
        	
        	scaleTable <- self$results$scale
        	
        	values <- list()
        	
        	resultAlpha <- psych::alpha(data, delete=FALSE, warnings=FALSE)
        	
        	values[["alpha"]] <- resultAlpha$total$raw_alpha
        	values[["mean"]] <- resultAlpha$total$mean
        	values[["sd"]] <- resultAlpha$total$sd
        	
        	if (self$options$omegaScale) {
        		resultOmega <- psych::omega(data, 1)
        		values[["omega"]] <- resultOmega$omega.tot
        		omegaItems <- private$.omegaDrop(data, items)
        	} 
        	
        	scaleTable$setRow(rowNo=1, values=values)
        	
        	itemsTable <- self$results$items
        	
        	rowNo <- 1
        	for (item in items) {
        		
        		row <- list()
        		row[["alpha"]] <- resultAlpha$alpha.drop[item,"raw_alpha"]
        		row[["mean"]] <- resultAlpha$item.stats[item,"mean"]
        		row[["sd"]] <- resultAlpha$item.stats[item,"sd"]
        		row[["itemRestCor"]] <- resultAlpha$item.stats[item,"r.drop"]
        		row[["omega"]] <- omegaItems[[item]]
				
        		itemsTable$setRow(rowNo=rowNo, values=row)
        		
        		rowNo <- rowNo + 1
        	}
        },
        .omegaDrop=function(data, items) {
        	
        	omegas <- list()
        	for (item in items)
        		omegas[[item]] <- psych::omega(data[,colnames(data) != item], 1)$omega.tot

        	omegas
        
        },
        .init=function() {
        	
        	items <- self$results$items
        	
        	items$getColumn('alpha')$setSuperTitle('if item dropped')
        	items$getColumn('omega')$setSuperTitle('if item dropped')
        })
)
