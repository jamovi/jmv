
# This file is a generated template, your changes will not be overwritten

ReliabilityClass <- R6::R6Class(
    "ReliabilityClass",
    inherit = ReliabilityBase,
    private = list(
        .run = function() {
        	
        	if (is.null(self$options$vars) || length(self$options$vars) < 2)
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
        
        	# Fill scale statistics table
        	
        	scaleTable <- self$results$scale
        	
        	resultAlpha <- psych::alpha(data, delete=FALSE, warnings=FALSE)
        	
        	values <- list()
        	values[["alpha"]] <- resultAlpha$total$raw_alpha
        	values[["mean"]] <- resultAlpha$total$mean
        	values[["sd"]] <- resultAlpha$total$sd
        	
        	if (self$options$omegaScale) {
        		resultOmega <- psych::omega(data, 1, flip = FALSE)
        		values[["omega"]] <- resultOmega$omega.tot
        	} 
        	
        	scaleTable$setRow(rowNo=1, values=values)
        	
        	private$.checkpoint()
        	
        	# Fill item statistics table
        	
        	itemsTable <- self$results$items
        	
        	for (item in items) {
        		
        		row <- list()
        		row[["alpha"]] <- resultAlpha$alpha.drop[item,"raw_alpha"]
        		row[["mean"]] <- resultAlpha$item.stats[item,"mean"]
        		row[["sd"]] <- resultAlpha$item.stats[item,"sd"]
        		row[["itemRestCor"]] <- resultAlpha$item.stats[item,"r.drop"]
				
        		itemsTable$setRow(rowKey=item, values=row)
        	}
        	
        	private$.checkpoint()
        	
        	omegaItems <- NULL
        	if (self$options$omegaItems && length(self$options$vars) > 2)
        		omegaItems <- private$.omegaDrop(data, items)
        	
        	for (item in items) 
        		itemsTable$setCell(rowKey=item, col="omega", value=omegaItems[[item]])
        	
        },
        .omegaDrop=function(data, items) {
        	
        	omegas <- list()
        	for (item in items)
        		omegas[[item]] <- psych::omega(data[,colnames(data) != item], 1, flip = FALSE)$omega.tot

        	omegas
        },
        .init=function() {
        	
        	items <- self$results$items
        	
        	items$getColumn('alpha')$setSuperTitle('if item dropped')
        	items$getColumn('omega')$setSuperTitle('if item dropped')
        })
)
