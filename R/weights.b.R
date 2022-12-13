
weightsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "weightsClass",
    inherit = weightsBase,
    private = list(
        .init = function() {
            if (is.null(self$options$weights)) {
                self$results$text$setContent('Data is unweighted')
            } else {
                self$results$text$setContent(paste0('Data is weighted by the variable <em>', self$options$weights,'</em>'))
            }
            self$setStatus('complete')
        })
)
