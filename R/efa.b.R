
efaClass <- R6::R6Class(
    "efaClass",
    inherit = pcaClass,
    private = list(
        analysis = 'efa',
        .init=function() {

            super$.init()

            self$results$setTitle('Exploratory Factor Analysis')
            self$results$loadings$setTitle('Factor Loadings')
            self$results$factorStats$setTitle('Factor Statistics')
        }
    ),
    public = list(
        asSource=function() {
            paste0(private$.package, '::', 'efa', '(', private$.asArgs(), ')')
        }
    )
)
