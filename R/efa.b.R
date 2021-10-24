
#' @importFrom jmvcore .
efaClass <- R6::R6Class(
    "efaClass",
    inherit = pcaClass,
    private = list(
        analysis = 'efa',
        .init=function() {
            super$.init()

            self$results$setTitle(.('Exploratory Factor Analysis'))
            self$results$loadings$setTitle(.('Factor Loadings'))
            self$results$factorStats$setTitle(.('Factor Statistics'))

            self$results$loadings$getColumn('pc1')$setSuperTitle(.('Factor'))
            self$results$factorStats$factorSummary$getColumn('comp')$setTitle(.('Factor'))

            ie <- self$results$get('eigen')$get('initEigen')
            column <- ie$getColumn('varProp')
            column$visible <- FALSE
            column <- ie$getColumn('varCum')
            column$visible <- FALSE
            column <- ie$getColumn('comp')
            column$setTitle(.('Factor'))
        }
    ),
    public = list(
        asSource=function() {
            paste0(private$.package, '::', 'efa', '(', private$.asArgs(), ')')
        },
        initialize=function(...) {
            super$initialize(...)
            private$.name <- 'efa'
        }
    )
)
