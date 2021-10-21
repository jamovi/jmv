
#' @importFrom jmvcore .
anovaClass <- R6::R6Class(
    "anovaClass",
    inherit = ancovaClass,
    private = list(
        .init=function() {
            super$.init()
            self$results$setTitle(.('ANOVA'))
            self$results$main$setTitle(self$options$eval('`ANOVA - ${dep}`'))
            self$results$residsOV$setDescription(.('Residuals from ANOVA'))
        }
    ),
    public = list(
        asSource=function() {
            paste0(private$.package, '::', 'ANOVA', '(', private$.asArgs(), ')')
        },
        initialize=function(...) {
            super$initialize(...)
            private$.name <- 'anova'
        }
    )
)
