
anovaClass <- R6::R6Class(
    "anovaClass",
    inherit = ancovaClass,
    private = list(
        .init=function() {
            super$.init()
            self$results$setTitle('ANOVA')

            if ( ! is.null(self$options$dep))
                self$results$main$setTitle(paste0('ANOVA - ', self$options$dep))
            else
                self$results$main$setTitle('ANOVA - \u2026')

            self$results$resids$setDescription('Residuals from ANOVA')
            self$results$predict$setDescription('Predicted values from ANOVA')
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
