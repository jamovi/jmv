
anovaClass <- R6::R6Class(
    "anovaClass",
    inherit = ancovaClass,
    private = list(
        .init=function() {
            super$.init()
            self$results$setTitle('ANOVA')
            self$results$main$setTitle('ANOVA')
        }
    ),
    public = list(
        asSource=function() {
            paste0(private$.package, '::', 'anova', '(', private$.asArgs(), ')')
        },
        initialize=function(...) {
            super$initialize(...)
            private$.name <- 'anova'
        }
    )
)
