
anovaClass <- R6::R6Class(
    "anovaClass",
    inherit = ancovaClass,
    private = list(
        .init=function() {

            super$.init()

            self$results$setTitle('ANOVA')

            anovaTable <- self$results$main
            anovaTable$setTitle('ANOVA')
        }
    ),
    public = list(
        asSource=function() {
            paste0(private$.package, '::', 'anova', '(', private$.asArgs(), ')')
        }
    )
)
