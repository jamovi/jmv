
htmlClass <- R6::R6Class(
    "htmlClass",
    inherit = htmlBase,
    private = list(
        .run = function() {
            self$results$html$setContent(self$options$content)
        })
)
