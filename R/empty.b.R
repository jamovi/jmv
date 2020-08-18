emptyClass <- R6::R6Class(
    "emptyClass",
    inherit = emptyBase,
    private = list(
        .run = function() {
        }),
    public = list(
        asSource = function() ''
    )
)
