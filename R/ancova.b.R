
AncovaClass <- R6::R6Class(
    "AncovaClass",
    inherit=silkyR::AnovaClass,
    public=list(),
    private=list(
        .init=function() {
            super$.init()
        },
        .run=function() {
            super$.run()
        }
    ))

