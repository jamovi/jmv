#' Update reference levels for a set of variables
#'
#' @param data The data frame containing the variables
#' @param vars The names of the variables to update
#' @param refLevels A list of reference levels to use for each variable
#' @return A list of updated reference levels and a list of variables that
#'   had their reference levels changed
#' @keywords internal
getReferenceLevels = function(data, vars, refLevels) {
    updatedRefLevels <- list()
    changedVars <- c()

    # Create a named list from the refLevels input for easier access
    refLevelsList <- stats::setNames(
        lapply(refLevels, function(ref) ref$ref),
        sapply(refLevels, function(ref) ref$var)
    )

    for (var in vars) {
        factorLevels <- levels(data[[var]])
        refLevel <- refLevelsList[[var]]

        # If no refLevel is provided or the provided level is invalid, use the first level
        if (is.null(refLevel) || ! (refLevel %in% factorLevels)) {
            refLevel <- factorLevels[1]
            changedVars <- c(changedVars, var)
        }

        updatedRefLevels[[ length(updatedRefLevels) + 1 ]] <- list(
            var = var, ref = refLevel
        )
    }

    return(list(refLevels=updatedRefLevels, changedVars=changedVars))
}


#' Set a warning notice for reference level changes
#'
#' @param self The analysis object
#' @param changedVars The variables that had their reference levels changed
#' @keywords internal
setRefLevelWarning = function(self, changedVars) {
    message <- jmvcore::format(
        .("The specified reference level was not found for the following variable(s): {vars}. Defaulting to the first available level. To use a custom reference level, ensure the defined reference level is available in the data."),
        vars=listItems(self, changedVars)
    )

    setAnalysisNotice(
        self, message, name="refLevelWarning", type=jmvcore::NoticeType$STRONG_WARNING
    )
}
