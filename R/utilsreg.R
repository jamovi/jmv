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


#' Reject the analysis if variables have fewer than two levels with observations
#'
#' Unlike `rejectEmptyLevels()` this permits unused levels, as long as at least
#' two levels are observed; the models fitted by the regression analyses cope
#' with the unused ones. Fewer than two observed levels leaves no contrast to
#' estimate, which R reports as 'contrasts can be applied only to factors with
#' 2 or more levels'.
#'
#' @param self    The analysis object (for translation)
#' @param data    The data the analysis is run on, after removing missing values
#' @param dep     The name of the dependent variable, when it is a factor
#' @param factors The names of the factors to check, as named in `data`
#' @keywords internal
rejectSingleLevelVars = function(self, data, dep=NULL, factors=NULL) {
    hasTwoLevels = function(varName) sum(table(data[[varName]]) > 0) >= 2

    if (! is.null(dep) && ! hasTwoLevels(dep)) {
        jmvcore::reject(
            .("The dependent variable '{varName}' has fewer than two levels with observations. This can happen when all observations of a level are excluded by filters or removed due to missing values."),
            code=exceptions$dataError,
            varName=dep
        )
    }

    for (factorName in factors) {
        if (! hasTwoLevels(factorName)) {
            jmvcore::reject(
                .("Factor '{varName}' has fewer than two levels with observations. This can happen when all observations of a level are excluded by filters or removed due to missing values."),
                code=exceptions$dataError,
                varName=factorName
            )
        }
    }
}


#' Reject the analysis if no rows are left after removing missing values
#'
#' @param self The analysis object (for translation)
#' @param data The data the analysis is run on, after removing missing values
#' @keywords internal
rejectEmptyData = function(self, data) {
    if (nrow(data) == 0) {
        jmvcore::reject(
            .("The dataset contains 0 rows (after removing rows with missing values)"),
            code=exceptions$dataError
        )
    }
}
