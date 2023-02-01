#' @importFrom jmvcore .

checkTypes <- list(
    "variable_contains_inf" = "variable_contains_inf",
    "variable_contains_missing" = "variable_contains_missing",
    "variable_contains_only_missing" = "variable_contains_only_missing",
    "variable_contains_one_unique_value" = "variable_contains_one_unique_value"
)

variableContainsInf = function(self, col, colName) {
    fail <- any(is.infinite(col))
    if (fail) {
        jmvcore::reject(
            jmvcore::format(.("'{col}' contains infinite values"), col=colName),
            code = checkTypes$variable_contains_inf,
            expected = TRUE
        )
    }
}

variableContainsMissing = function(self, col, colName) {
    fail <- any(is.na(col))
    if (fail) {
        jmvcore::reject(
            jmvcore::format(.("'{col}' contains missing values"), col=colName),
            code = checkTypes$variable_contains_missing,
            expected = TRUE
        )
    }
}

variableContainsOnlyMissing = function(self, col, colName) {
    fail <- all(is.na(col))
    if (fail) {
        jmvcore::reject(
            jmvcore::format(.("'{col}' contains only missing values"), col=colName),
            code = checkTypes$variable_contains_only_missing,
            expected = TRUE
        )
    }
}

variableContainsOneUniqueValue = function(self, col, colName) {
    fail <- length(unique(col)) == 1
    if (fail) {
        jmvcore::reject(
            jmvcore::format(.("'{col}' contains only one unique value"), col=colName),
            code = checkTypes$variable_contains_one_unique_value,
            expected = TRUE
        )
    }
}

checkData = function(self, data, types, B64 = FALSE) {
    for (colNo in seq_along(data)) {
        col <- data[[colNo]]
        colName <- names(data)[[colNo]]
        if (B64)
            colname <- jmvcore::fromB64(colName)

        if (checkTypes$variable_contains_inf %in% types)
            variableContainsInf(self, col, colName)
        if (checkTypes$variable_contains_missing %in% types)
            variableContainsMissing(self, col, colName)
        if (checkTypes$variable_contains_only_missing %in% types)
            variableContainsOnlyMissing(self, col, colName)
        if (checkTypes$variable_contains_one_unique_value %in% types)
            variableContainsOneUniqueValue(self, col, colName)
    }
}

exceptions = list(
    "attributeError" = "attributeError",
    "indexError" = "indexError",
    "keyError" = "keyError",
    "modelError" = "modelError",
    "nameError" = "nameError",
    "valueError" = "valueError",
    "dataError" = "dataError"
)

cfaErrors = list(
    list(
        originalMessage = paste0(
            'invalid object for slot "fx.group" in class "Fit": got class "NULL", should be or',
            ' extend class "numeric"'
        ),
        message = "Model dit not converge",
        class = exceptions$modelError
    ),
    list(
        originalMessage = 'lavaan ERROR: fit measures not available if model did not converge',
        message = "Model dit not converge",
        class = exceptions$modelError
    )
)
