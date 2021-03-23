checkTypes <- list(
    "variable_contains_inf" = "variable_contains_inf",
    "variable_contains_missing" = "variable_contains_missing",
    "variable_contains_only_missing" = "variable_contains_only_missing",
    "variable_contains_one_unique_value" = "variable_contains_one_unique_value"
)

variableContainsInf = function(col, colName) {
    fail <- any(is.infinite(col))
    if (fail) {
        jmvcore::reject(
            jmvcore::format("'{}' contains infinite values", colName),
            code = checkTypes$variable_contains_inf,
            expected = TRUE
        )
    }
}

variableContainsMissing = function(col, colName) {
    fail <- any(is.na(col))
    if (fail) {
        jmvcore::reject(
            jmvcore::format("'{}' contains missing values", colName),
            code = checkTypes$variable_contains_missing,
            expected = TRUE
        )
    }
}

variableContainsOnlyMissing = function(col, colName) {
    fail <- all(is.na(col))
    if (fail) {
        jmvcore::reject(
            jmvcore::format("'{}' contains only missing values", colName),
            code = checkTypes$variable_contains_only_missing,
            expected = TRUE
        )
    }
}

variableContainsOneUniqueValue = function(col, colName) {
    fail <- length(unique(col)) == 1
    if (fail) {
        jmvcore::reject(
            jmvcore::format("'{}' contains only one unique value", colName),
            code = checkTypes$variable_contains_one_unique_value,
            expected = TRUE
        )
    }
}

checkData = function(data, types, B64 = FALSE) {
    for (colNo in seq_along(data)) {
        col <- data[[colNo]]
        colName <- names(data)[[colNo]]
        if (B64)
            colname <- jmvcore::fromB64(colName)

        if (checkTypes$variable_contains_inf %in% types)
            variableContainsInf(col, colName)
        if (checkTypes$variable_contains_missing %in% types)
            variableContainsMissing(col, colName)
        if (checkTypes$variable_contains_only_missing %in% types)
            variableContainsOnlyMissing(col, colName)
        if (checkTypes$variable_contains_one_unique_value %in% types)
            variableContainsOneUniqueValue(col, colName)
    }
}
