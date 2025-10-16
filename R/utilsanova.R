#' Set a warning notice for singularity in the design matrix
#'
#' @param self The analysis object
#' @keywords internal
setSingularityWarning = function(self) {
    message <- paste(
        .("Model fit issue detected!"),
        .("It appears that your model has a \"singularity\" problem. This usually means that the model is having trouble distinguishing between certain effects because some variables are too closely related or there isn't enough data to estimate all the effects reliably. To resolve this, consider simplifying the model by removing some of the effects or checking if your data might have redundant or highly correlated variables.")
    )

    setAnalysisNotice(
        self, message, name="refLevelWarning", type=jmvcore::NoticeType$STRONG_WARNING
    )
}

#' Create contrast labels (for AN(C)OVA and rmANOVA)
#'
#' @param levels character vector with the (names of the) factor levels
#' @param type   the type of contrast
#' @param self   the analysis object (for translation)
#' @keywords internal
contrastLabels = function(levels, type, self) {
    nLevels <- length(levels)
    labels <- list()

    if (length(levels) <= 1) {

        # do nothing

    } else if (type %in% c('simple_1', 'simple')) {

        for (i in seq_len(nLevels-1))
            labels[[i]] <- paste(levels[i+1], '-', levels[1])

    } else if (type == 'simple_k') {

        for (i in seq_len(nLevels-1))
            labels[[i]] <- paste(levels[i], '-', levels[nLevels])

    } else if (type == 'deviation') {

        all <- paste(levels, collapse=', ')
        for (i in seq_len(nLevels-1))
            labels[[i]] <- paste(levels[i+1], '-', all)

    } else if (type == 'difference') {

        for (i in seq_len(nLevels-1)) {
            rhs <- paste0(levels[1:i], collapse=', ')
            labels[[i]] <- paste(levels[i + 1], '-', rhs)
        }

    } else if (type == 'helmert') {

        for (i in seq_len(nLevels-1)) {
            rhs <- paste(levels[(i+1):nLevels], collapse=', ')
            labels[[i]] <- paste(levels[i], '-', rhs)
        }

    } else if (type == 'repeated') {

        for (i in seq_len(nLevels-1))
            labels[[i]] <- paste(levels[i], '-', levels[i+1])

    } else if (type == 'polynomial') {

        # adapted / shortened to match poly.emmc
        names <- c(.("linear"), .("quadratic"), .("cubic"), .("quartic"))
        
        for (i in seq_len(nLevels-1)) {
            if (i <= length(names)) {
                labels[[i]] <- names[i]
            } else {
                labels[[i]] <- paste(.("degree"), i)
            }
        }
    }

    labels
}
