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
