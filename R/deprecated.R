
#' @rdname ANOVA
#' @param ... arguments passed to anova() are passed on to ANOVA()
#' @export
anova <- function(...) {
    .Deprecated('ANOVA', 'anova() from jmv is deprecated, use ANOVA() instead')
    ANOVA(...)
}
