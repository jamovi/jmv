
# aggregate from R 3.3 for use in R 3.2

#' @importFrom stats aggregate.data.frame
aggregate <- function (x, by, FUN, ..., simplify = TRUE, drop = TRUE)
{
    if (!is.data.frame(x))
        x <- as.data.frame(x)
    FUN <- match.fun(FUN)
    if (NROW(x) == 0L)
        stop("no rows to aggregate")
    if (NCOL(x) == 0L) {
        x <- data.frame(x = rep(1, NROW(x)))
        return(aggregate.data.frame(x, by, function(x) 0L)[seq_along(by)])
    }
    if (!is.list(by))
        stop("'by' must be a list")
    if (is.null(names(by)) && length(by))
        names(by) <- paste("Group", seq_along(by), sep = ".")
    else {
        nam <- names(by)
        ind <- which(!nzchar(nam))
        names(by)[ind] <- paste("Group", ind, sep = ".")
    }
    nrx <- NROW(x)
    if (any(lengths(by) != nrx))
        stop("arguments must have same length")
    y <- as.data.frame(by, stringsAsFactors = FALSE)
    keep <- complete.cases(by)
    y <- y[keep, , drop = FALSE]
    x <- x[keep, , drop = FALSE]
    nrx <- NROW(x)
    ident <- function(x) {
        y <- as.integer(as.factor(x))
        z <- gsub(" ", "0", format(y, scientific = FALSE))
        return(z)
    }
    grp <- if (ncol(y)) {
        grp <- lapply(rev(y), ident)
        names(grp) <- NULL
        do.call(paste, c(grp, list(sep = ".")))
    }
    else integer(nrx)
    if (!drop && ncol(y)) {
        y <- expand.grid(lapply(y, function(e) sort(unique(e))))
        lev <- lapply(rev(y), ident)
        names(lev) <- NULL
        lev <- do.call(paste, c(lev, list(sep = ".")))
        grp <- factor(grp, levels = lev)
    }
    else y <- y[match(sort(unique(grp)), grp, 0L), , drop = FALSE]
    nry <- NROW(y)
    z <- lapply(x, function(e) {
        ans <- lapply(X = split(e, grp), FUN = FUN, ...)
        if (simplify && length(len <- unique(lengths(ans))) ==
            1L) {
            if (len == 1L) {
                cl <- lapply(ans, oldClass)
                cl1 <- cl[[1L]]
                ans <- unlist(ans, recursive = FALSE)
                if (!is.null(cl1) && all(sapply(cl, function(x) identical(x,
                                                                          cl1))))
                    class(ans) <- cl1
            }
            else if (len > 1L)
                ans <- matrix(unlist(ans, recursive = FALSE),
                              nrow = nry, ncol = len, byrow = TRUE, dimnames = {
                                  if (!is.null(nms <- names(ans[[1L]])))
                                      list(NULL, nms)
                                  else NULL
                              })
        }
        ans
    })
    len <- length(y)
    for (i in seq_along(z)) y[[len + i]] <- z[[i]]
    names(y) <- c(names(by), names(x))
    row.names(y) <- NULL
    y
}
