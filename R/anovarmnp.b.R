
anovaRMNPClass <- R6::R6Class(
  "anovaRMNPClass",
  inherit=anovaRMNPBase,
  private=list(
    desc = list(),
    .run=function() {

        measureNames <- self$options$get('measures')

        if (length(measureNames) < 2)
            return()

        data <- select(self$data, measureNames)
        data <- jmvcore::naOmit(data)
        mat <- matrix(nrow=nrow(data), ncol=length(measureNames), dimnames=list(NULL, measureNames))

        for (i in seq_along(measureNames)) {
            name <- measureNames[[i]]
            mat[,i] <- jmvcore::toNumeric(data[[name]])
        }

        result <- friedman.test(mat)

        table <- self$results$get('table')
        table$setRow(rowNo=1, list(
            stat=unname(result$statistic),
            df=unname(result$parameter),
            p=unname(result$p.value)))

        if (self$options$get('pairs')) {

            table  <- self$results$get('comp')

            resDurbin <- calcDurbin(mat)
            
            for (i in seq_len(nrow(resDurbin))) {
                if (table$getCell("i1", rowNo = i)$value == resDurbin[i, "i1"] &&
                    table$getCell("i2", rowNo = i)$value == resDurbin[i, "i2"]) {
                    table$setRow(rowNo = i, list(stat = resDurbin[i, "t"], p = resDurbin[i, "p"]))
                }
            }

        }

        private$.preparePlot(data)

        desc <- self$results$get('desc')
        descData <- private$desc

        for (m in measureNames) {
            index <- which(descData$group == m)
            desc$setRow(rowKey=m, list(mean=descData[index, 'mean'], median=descData[index, 'median']))
        }

    },
    .init=function() {

        measureNames <- self$options$get('measures')

        if (length(measureNames) < 2)
            return()

        compTable <- self$results$get('comp')

        combns <- combn(unlist(measureNames), 2)

        for (i in seq_len(ncol(combns))) {
            compTable$addRow(rowKey=combns[,i], values=list(
                i1=combns[1,i],
                i2=combns[2,i]))
        }

        desc <- self$results$get('desc')

        for (m in measureNames)
            desc$addRow(rowKey=m, list(level=m))
    },
    .preparePlot=function(data) {

        for (i in seq_along(data))
            data[[i]] <- jmvcore::toNumeric(data[[i]])

        dataNew <- reshape2:::melt.data.frame(data, measure.vars=self$options$get('measures'), value.name='.DEPENDENT')

        by <- list(group = dataNew[["variable"]])
        dep <- dataNew[[".DEPENDENT"]]

        means <- aggregate(dep, by=by, mean, simplify=FALSE)
        medians <- aggregate(dep, by=by, median, simplify=FALSE)

        plotData <- data.frame(group=means$group, mean=unlist(means$x), median=unlist(medians$x))

        private$desc <- plotData

        image <- self$results$get('plot')
        image$setState(plotData)

    },
    .plot=function(image, ggtheme, theme, ...) {

        if (is.null(image$state))
            return(FALSE)

        p <- ggplot(data=image$state) + labs(x=.("Measure"), y=.("Value")) + ggtheme

        if (self$options$plotType == "means")
            p <- p + geom_point(aes(x=group, y=mean), shape=21, color=theme$color[1], fill=theme$fill[1], size=3)
        else
            p <- p + geom_point(aes(x=group, y=median), shape=21, color=theme$color[1], fill=theme$fill[1], size=3)

        return(p)
    })
)

calcDurbin=function(mat, adjustP = "none") {
    adjustP <- match.arg(adjustP, p.adjust.methods)

    # adapted from https://www.nist.gov/ -> search "Friedman Test" and
    # https://en.wikipedia.org/wiki/Durbin_test
    # NB: assumes that rows with NAs are omitted
    # b denotes blocks (participant / unit of observation)
    b <- nrow(mat)
    # k denotes treatments (conditions / measures)
    k <- ncol(mat)

    # assign ranks per block and calculate the rank sum per treatment 
    Rij <- apply(mat, 1, rank)
    Rj  <- apply(Rij, 1, sum)

    # use the formulas on the NIST web page to calculate the t-value
    # for the rank sum, which is then used to determine the p-value
    A1 <- sum(Rij ^ 2)
    C1 <- (b * k * (k + 1) ^ 2) / 4
    T1 <- (k - 1) / (A1 - C1) * (sum(Rj ^ 2) - b * C1)
    DF <- (b - 1) * (k - 1)
    DN <- sqrt(((2 *  (A1 - C1) * b) / DF) * (1 - T1 / (b * (k - 1))))

    combPairs <- t(combn(colnames(mat), 2))
    valT <- apply(combPairs, 1, function(c) abs(diff(Rj[c])) / DN)

    data.frame(i1 = combPairs[, 1], i2 = combPairs[, 2],
               t = valT, p = stats::p.adjust(2 * (1 - pt(valT, DF)), adjustP))
}
