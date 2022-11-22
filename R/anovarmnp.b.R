
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
            result <- PMCMRplus::durbinAllPairsTest(mat, p.adjust='none')

            n <- length(measureNames)-1
            rowNo <- 1
            for (j in 1:n) {
                for (k in j:n) {
                    table$setRow(rowNo=rowNo, list(
                        stat=result$statistic[k,j],
                        p=result$p.value[k,j]
                    ))
                    rowNo <- rowNo + 1
                }
            }
        }

        private$.preparePlot(data)

        desc <- self$results$get('desc')
        descData <- private$desc

        for (i in seq_along(measureNames))
            desc$setRow(rowKey=i, list(mean=descData$mean[i], median=descData$median[i]))

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

        for (i in seq_along(measureNames))
            desc$addRow(rowKey=i, list(level=measureNames[i]))
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
