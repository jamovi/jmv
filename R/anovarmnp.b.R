
anovaRMNPClass <- R6::R6Class(
  "anovaRMNPClass",
  inherit=anovaRMNPBase,
  private=list(
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
            result <- PMCMR::posthoc.durbin.test(mat, p.adjust='none')

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
    },
    .preparePlot=function(data) {

        for (i in seq_along(data))
            data[[i]] <- jmvcore::toNumeric(data[[i]])

        dataNew <- reshape2::melt(data, measure.vars=self$options$get('measures'), value.name='.DEPENDENT')

        by <- list(group = dataNew[["variable"]])
        dep <- dataNew[[".DEPENDENT"]]

        means <- aggregate(dep, by=by, mean, simplify=FALSE)
        medians <- aggregate(dep, by=by, median, simplify=FALSE)

        plotData <- data.frame(group=means$group, mean=unlist(means$x), median=unlist(medians$x))

        image <- self$results$get('plot')
        image$setState(plotData)

    },
    .plot=function(image,...) {

        if (is.null(image$state))
            return(FALSE)

        the <- theme(
            text=element_text(size=16, colour='#333333'),
            plot.background=element_rect(fill='transparent', color=NA),
            legend.background=element_rect(fill='transparent', colour=NA),
            panel.background=element_rect(fill='#E8E8E8'),
            plot.margin=margin(15, 15, 15, 15),
            axis.text.x=element_text(margin=margin(5,0,0,0)),
            axis.text.y=element_text(margin=margin(0,5,0,0)),
            axis.title.x=element_text(margin=margin(10,0,0,0)),
            axis.title.y=element_text(margin=margin(0,10,0,0)))

        p <- ggplot(data=image$state) + labs(x="Measure", y="Value") + the

        if (self$options$plotType == "means")
            p <- p + geom_point(aes(x=group, y=mean), shape=21, fill='white', size=3)
        else
            p <- p + geom_point(aes(x=group, y=median), shape=21, fill='white', size=3)

        print(p)

        TRUE
    })
)
