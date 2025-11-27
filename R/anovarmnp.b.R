
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

            # code adjusted from PMCMR::posthoc.durbin.test
            # [1] calculate the rank of each condition (within each participant / unit of observation),
            # [2] calculate the t-value for the rank sum per condition and [3] determine the p-value from that t-value 
#           GRPNAME <- colnames(mat)
            datf <- data.frame(y = as.vector(mat), b = factor(c(row(mat))), g = factor(c(col(mat))))[!is.na(as.vector(mat)), ]
            datf <- datf[order(datf[, 2], datf[, 3]), ]

            t <- nlevels(datf[, 3])
            b <- nlevels(datf[, 2])
            r <- unique(table(datf[, 3]))
            k <- unique(table(datf[, 2]))
            rij <- unlist(tapply(datf[, 1], datf[, 2], rank), use.names = FALSE)
            Rj <- tapply(rij, datf[, 3], sum)
            # taken from NIST
            A <- sum(rij ^ 2) 
            C <- (b * k * (k + 1) ^ 2) / 4
            T1 <- (t - 1) / (A - C) * (sum(Rj ^ 2) - r * C)
            denom <- sqrt(((A - C) * 2 * r) / (b * k - b - t + 1) * (1 - T1 / (b * (k -1))))

            STAT <- pairwise.table(function(i, j) abs(Rj[i] - Rj[j]) / denom, levels(datf[, 3]), p.adjust.method = "none")
            PVAL  <- matrix(2 * pt(q = as.vector(STAT), df = (b - 1) * (k - 1), lower.tail = FALSE), nrow = nrow(STAT))
#           colnames(STAT) <- colnames(PVAL) <- GRPNAME[1:(t - 1)]
#           rownames(STAT) <- rownames(PVAL) <- GRPNAME[2:t]

            n <- length(measureNames) - 1
            rowNo <- 1
            for (j in 1:n) {
                for (k in j:n) {
                    table$setRow(rowNo = rowNo, list(stat = STAT[k, j], p = PVAL[k, j]))
                    rowNo <- rowNo + 1
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
