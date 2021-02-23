
reliabilityClass <- R6::R6Class(
    "reliabilityClass",
    inherit = reliabilityBase,
    #### Active bindings ----
    active = list(
        dataProcessed = function() {
            if (is.null(private$.dataProcessed))
                private$.dataProcessed <- private$.cleanData()

            return(private$.dataProcessed)
        },
        alpha = function() {
            if (is.null(private$.alpha))
                private$.alpha <- private$.computeAlpha()

            return(private$.alpha)
        },
        omega = function() {
            if (is.null(private$.omega))
                private$.omega <- private$.computeOmega()

            return(private$.omega)
        },
        omegaDrop = function() {
            if (is.null(private$.omegaDrop))
                private$.omegaDrop <- private$.computeOmegaDrop()

            return(private$.omegaDrop)
        },
        loadings = function() {
            if (is.null(private$.loadings))
                private$.loadings <- private$.computeLoadings()

            return(private$.loadings)
        }
    ),
    private = list(
        #### Member variables ----
        .dataProcessed = NULL,
        .alpha = NULL,
        .omega = NULL,
        .omegaDrop = NULL,
        .loadings = NULL,
        .negCorItems = NULL,
        .varList = NULL,

        #### Init + run functions ----
        .init = function() {
            private$.initItemsTable()
            private$.initOutputs()
        },
        .run = function() {
            if (is.null(self$options$vars) || length(self$options$vars) < 2)
                return()

            private$.populateScaleTable()
            private$.populateItemsTable()
            private$.prepareCorPlot()
            private$.populateOutputs()
        },

        #### Compute results ----
        .computeAlpha = function() {
            suppressMessages({
                suppressWarnings({
                    alpha <- psych::alpha(self$dataProcessed, delete=FALSE, warnings=FALSE)
                })
            })
            return(alpha)
        },
        .computeOmega = function() {
            suppressMessages({
                suppressWarnings({
                    omega <- psych::omega(self$dataProcessed, 1, flip = FALSE)
                })
            })
            return(omega)
        },
        .computeOmegaDrop = function() {
            suppressMessages({
                suppressWarnings({
                    items <- self$options$vars

                    omegaDrop <- numeric(length(items))
                    if (length(items) > 2) {
                        colNames <- colnames(self$dataProcessed)
                        for (i in seq_along(items)) {
                            omegaDrop[i] <- psych::omega(
                                self$dataProcessed[, colNames != items[i]],
                                1, flip = FALSE)$omega.tot
                        }
                    }
                })
            })

            return(omegaDrop)
        },
        .computeLoadings = function() {
            suppressMessages({
                suppressWarnings({
                    loadings <- psych::principal(self$dataProcessed, scores=FALSE)$loadings
                })
            })
            return(loadings)
        },

        #### Init tables ----
        .initItemsTable = function() {
            table <- self$results$items
            items <- self$options$revItems

            for (i in seq_along(items))
                table$addFootnote(rowKey=items[i], 'name', 'reverse scaled item')
        },
        .initOutputs = function() {
            description = function(aggrType) {
                return(
                    jmvcore::format(
                        "{} score based on the variables {}",
                        aggrType,
                        listItems(private$.getVarList())
                    )
                )
            }

            if (self$options$meanScoreOV)
                self$results$meanScoreOV$setDescription(description("Mean"))

            if (self$options$sumScoreOV)
                self$results$sumScoreOV$setDescription(description("Sum"))
        },

        #### Populate tables ----
        .populateScaleTable = function() {

            table <- self$results$scale

            alpha <- self$alpha
            omega <- self$omega
            negCorItems <- private$.getNegCorItems()

            row <- list()
            row[['alpha']] <- alpha$total$raw_alpha
            row[['mean']] <- alpha$total$mean
            row[['sd']] <- alpha$total$sd
            row[['omega']] <- omega$omega.tot

            table$setRow(rowNo=1, values=row)

            if (length(negCorItems) > 0) {
                if (length(negCorItems) == 1) {
                    note <- jmvcore::format('item {} correlates negatively with the total scale and probably should be reversed',
                                            listItems(negCorItems))
                } else {
                    note <- jmvcore::format('items {} correlate negatively with the total scale and probably should be reversed',
                                            listItems(negCorItems))
                }
                table$setNote(key='negCor', note=note, init=FALSE)
            }
        },
        .populateItemsTable = function() {
            table <- self$results$items

            items <- self$options$vars
            alpha <- self$alpha
            omegaDrop <- self$omegaDrop

            for (i in seq_along(items)) {

                row <- list()
                row[["alpha"]] <- alpha$alpha.drop[items[i],"raw_alpha"]
                row[["mean"]] <- alpha$item.stats[items[i],"mean"]
                row[["sd"]] <- alpha$item.stats[items[i],"sd"]
                row[["itemRestCor"]] <- alpha$item.stats[items[i],"r.drop"]
                row[["omega"]] <- omegaDrop[i]

                table$setRow(rowKey=items[i], values=row)
            }
        },
        .populateOutputs = function() {
            if (self$options$meanScoreOV && self$results$meanScoreOV$isNotFilled()) {
                means <- apply(self$dataProcessed, 1, mean)

                df <- data.frame(
                    meanScoreOV = means,
                    row.names = rownames(self$dataProcessed)
                )
                self$results$meanScoreOV$setValues(df)
            }

            if (self$options$sumScoreOV && self$results$sumScoreOV$isNotFilled()) {
                sums <- apply(self$dataProcessed, 1, sum)

                df <- data.frame(
                    sumScoreOV = sums,
                    row.names = rownames(self$dataProcessed)
                )
                self$results$sumScoreOV$setValues(df)
            }
        },

        #### Plot functions ----
        .prepareCorPlot = function(reorder = FALSE) {
            cormat <- round(cor(self$dataProcessed), 2)

            if (reorder) {
                dd <- stats::as.dist((1-cormat)/2)
                hc <- stats::hclust(dd)
                cormat <- cormat[hc$order, hc$order]
            }

            cormat[lower.tri(cormat)] <- NA
            melted_cormat <- reshape2::melt(cormat, na.rm = TRUE)

            image <- self$results$get('corPlot')
            image$setState(melted_cormat)
        },
        .corPlot = function(image, ...) {
            if (is.null(image$state))
                return(FALSE)

            the <- theme_minimal() + theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.grid.major = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.ticks = element_blank(),
                legend.justification = c(1, 0),
                legend.position = c(0.45, 0.7),
                legend.direction = "horizontal",
                axis.text.x = element_text(angle = 45, vjust = 1,
                                           size = 12, hjust = 1),
                axis.text.y = element_text(size = 12)
            )

            p <- ggplot(data = image$state, aes(Var2, Var1, fill = value)) +
                        geom_tile(color = "white") +
                        scale_fill_gradient2(low = "#FF1919", high = "#00B233", mid = "white",
                                             midpoint = 0, limit = c(-1,1), space = "Lab",
                                             name="Pearson\nCorrelation") +
                        coord_fixed() +
                        geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
                        guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                                     title.position = "top", title.hjust = 0.5)) +
                        the

            return(p)
        },

        #### Helper functions ----
        .cleanData = function() {
            items <- self$options$vars

            data <- list()
            for (item in items)
                data[[item]] <- jmvcore::toNumeric(self$data[[item]])

            attr(data, 'row.names') <- seq_len(length(data[[1]]))
            attr(data, 'class') <- 'data.frame'
            data <- jmvcore::naOmit(data)

            for (item in self$options$revItems) {

                dataItem <- data[[item]]
                minItem <- min(dataItem)
                maxItem <- max(dataItem)
                adjust <- minItem + maxItem

                data[[item]] <- adjust - dataItem
            }

            private$.errorCheck(data)

            return(data)
        },
        .errorCheck = function(data) {
            items <- self$options$vars

            infItems <- sapply(data, function(x) any(is.infinite(x)))
            allNAItems <- sapply(data, function(x) all(is.na(x)))
            noVarItems <- sapply(data, function(x) var(x, na.rm = TRUE) == 0)

            if (any(infItems))
                jmvcore::reject("Item '{}' contains infinite values", code='', items[infItems])
            if (any(allNAItems))
                jmvcore::reject("Item '{}' contains only missing values", code='', items[allNAItems])
            if (any(noVarItems))
                jmvcore::reject("Item '{}' has no variance", code='', items[noVarItems])
        },
        .getNegCorItems = function() {
            if (is.null(private$.negCorItems))
                private$.negCorItems <- self$options$vars[self$loadings < 0]

            return(private$.negCorItems)
        },
        .getVarList = function() {
            if (is.null(private$.varList)) {
                items <- self$options$vars
                for (revItem in self$options$revItems)
                    items[which(revItem == self$options$vars)] <- paste0(revItem, " (reversed)")

                private$.varList <- items
            }

            return(private$.varList)
        })
)
