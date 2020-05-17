
reliabilityClass <- R6::R6Class(
    "reliabilityClass",
    inherit = reliabilityBase,
    private = list(
        #### Init + run functions ----
        .init = function() {

            private$.initItemsTable()

        },
        .run = function() {

            ready <- TRUE
            if (is.null(self$options$vars) || length(self$options$vars) < 2)
                ready <- FALSE

            if (ready) {

                data <- private$.cleanData()
                private$.errorCheck(data)

                results <- private$.compute(data)

                private$.populateScaleTable(results)
                private$.populateItemsTable(results)
                private$.prepareCorPlot(data)

            }
        },

        #### Compute results ----
        .compute = function(data) {

            suppressMessages({
                suppressWarnings({

                    alpha <- psych::alpha(data, delete=FALSE, warnings=FALSE)
                    omega <- psych::omega(data, 1, flip = FALSE)

                    items <- self$options$vars

                    omegaDrop <- numeric(length(items))
                    if (length(items) > 2) {
                        for (i in seq_along(items))
                            omegaDrop[i] <- psych::omega(data[, colnames(data) != items[i]], 1,
                                                         flip = FALSE)$omega.tot
                    }

                    loadings <- psych::principal(data, scores=FALSE)$loadings
                    negCorItems <- items[loadings < 0]

                }) # suppressWarnings
            }) # suppressMessages

            return(list('alpha'=alpha, 'omega'=omega, 'omegaDrop'=omegaDrop, 'negCorItems'=negCorItems))
        },

        #### Init tables ----
        .initItemsTable = function() {

            table <- self$results$items
            items <- self$options$revItems

            for (i in seq_along(items))
                table$addFootnote(rowKey=items[i], 'name', 'reverse scaled item')

        },

        #### Populate tables ----
        .populateScaleTable = function(results) {

            table <- self$results$scale

            alpha <- results$alpha
            omega <- results$omega
            negCorItems <- results$negCorItems

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
        .populateItemsTable = function(results) {

            table <- self$results$items

            items <- self$options$vars
            alpha <- results$alpha
            omegaDrop <- results$omegaDrop

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

        #### Plot functions ----
        .prepareCorPlot = function(data, reorder = FALSE) {

            cormat <- round(cor(data), 2)

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
        })
)
