
#' @rdname jamovi
#' @importFrom psych alpha omega
#' @importFrom ggplot2 ggplot
#' @import GPArotation
#' @export
reliabilityClass <- R6::R6Class(
    "reliabilityClass",
    inherit = reliabilityBase,
    private = list(
        .run = function() {
            
            suppressWarnings({
                
                if (is.null(self$options$vars) || length(self$options$vars) < 2)
                    return()
                
                items <- unlist(self$options$vars)
                
                data <- list()
                for (item in items)
                    data[[item]] <- jmvcore::toNumeric(self$data[[item]])
                
                data <- as.data.frame(data)
                
                private$.errorCheck(data)
                
                data <- jmvcore::naOmit(data)
                
                # Fill scale statistics table
                
                scaleTable <- self$results$scale
                
                resultAlpha <- psych::alpha(data, delete=FALSE, warnings=FALSE)
                
                values <- list("alpha" = resultAlpha$total$raw_alpha,
                               "mean" = resultAlpha$total$mean,
                               "sd" = resultAlpha$total$sd)
                
                if (self$options$omegaScale) {
                    resultOmega <- psych::omega(data, 1, flip = FALSE)
                    values[["omega"]] <- resultOmega$omega.tot
                }
                
                scaleTable$setRow(rowNo=1, values=values)
                
                private$.checkpoint()
                
                # Fill item statistics table
                
                itemsTable <- self$results$items
                
                for (item in items) {
                    
                    row <- list()
                    row[["alpha"]] <- resultAlpha$alpha.drop[item,"raw_alpha"]
                    row[["mean"]] <- resultAlpha$item.stats[item,"mean"]
                    row[["sd"]] <- resultAlpha$item.stats[item,"sd"]
                    row[["itemRestCor"]] <- resultAlpha$item.stats[item,"r.drop"]
                    
                    itemsTable$setRow(rowKey=item, values=row)
                }
                
                private$.checkpoint()
                
                if (self$options$omegaItems && length(self$options$vars) > 2)
                    private$.omegaDrop(data, items, itemsTable)
                
                private$.checkpoint()
                
                private$.prepareCorPlot(data)
                
            }) # suppressWarnings
        },
        .errorCheck=function(data) {
            
            items <- self$options$vars
            
            infItems <- sapply(data, function(x) any(is.infinite(x)))
            allNAItems <- sapply(data, function(x) all(is.na(x)))
            noVarItems <- sapply(data, function(x) var(x, na.rm = TRUE) == 0)
            
            error <- list()
            if (any(infItems))
                jmvcore::reject("Item '{}' contains infinite values", code='', items[infItems])
            if (any(allNAItems))
                jmvcore::reject("Item '{}' contains only missing values", code='', items[allNAItems])
            if (any(noVarItems))
                jmvcore::reject("Item '{}' has no variance", code='', items[noVarItems])
        },
        .omegaDrop=function(data, items, itemsTable) {
            
            suppressWarnings({
                for (item in items) {
                    omega <- psych::omega(data[,colnames(data) != item], 1, flip = FALSE)$omega.tot
                    itemsTable$setCell(rowKey=item, col="omega", value=omega)
                }
            }) # suppressWarnings
        },
        .prepareCorPlot=function(data, reorder = FALSE) {
            
            cormat <- round(cor(data), 2)
            
            if (reorder) {
                dd <- stats::as.dist((1-cormat)/2)
                hc <- stats::hclust(dd)
                cormat <- cormat[hc$order, hc$order]
            }
            
            upper_tri <- cormat[lower.tri(cormat)] <- NA
            melted_cormat <- reshape2::melt(cormat, na.rm = TRUE)
            
            image <- self$results$get('corPlot')
            image$setState(melted_cormat)
        },
        .corPlot=function(image, ...) {
            
            if (is.null(image$state))
                return(FALSE)
            
            the <- theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.grid.major = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.ticks = element_blank(),
                legend.justification = c(1, 0),
                legend.position = c(0.45, 0.7),
                legend.direction = "horizontal")
            
            print(ggplot2::ggplot(data = image$state, aes(Var2, Var1, fill = value)) +
                      geom_tile(color = "white") +
                      scale_fill_gradient2(low = "#FF1919", high = "#00B233", mid = "white", 
                                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                                           name="Pearson\nCorrelation") +
                      theme_minimal() +
                      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                                       size = 12, hjust = 1),
                            axis.text.y = element_text(size = 12)) +
                      coord_fixed() +
                      geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
                      the +
                      guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                                   title.position = "top", title.hjust = 0.5)))
            
            TRUE
        },
        .init=function() {
            
            items <- self$results$items
            
            items$getColumn('alpha')$setSuperTitle('if item dropped')
            items$getColumn('omega')$setSuperTitle('if item dropped')
        })
)
