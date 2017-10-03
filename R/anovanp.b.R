
anovaNPClass <- R6::R6Class(
    "anovaNPClass",
    inherit=anovaNPBase,
    private=list(
        .run=function() {
            deps  <- self$options$get('deps')
            group <- self$options$get('group')

            if (length(deps) == 0 || is.null(group))
                return()

            data <- self$data
            table <- self$results$get('table')

            groupColumn  <- as.factor(data[[group]])

            for (depName in self$options$get('deps')) {
                depColumn <- jmvcore::toNumeric(data[[depName]])
                result <- kruskal.test(x=depColumn, g=groupColumn)
                table$setRow(rowKey=depName, values=list(
                    chiSq=result$statistic,
                    df=result$parameter,
                    p=result$p.value
                ))
            }

            if (self$options$get("pairs")) {

                pairs <- private$.genPairs(groupColumn)

                for (depName in deps) {

                    depColumn <- data[[depName]]
                    table <- self$results$get('comparisons')$get(depName)

                    sdata <- base::split(depColumn, groupColumn)

                    for (pair in pairs) {
                        if (table$getCell(rowKey=pair, 'W')$isEmpty) {

                            table$setStatus('running')
                            private$.checkpoint()

                            pairData <- list(sdata[[pair[1]]], sdata[[pair[2]]])
                            result <- pSDCFlig(pairData, method="Asymptotic")

                            table$setRow(rowKey=pair, list(
                                p1=pair[1],
                                p2=pair[2],
                                W=result$obs.stat,
                                p=result$p.val
                            ))

                            table$setStatus('complete')
                        }
                    }
                }
            }
        },
        .init=function() {

            data <- self$data
            deps <- self$options$get('deps')
            group <- self$options$get('group')

            if (is.null(group))
                return()

            compTables <- self$results$get('comparisons')

            groupColumn <- data[[group]]
            pairs <- private$.genPairs(groupColumn)

            for (depName in deps) {
                depColumn <- data[[depName]]
                depTable <- compTables$get(depName)
                for (pair in pairs) {
                    depTable$addRow(rowKey=pair, values=list(
                        p1=pair[1],
                        p2=pair[2]))
                }
            }
        },
        .genPairs=function(groupColumn) {
            groupLevels <- base::levels(groupColumn)

            pairsList <- list()

            if (length(groupLevels) > 0) {
                pairsMatrix <- utils::combn(groupLevels, 2)
                for (i in seq_len(dim(pairsMatrix)[2]))
                    pairsList[[i]] <- pairsMatrix[,i]
            }

            pairsList
        })
)

# the following is borrowed from NSM3
# we should talk to them and cite them somehow, send them a pizza

#' @importFrom stats pnorm integrate
pRangeNor<-function(x,k){
    r<-function(x,n){
        inner.int<-function(s){
            exp(-s^2)*(pnorm(s+x/2)-pnorm(s-x/2))^(n-2)
        }
        return(n*(n-1)*exp(-x^2/4)/(2*pi)*integrate(inner.int,-Inf,Inf)$value)
    }

    approx.dens<-test.grid<-seq(0,10,.001)
    test.grid<-round(test.grid,3)
    for(i in 1:length(test.grid)){
        approx.dens[i]<-r(test.grid[i],k)*.001
    }
    approx.dens=approx.dens/sum(approx.dens)

    upper.tails<-rev(cumsum(rev(approx.dens)))
    max(upper.tails[test.grid==round(x,3)], 0)
}

#' @importFrom stats complete.cases
pSDCFlig<-function(x,g=NA,method=NA,n.mc=10000){
    outp<-list()
    outp$stat.name<-"Dwass, Steel, Critchlow-Fligner W"

    outp$n.mc<-n.mc

    ##From kruskal.test()##
    if (is.list(x)) {
        if (length(x) < 2L)
            stop("'x' must be a list with at least 2 elements")
        DNAME <- deparse(substitute(x))
        x <- lapply(x, function(u) u <- u[complete.cases(u)])
        k <- length(x)
        l <- sapply(x, "length")
        if (any(l == 0))
            stop("all groups must contain data")
        g <- factor(rep(1:k, l))
        x <- unlist(x)
    }
    else {
        if (length(x) != length(g))
            stop("'x' and 'g' must have the same length")
        DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
        OK <- complete.cases(x, g)
        x <- x[OK]
        g <- g[OK]
        if (!all(is.finite(g)))
            stop("all group levels must be finite")
        g <- factor(g)
        l<-as.numeric(table(g))
        k <- nlevels(g)
        if (k < 2)
            stop("all observations are in the same group")
    }
    N <- length(x)
    #####################
    outp$n<-l

    outp$num.comp<-num.comp<-k*(k-1)/2
    outp$ties<- (length(x) != length(unique(x)))

    ##When the user doesn't give us any indication of which method to use, try to pick one.
    if(is.na(method)){
        if(factorial(sum(outp$n))/prod(factorial(outp$n))<=10000){
            method<-"Exact"
        }
        if(factorial(sum(outp$n))/prod(factorial(outp$n))>10000){
            method<-"Monte Carlo"
        }
    }
    #####################################################################
    outp$method<-method

    count<-1
    outp$labels<-character(num.comp)
    for(i in 1:(k-1)){
        for(j in (i+1):k){
            outp$labels[count]<-paste(levels(g)[i],"-",levels(g)[j])
            count<-count+1
        }
    }

    W.star.calc<-function(x,i,j){
        group.sizes<-l[c(i,j)]
        W.stat<-sum(rank(c(x[g==levels(g)[i]],x[g==levels(g)[j]]))[(group.sizes[1]+1):sum(group.sizes)])
        W.mean<-group.sizes[2]*(sum(group.sizes)+1)/2
        tie.vec<-as.numeric(table(c(x[g==levels(g)[i]],x[g==levels(g)[j]])))
        W.var<-prod(group.sizes)/24*(sum(group.sizes)+1-sum((tie.vec-1)*tie.vec*(tie.vec+1)/(sum(group.sizes)*(sum(group.sizes)-1))))
        (W.stat-W.mean)/sqrt(W.var)
    }

    W.star.all<-function(x){
        W.star.vec<-numeric(num.comp)
        count<-1
        for(i in 1:(k-1)){
            for(j in (i+1):k){
                W.star.vec[count]<-W.star.calc(x,i,j)
                count<-count+1
            }
        }
        W.star.vec
    }

    outp$obs.stat<-W.star.all(x);

    # if(method=="Exact"){
    #     possible.combs<-multComb(l)
    #     if(outp$ties){
    #         possible.ranks<-as.numeric(rank(x))
    #         possible.combs<-t(apply(possible.combs,1,function(x) possible.ranks[x]))
    #     }
    #     exact.dist<-apply(possible.combs,1,function(x) max(abs(W.star.all(x))))
    #     for(i in 1:num.comp){
    #         outp$p.val[i]<-mean(exact.dist>=abs(outp$obs.stat[i]))
    #     }
    # }


    if(method=="Monte Carlo"){
        outp$p.val<-numeric(num.comp)
        for(i in 1:n.mc){
            mc.order<-as.numeric(sample(rank(x)))
            for(j in 1:num.comp){
                if(max(abs(W.star.all(mc.order)))>=abs(outp$obs.stat[j])){
                    outp$p.val[j]=outp$p.val[j]+1/n.mc
                }
            }
        }
    }
    if(method=="Asymptotic"){
        for(j in 1:num.comp){
            outp$p.val[j]<-pRangeNor(abs(outp$obs.stat[j]),k)
        }
    }

    class(outp)<-"NSM3Ch6MCp"
    outp
}
