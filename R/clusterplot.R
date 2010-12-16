clusterplot_arules <- function(rules, measure, shading, control=NULL, ...){

    ## measure controls circle size
    ## shading controls color

    control <- .get_parameters(list(
		    main =paste("Cluster plot for", length(rules), "rules"),
		    k = 20,
		    aggr.fun=median, 
		    order.fun=rowMeans, 
		    ## fix lift so serveral plots are comparable (NA: take max)
		    max.shading=NA,
		    interactive = FALSE,
		    newpage=TRUE
		    ), control)


    x <- clusterplot_int(rules, measure, shading,
	    k=control$k, 
	    aggr.fun=control$aggr.fun, order.fun=control$order.fun, 
	    max.shading=control$max.shading 
	    )

    if(!control$interactive) return(invisible(x))

    ## interactive mode
    cat("Interactive mode.\n")
    seekViewport("clusterplot")

    ## draw buttons
    gI <- gInteraction(data.frame(
		    row.names = c("inspect","zoom in", "zoom out", "end"),
		    active = rep(FALSE, 4),
		    x = c(0.3, 0.5, 0.7, 0.9),
		    y = I(rep(unit(-3, "lines"), 4)),
		    w = I(rep(unit(3.5, "lines"), 4)),
		    h = I(rep(unit(1, "lines"), 4))
		    )
	    )

    drawButtons(gI)
    
    ## event loop
    while(TRUE) {
	gI <- gGetEvent(gI, box=FALSE, checkPlotBoundaries=FALSE)
	
	b <- lastButton(gI)
	if(is.null(b)) next

	## actions
	if(b=="end") {
	    cat("Leaving interactive mode.\n")
	    return(rules)
	}

	if(b=="zoom out") {
	    cat("Going up.\n")
	    return("zoom out")
	}
	

	select <- convertLoc(selection(gI)$loc,
		"native", valueOnly=TRUE)$x
	if(is.null(select)) {
	    cat("Select a lhs first!\n")
	    gI <- resetButtons(gI)
	    next
	}
	
	select <- round(select) 

	if(select>0 && select <= control$k) {
	    select <- get_order(x$order[2])[select]
	    rulesSelected <- rules[x$cl==select]
	}else{
	    cat("Illegal selection! Choose a lhs.\n")
	    next
	}

	if(b=="zoom in") {
	    if(length(unique(lhs(rulesSelected)))<2) {
		cat("Can't zoom in any further. Use inspect!\n")
		gI <- changeButton(gI, "zoom in", FALSE)
		next
	    }
		
	    cat("Zooming in. This might take a while\n")

	    ret <- clusterplot_arules(rulesSelected, measure, 
		    shading, control, ...)

	    if(!identical(ret, "zoom out")) return(ret)

	    ## we come back up so replot
	    plot(x)
	    seekViewport("clusterplot")
	    gI <- resetButtons(gI)
	}

	if(b=="inspect") {
		cat("Selected rules:\n")
		## FIXME: click on bubble
		#selectRHS <- round(as.numeric(convertY(location$Y, "native")))
		inspect(SORT(rulesSelected, by="lift"))
		gI <- changeButton(gI, "inspect", FALSE)
	    }

	## nothing else to do
	next
    }
}

## helper
rowMaxs <- function(x, na.rm=FALSE) apply(x, MARGIN=1, max, na.rm=na.rm)

.aggr <- function(m, cl, aggr.fun = median) {
    ma <- matrix(nrow=nrow(m), ncol=0)
    for(i in 1:max(cl)) {
	ma <- cbind(ma, apply(m[, cl==i, drop=FALSE], 
			MARGIN=1, aggr.fun , na.rm=TRUE))
    }
    ma[!is.finite(ma)] <- NA
    ma
}

## create an clusterplot
clusterplot_int <- function(rules, measure, shading,
	k=15, aggr.fun=median, order.fun=rowMeans, max.shading=NA) {

    ## check k
    if(length(unique(lhs(rules)))< k) k <- length(unique(lhs(rules)))

    ## cluster for shading
    s <- rulesAsMatrix(rules, shading)
    if(is.na(max.shading)) max.shading <- max(s, na.rm=TRUE)
    
    ## fixme: this handling of na for clustering is not great!
    s_clust <- s
    if(shading=="lift") naVal <- 1
    else naVal <- 0
    s_clust[is.na(s_clust)] <- naVal

    s_clust <- t(s_clust)
    if(nrow(s_clust)>k) km <-  kmeans(s_clust, k, iter.max=50, nstart=10)$cl
    else km <- 1:nrow(s_clust)

    sAggr <- .aggr(s, km, aggr.fun)

    ## reorder for shading
    order.fun <- rowMeans
    order <- ser_permutation(
	    order(order.fun(sAggr, na.rm=TRUE), decreasing=TRUE),
	    order(order.fun(t(sAggr), na.rm=TRUE), decreasing=TRUE)
	    )

    cl <- vector()
    enc <- attr(s, "encoding")
    for(i in 1:ncol(enc)) cl[enc[,i]] <- km[i]
    
    ## use measure for size
    mAggr <- .aggr(rulesAsMatrix(rules, measure[1]), km, aggr.fun)

    ret <- list(rules=rules, measure=measure, shading=shading, 
	    cl=cl, km= km, max.shading=max.shading, 
	    order.fun=order.fun, aggr.fun=aggr.fun, 
	    order=order, k=k, sAggr=sAggr, mAggr=mAggr)
    class(ret) <- "clusterplot"

    ## call plotting work horse
    plot(ret)

    ret
}

## display clusterplot
plot.clusterplot <- function(x) {
    ## circle size
    sn <- x$mAggr
    ## shading
    ln <- x$sAggr
    
    #sn <- .aggr(rulesAsMatrix(x$rules, "support"), x$km, x$aggr.fun)
    #ln <- .aggr(rulesAsMatrix(x$rules, "lift"), x$km, x$aggr.fun)

    ## get most important item in the lhs
    f <- lapply(split(x$rules, x$cl), FUN = function(r) itemFrequency(lhs(r)))
    most_imp_item <- lapply(f, FUN = 
	    function(f) paste(names(which.max(f)), " +",sum(f>0)-1, 
		    sep=""))

    clusterplot_plot_int(
	    x = map(sn, c(0.2,1)), 
	    y = map(ln, range = c(1,.2), 
		    from.range = c(min(x$sAggr, na.rm=TRUE), x$max.shading)),
	    order = x$order,
	    options = list(
		    panel = panel.circles, 
		    spacing = -1, 
		    reverse=TRUE,
		    ylab=paste(#1:max(x$cl), "-",
			    table(x$cl),
			    paste('(',most_imp_item, ')', sep='')),
		    main = paste("Grouped matrix for", length(x$rules), "rules"),
		    legend = paste("size:",x$measure, "\ncolor:",x$shading)
		    )
	    )
}

## inspect rules inside an clusterplot
inspect.clusterplot <- function(x, cluster, measure="lift") {
    inspect(SORT(x$rules[x$cl==cluster], by=measure))
}


## workhorse for plotting
## based on bertinplot in package seriation
clusterplot_plot_int <- function (x, y, order = NULL, options = NULL) {
    if (!is.matrix(x)) 
    	stop("Argument 'x' must be a matrix.")

    options <- .get_parameters(list(
		    panel.function = panel.bars, 
		    reverse = FALSE, 
		    xlab = NULL, 
		    ylab = NULL, 
		    frame = FALSE, 
		    spacing = 0.2, 
		    gp_labels = gpar(cex=.8), 
		    gp_panels = gpar(), 
		    newpage = TRUE,
		    main = "Grouped matrix",
		    legend = ""
		    ), options)
	
    if (!is.null(options$xlab)) rownames(x) <- options$xlab
    if (!is.null(options$ylab)) colnames(x) <- options$ylab

    if (!is.null(order)) {
	x <- permute(x, order)
	y <- permute(y, order)
    }

    if (options$reverse) {
	x <- t(x)
	y <- t(y)
	tmp <- options$xlab
	options$xlab <- options$ylab
	options$ylab <- tmp
	order <- rev(order)
    }

    if (options$newpage) grid.newpage()

    
    ## main
    gTitle(options$main, name="main")
    
    ## legend
    downViewport("main")
    grid.text(options$legend, 
	    x=unit(1, "npc")-unit(1,"lines"),
	    y=unit(-2, "lines"),
	    just=c("right", "top"), gp=options$gp_labels)
    upViewport(1)

    ## determine margins
    topSpace <- max(stringWidth(rownames(x)))
    rightSpace <- max(stringWidth(colnames(x)))
    
    pushViewport(viewport(x=unit(2,"lines"), y=unit(4,"lines"),
		    just <-c("left","bottom"),
		    width = unit(1, "npc")-rightSpace-unit(3,"lines"), 
		    height = unit(1, "npc")-topSpace-unit(4+3,"lines"),
		    #xscale = c(1, nrow(x)), yscale = c(1, ncol(x)), 
		    xscale = c(.5, nrow(x)+.5), yscale = c(.5, ncol(x)+.5), 
		    default.units = "native", gp=options$gp_labels,
		    name="clusterplot"))

    ## grid
    yLabPos <- unit(ncol(x), "native")
    xLabPos <- unit(nrow(x), "native") 
    
    gp_lines <- gpar(col="gray", lty=3)
    for(i in 1:nrow(x))  grid.lines(x = c(i,i),  
	    y=c(1, yLabPos),
	    default.units = "native", gp=gp_lines)
    for(i in 1:ncol(x))  grid.lines(y = c(i,i),  
	    x=c(1, xLabPos),
	    default.units = "native", gp=gp_lines)

    ## symbols
    for (variable in 1:ncol(x)) {
	size <- x[, variable]
	shading <- y[, variable]

	options$panel.function(ncol(x)-variable+1L, size, 
		shading, options$spacing)
    }

    ## labels
    yLabPos <- yLabPos + unit(1, "lines")
    xLabPos <- xLabPos + unit(1, "lines")
    grid.text(rownames(x), x = 1:nrow(x), y = yLabPos,
	    rot = 90, just = "left", 
	    default.units = "native")
    # gpar is already set in viewport
    # gp = options$gp_labels)


    grid.text(rev(colnames(x)), x = xLabPos, y = (1:ncol(x)), 
	    just = "left", 
	    default.units = "native")
    # gpar is already set in viewport
    # gp = options$gp_labels)

    ## add lhs, rhs
    gp <- gpar(fontface = "bold", cex = 1.2)
    grid.text("lhs", 
	    x = unit(1, "native")-unit(1,"lines"), y = yLabPos, 
	    rot = 90, just = "left", 
	    default.units = "native", 
	    gp = gp)
    grid.text("rhs", x = xLabPos,  
	    y = unit(ncol(x), "native")+unit(1,"lines"), 
	    just = "left", 
	    default.units = "native", gp = gp)



    upViewport(1)
}


panel.circles <- function (row, size, shading, spacing) 
{
    size[size == 0] <- NA
    #NAs are white
    shading[is.na(shading)] <- 1

    grid.circle(x = c(1:length(size)), y=row, r = size/2 * (1 - spacing), 
	    default.units = "native", 
	    gp = gpar(fill = gray(shading), alpha=.9))
}

panel.squares <- function (row, size, shading, spacing) 
{
    size[size == 0] <- NA
    shading[is.na(shading)] <- 1
    grid.rect(x = c(1:length(size)), y=row, width = size * (1 - spacing), 
	    height = size * (1 - spacing), 
	    default.units = "native", 
	    gp = gpar(fill = gray(shading), alpha=.9))
}





