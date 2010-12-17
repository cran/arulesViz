### default with alpha
.nodeColors <- function(alpha=NULL) {
    if(is.null(alpha)) alpha <- 1
    c(rgb(.4,.8,.4, alpha), rgb(.6,.6,.8, alpha))
}

gray_hcl <- function(level, alpha=1) {
    hcl(c=0, l=level*100, alpha=alpha)
}

black_hcl <- function(alpha=1) {
        hcl(c=0, l=0, alpha=alpha)
}

graph_arules <- function(rules, measure = "support", shading = "lift", 
	control=NULL, ...) {

    control <- .get_parameters(list(
		    main =paste("Graph for", length(rules), "rules"),
		    nodeColors = .nodeColors(control$alpha),
		    #nodeColors = c("lightgreen", "skyblue"),
		    alpha = 1,
		    nodeLabels = TRUE,
		    edgeLabels = FALSE,
		    precision = 4,
		    type = "itemsets",
		    layout = NULL,
		    interactive = FALSE
		    ), control)

    if(control$type=="bipartite") {
	## make bipartite graph
	leftHandSide <- as.factor(labels(lhs(rules))$elements)
	rightHandSide <- as.factor(labels(rhs(rules))$elements)
	names(leftHandSide) <- paste("l",as.integer(leftHandSide),sep='')
	names(rightHandSide) <- paste("r",as.integer(rightHandSide),sep='')
	
	fromto <- cbind(names(leftHandSide), names(rightHandSide))
	graph <- ftM2graphNEL(fromto, edgemode = "directed")

	eAttrs <- list()
	if(control$edgeLabels) {
	    edgeLabels <- paste(paste(round(m, control$precision), "/", 
			    round(s, control$precision)))
	    ## edges not shown in the graph?
	    edgeLabels <- edgeLabels[setdiff(seq(along = edgeLabels),
		    removedEdges(graph))]
	    names(edgeLabels) <- edgeNames(graph)
	    eAttrs$label <- edgeLabels
	    
	    ## does not seem to work
	    #labelfontsize <- rep(4.0, length(edgeNames(graph)))
	    #names(labelfontsize) <- edgeNames(graph)
	    #eAttrs$labelfontsize <- labelfontsize
	}

	## use gray value to code for measure
	#color <- paste("grey", floor(map(m, c(80,1))), sep='')
	if(!is.na(measure)) {
	    m <- quality(rules)[[measure]]
	    lwd <- map(m, c(1,5))
	    names(lwd) <- edgeNames(graph)
	    eAttrs$lwd <- lwd
	}
	
	if(!is.na(shading)) {
	    s <- quality(rules)[[shading]]
	    color <- gray_hcl(map(s, c(0.9,0.1)), alpha=control$alpha)
	    names(color) <- edgeNames(graph)
	    eAttrs$color <- color
	}

	nodesRHS <- unique(names(rightHandSide))

	## node colors
	nodeType <- 1 + (nodes(graph) %in% nodesRHS)

	if(control$nodeLabels) {

	    uni <- !duplicated(leftHandSide)
	    nodeLabels <- structure(
		    as.character(leftHandSide[uni]), 
		    names = names(leftHandSide)[uni])
	    
	    uni <- !duplicated(rightHandSide)
	    nodeLabels <- c(nodeLabels, structure(
			    as.character(rightHandSide[uni]), 
			    names = names(rightHandSide)[uni]))

	    nAttrs <- makeNodeAttrs(graph, 
		    fillcolor = control$nodeColors[nodeType], 
		    label = nodeLabels,
		    shape = "box", width=.75, fixedsize=FALSE)
	} else {
	    nAttrs <- makeNodeAttrs(graph, 
		    fillcolor = control$nodeColors[nodeType],
		    shape = "circle")
	    writeLines("Itemsets in Antecedent (lhs)")
	    print(levels(leftHandSide))
	    writeLines("Itemsets in Consequent (rhs)")
	    print(levels(rightHandSide))
	}


	sgL <- list(list(graph = subGraph(nodesRHS, graph), cluster = FALSE,
			attrs = c(rank = "sink")))
	att <- list(graph = list(rankdir = "LR", rank = "min"))
	att$edge$len <- 2.5 # neato
	att$edge$color <- black_hcl(control$alpha)
	

	if(is.null(control$layout)) control$layout <- "dot"
	
	   ## plot whines about zero length arrows
	    suppressWarnings(pl <- plot(graph, control$layout, 
			    nodeAttrs = nAttrs, 
			    attrs = att, 
			    edgeAttrs = eAttrs,
			    subGList = sgL,
			    main=control$main))

	    legend <- ''

	    if(!is.na(measure))
		legend <- paste(legend, "width: ", measure[1], " (",
		    paste(round(range(m),control$precision), collapse=' - '), ")\n",
		    sep='')

	    if(!is.na(shading)) legend <- paste(legend,
		    "color: ", shading, " (",
		    paste(round(range(s),control$precision), collapse=' - '), ")",
		    sep ='')

	    text(pl@boundBox@upRight@x, pl@boundBox@upRight@y*1.05, 
		    legend, pos=2,cex=.8, xpd=NA)

	    return(invisible(pl))
    } else if(control$type=="itemsets") {

	itemsets <- c(lhs(rules),rhs(rules))
	itemsetLabels <- as.factor(labels(itemsets)$elements)

	fromto <- matrix(as.integer(itemsetLabels), nrow = length(rules))
	#fromto <- matrix(itemsetLabels, nrow = length(rules))
	graph <- ftM2graphNEL(fromto, edgemode = "directed")

	## edges
	eAttrs <- list()
	if(control$edgeLabels) {
	    edgeLabels <- paste(paste(round(m, control$precision), "/", round(s, control$precision)))
	    edgeLabels <- edgeLabels[setdiff(seq(along = edgeLabels),
		    removedEdges(graph))]
	    names(edgeLabels) <- edgeNames(graph)
	    eAttrs$label <- edgeLabels
	    #labelfontsize <- rep("1", length(edgeNames(graph)))
	    #names(labelfontsize) <- edgeNames(graph)
	    #eAttrs$labelfontsize <- labelfontsize
	}

	## use gray value to code for measure
	#color <- paste("grey", floor(map(m, c(80,1))), sep='')
	if(!is.na(measure)) {
	    m <- quality(rules)[[measure]]
	    lwd <- map(m, c(1,5))
	    names(lwd) <- edgeNames(graph)
	    eAttrs$lwd <- lwd
	}
	
	if(!is.na(shading)) {
	    s <- quality(rules)[[shading]]
	    color <- gray_hcl(map(s, c(0.9,0.1)), alpha=control$alpha)
	    names(color) <- edgeNames(graph)
	    eAttrs$color <- color
	}

	if(control$nodeLabels) {
	    nAttrs <- makeNodeAttrs(graph, 
		    fillcolor = control$nodeColors[1], 
		    label = levels(itemsetLabels)[as.integer(nodes(graph))],
		    shape = "box", width=0.75, fixedsize=FALSE)
	} else {
	    nAttrs <- makeNodeAttrs(graph, 
		    fillcolor = control$nodeColors[1],
		    shape = "circle")

	    writeLines("Itemsets")
	    print(levels(itemsetLabels))
	}

	att <-  getDefaultAttrs(layoutType = "neato")
	#att$node$width <- .75
	#att$node$fillcolor <- "#e0e0e0"
	#att$node$shape <- "ellipse"
	#att$node$fixedsize <- FALSE
	#att$node$fontsize <- 8.0
	#att$edge$weight <- 4.0
	att$edge$len <- 2.5 # neato
	att$edge$color <- black_hcl(control$alpha)

	if(is.null(control$layout)) control$layout <- "neato"
	
	## plot whines about zero length arrows
	    suppressWarnings(pl <- plot(graph, control$layout, 
			    nodeAttrs = nAttrs, 
			    attrs = att, 
			    edgeAttrs = eAttrs,
			    recipEdges="distinct",
			    main=control$main))

	    legend <- ''

	    if(!is.na(measure)) legend <- paste(legend,
		    "width: ", measure[1], " (",
		    paste(round(range(m),control$precision), collapse=' - '), ")\n",
		    sep ='')

	    if(!is.na(shading)) legend <- paste(legend,
		    "color: ", shading, " (",
		    paste(round(range(s),control$precision), collapse=' - '), ")",
		    sep ='')

	    text(pl@boundBox@upRight@x, pl@boundBox@upRight@y*1.05,
		    legend, pos=2,cex=.8, xpd=NA)


	    return(invisible(pl))
    } else if(control$type=="items") {
	
	itemNodes <- which(itemFrequency(items(generatingItemsets(rules)), 
			type="absolute") >0)

	ruleNodes <- paste("r", 1:length(rules), sep='')
	
	lhs <- LIST(lhs(rules), decode=FALSE)
	rhs <- LIST(rhs(rules), decode=FALSE)
	
	from_lhs <- unlist(lhs)
	to_lhs <- ruleNodes[rep(1:length(rules), sapply(lhs, length))]

	to_rhs <- unlist(rhs)
	from_rhs <- ruleNodes[rep(1:length(rules), sapply(rhs, length))]

	type <- c(rep(1, length(itemNodes)), rep(2, length(ruleNodes)))
	
	#nodeLabels <- c(itemLabels(rules)[itemNodes], ruleNodes)
	nodeLabels <- c(itemLabels(rules)[itemNodes], rep("", length(ruleNodes)))

	fromto <- cbind(c(from_lhs, from_rhs), c(to_lhs, to_rhs))
	graph <- ftM2graphNEL(fromto, edgemode = "directed",
		V = c(as.character(itemNodes), ruleNodes))
	
	
	if(!is.na(measure)) m1 <- quality(rules)[[measure]]
	else m1 <- rep(1,length(rules))
	
	if(!is.na(shading)) m2 <- quality(rules)[[shading]]
	else m2 <- rep(1,length(rules))

	if(control$nodeLabels) {
	    nAttrs <- makeNodeAttrs(graph, 
		    fillcolor = c(rep(control$nodeColors[1], 
				    length(itemNodes)), 
			    gray_hcl(map(m2, c(0.9,0.1)), alpha=control$alpha)), 
		    label = nodeLabels,
		    shape = c("box", "circle")[type], 
		    width=c(rep(.75, length(itemNodes)), map(m1, c(0.5,1.2))), 
		    fixedsize=FALSE)
	} else {
	    nAttrs <- makeNodeAttrs(graph, 
		    fillcolor = control$nodeColors[type],
		    shape = c("box", "circle")[type])

	    writeLines("Itemsets")
	    print(levels(itemsetLabels))
	}
	
	att <-  getDefaultAttrs(layoutType = "dot")
	#att$node$width <- .75
	#att$node$fillcolor <- "#e0e0e0"
	#att$node$shape <- "ellipse"
	#att$node$fixedsize <- FALSE
	#att$node$fontsize <- 8.0
	#att$edge$weight <- 4.0
	
	att$edge$color <- black_hcl(control$alpha)
	att$edge$len <- 2.0	# neato
	att$graph$rankdir <- "LR" # dot
	att$graph$ranksep <- .75 #dot
	att$graph$mclimit <- 1 #dot

	## fixme: make page smaller

	if(is.null(control$layout)) control$layout <- "dot"
	
	    suppressWarnings(pl <- plot(graph, control$layout, 
			    nodeAttrs = nAttrs, 
			    attrs = att, 
			    #edgeAttrs = eAttrs,
			    recipEdges="distinct",
			    main=control$main))

	    legend <- ''

	    if(!is.na(measure[1])) legend <- paste(legend,
		    "size: ", measure[1], " (",
		    paste(round(range(m1),control$precision), collapse=' - '), ")\n",
		    sep ='')

	    if(!is.na(shading)) legend <- paste(legend,
		    "color: ", shading, " (",
		    paste(round(range(m2),control$precision), collapse=' - '), ")",
		    sep ='')

	    text(pl@boundBox@upRight@x, pl@boundBox@upRight@y*1.05,
		    legend, pos=2,cex=.8, xpd=NA)

	    return(invisible(pl))

    
    } else stop("Unknown plot type.")

}


graph_arules_is <- function(itemsets, measure = "support", shading = NULL, 
	control=NULL, ...) {

    control <- .get_parameters(list(
		    main =paste("Graph for", length(itemsets), "itemsets"),
		    nodeColors = .nodeColors(control$alpha),
		    #nodeColors = c("lightgreen", "skyblue"),
		    alpha = 1,
		    nodeLabels = TRUE,
		    edgeLabels = FALSE,
		    precision = 4,
		    type = "items",
		    layout = "neato",
		    interactive = FALSE
		    ), control)

    if(control$type=="items") {
	
	#sets <- labels(itemsets)
	sets <- as.character(1:length(itemsets))
	items <- LIST(items(itemsets))
	fromto <- cbind(rep(sets, size(itemsets)),unlist(items))
	
	items <- unique(unlist(items))
	graph <- ftM2graphNEL(fromto, edgemode = "directed",
		V = c(sets, items))


	## measure[1] is size
	if(!is.na(measure)) size <- map(quality(itemsets)[[measure[1]]], 
		c(0.5,1.2))
	else size <- rep(.75, length(itemsets))
	
	#if(!is.na(shading)) {
	#    s <- quality(rules)[[shading]]
	#    color <- gray(map(s, c(0.9,0.1)))
	#    names(color) <- edgeNames(graph)
	#    eAttrs$color <- color
	#}

	## node colors
	nodeType <- 1 + (nodes(graph) %in% sets)

	nAttrs <- makeNodeAttrs(graph, 
		fillcolor = control$nodeColors[nodeType], 
		#    label = nodeLabels,
		shape = c("box","circle")[nodeType], 
		width=c(size, rep(.75, length(items))), 
		fixedsize=c(FALSE, TRUE)[nodeType])

	sgL <- list(list(graph = subGraph(as.character(sets), graph), 
			cluster = TRUE,
			attrs = c(rank = "sink")))
	att <- list(graph = list(
			rankdir = "LR", #dot
			rank = "min", #dot
			ranksep = .75, #dot
			mclimit = 1 #dot
			),
		edge=list(
			len = 2, #neato
			color = rgb(0,0,0,control$alpha)
			))
	
	suppressWarnings(pl <- plot(graph, control$layout, 
		nodeAttrs = nAttrs, 
		attrs = att, 
		subGList = sgL,
		main=control$main))
	
	
	#	legend <- ''
	#
	#	if(!is.na(measure))
	#	legend <- paste(legend, "width: ", measure[1], " (",
	#		paste(round(range(m),control$precision), collapse=' - '), ")\n",
	#		sep='')
	#
	#	if(!is.na(shading)) legend <- paste(legend,
	#		"color: ", shading, " (",
	#		paste(round(range(s),control$precision), collapse=' - '), ")",
	#		sep ='')
	#
	#	text(pl@boundBox@upRight@x, pl@boundBox@upRight@y*1.05, 
	#		legend, pos=2,cex=.8, xpd=NA)

	return(invisible(pl))
    }
}
