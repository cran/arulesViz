### default with alpha
.nodeColors <- function(alpha=NULL) {
    if(is.null(alpha)) alpha <- 1
    c(rgb(.4,.8,.4, alpha), rgb(.6,.6,.8, alpha))
}

gray_hcl <- function(level, alpha=NULL) hcl(c=0, l=level*100, 
	alpha=if(!is.null(alpha)) alpha else 1)

black_hcl <- function(alpha=NULL) hcl(c=0, l=0, 
	alpha=if(!is.null(alpha)) alpha else 1)


graph_arules <- function(rules, measure = "support", shading = "lift", 
	control=NULL, ...) {

    control <- .get_parameters(list(
		    main = paste("Graph for", length(rules), "rules"),
		    nodeColors = .nodeColors(
			    if(!is.null(control$alpha)) control$alpha else .8),
			#nodeColors = c("lightgreen", "skyblue"),
		    alpha = .8,
		    cex = 1,
		    itemLabels = TRUE,
		    measureLabels = FALSE,
		    precision = 3,
		    type = "itemsets",
		    layout = layout.fruchterman.reingold,
		    arrowSize = .5,
		    interactive = FALSE,
		    plot = TRUE
		    ), control)

    opar <- par(mar = c(0,0.1,4,0.1))
    on.exit(par(opar))

    if(control$type=="bipartite") {
	## make bipartite graph
	leftHandSide <- as.factor(labels(lhs(rules))$elements)
	rightHandSide <- as.factor(labels(rhs(rules))$elements)
	names(leftHandSide) <- paste("l",as.integer(leftHandSide),sep='')
	names(rightHandSide) <- paste("r",as.integer(rightHandSide),sep='')
	e.list <- cbind(names(leftHandSide), names(rightHandSide))
	
	v <- data.frame(
		name=c(paste("l", 1:length(levels(leftHandSide)),sep=''),
			paste("r", 1:length(levels(rightHandSide)),sep='')),
		label=c(levels(leftHandSide), levels(rightHandSide)))
	
	g <- graph.data.frame(e.list, directed=TRUE, vertices=v)
	
	for(m in names(quality(rules))) {
	    g <- set.edge.attribute(g, m, value=quality(rules)[[m]])
	}

	if(!control$plot) return(g)

	v <- labels(g)
	v.shape <- "circle"
	if(control$itemLabels) {
	    v <- c(levels(leftHandSide), levels(rightHandSide))
	    v.shape <- "none"
	}else{
	    writeLines("LHS")
	    print(levels(leftHandSide))
	    writeLines("RHS")
	    print(levels(leftHandSide))
	}
	
	
	## use gray value to code for measure
	#color <- paste("grey", floor(map(m, c(80,1))), sep='')
	e.width <- 1
	m <- NA
	if(!is.na(measure)) {
	    m <- quality(rules)[[measure]]
	    e.width <- map(m, c(.5,3))
	}
	
	e.color <- gray_hcl(0.3, control$alpha)
	s <- NA
	if(!is.na(shading)) {
	    s <- quality(rules)[[shading]]
	    e.color <- gray_hcl(map(s, c(0.9,0.1)), alpha=control$alpha)
	}

	e.label <- NA
	if(control$measureLabels) {
	    if(is.na(m) || is.na(s)) {
		if(!is.na(m)) e.label <- round(m, control$precision)
		if(!is.na(s)) e.label <- round(s, control$precision)
	    }else{
		e.label <- paste(round(m, control$precision),"/", 
			round(s, control$precision), sep='')
	    }
	}
	

	legend <- ''
	if(!is.na(measure))
	    legend <- paste(legend, "width: ", measure[1], " (",
		paste(round(range(m),control$precision), collapse=' - '), ")\n",
		sep='')

	if(!is.na(shading)) legend <- paste(legend,
		"color: ", shading, " (",
		paste(round(range(s),control$precision), collapse=' - '), ")",
		sep ='')

	v.color <-  .nodeColors(.5)[c( 
		rep(1,length(levels(leftHandSide))),
		rep(2, length(levels(rightHandSide))))]
	
	if(!control$interactive) {
	    
	    plot(g, layout=control$layout, 
		    vertex.label.family="Helvetica",
		    edge.label.family="Helvetica",
		    vertex.shape=v.shape, 
		    vertex.label=v,
		    vertex.label.cex=control$cex,
		    vertex.label.color="black",
		    vertex.color = v.color,
		    #vertex.size=v.size,
		    edge.width=e.width,
		    edge.label=e.label,
		    edge.label.cex=control$cex*.6,
		    edge.color=e.color,
		    edge.arrow.size=control$arrowSize,
		    main=control$main,...
		    )

	    mtext(legend, adj=1,cex=control$cex*.8)
	}else {
	    tkplot(g, layout=control$layout, 
		    #vertex.shape=v.shape, 
		    vertex.label=v,
		    #vertex.label.cex=.7,
		    #vertex.label.color="black",
		    vertex.color = v.color,
		    #vertex.size=v.size,
		    #edge.width=e.width,
		    edge.label=e.label,
		    #edge.label.cex=.5,
		    edge.color=e.color,
		    #main=control$main
		    )
	}

	return(invisible(g))
	
    } else if(control$type=="itemsets") {

	itemsets <- c(lhs(rules),rhs(rules))
	itemsetLabels <- as.factor(labels(itemsets)$elements)
	v.labels <- data.frame(
		name=1:length(levels(itemsetLabels)),
		label=levels(itemsetLabels ), 
		stringsAsFactors = FALSE)
	e.list <- matrix(as.integer(itemsetLabels), nrow = length(rules))

	g <- graph.data.frame(e.list, directed=TRUE, vertices=v.labels)

	for(m in names(quality(rules))) {
	    g <- set.edge.attribute(g, m, value=quality(rules)[[m]])
	}
	
	if(!control$plot) return(g)

	v <- v.labels[,1]
	v.shape <- "circle"
	if(control$itemLabels) {
	    v <- v.labels[,2]
	    v.shape <- "none"
	}else{
	    writeLines("itemsets")
	    print(v.labels[,2])
	}
	
	
	## use gray value to code for measure
	#color <- paste("grey", floor(map(m, c(80,1))), sep='')
	e.width <- 1
	m <- NA
	if(!is.na(measure)) {
	    m <- quality(rules)[[measure]]
	    e.width <- map(m, c(.5,3))
	}
	
	e.color <- gray_hcl(0.3, control$alpha)
	s <- NA
	if(!is.na(shading)) {
	    s <- quality(rules)[[shading]]
	    e.color <- gray_hcl(map(s, c(0.9,0.1)), alpha=control$alpha)
	}

	e.label <- NA
	if(control$measureLabels) {
	    if(is.na(m) || is.na(s)) {
		if(!is.na(m)) e.label <- round(m, control$precision)
		if(!is.na(s)) e.label <- round(s, control$precision)
	    }else{
		e.label <- paste(round(m, control$precision),"/", 
			round(s, control$precision), sep='')
	    }
	}
	

	legend <- ''
	if(!is.na(measure))
	    legend <- paste(legend, "width: ", measure[1], " (",
		paste(round(range(m),control$precision), collapse=' - '), ")\n",
		sep='')

	if(!is.na(shading)) legend <- paste(legend,
		"color: ", shading, " (",
		paste(round(range(s),control$precision), collapse=' - '), ")",
		sep ='')

	v.size <- 15 # default
	#v.color <- NA   

	if(!control$interactive) {

	    plot(g, layout=control$layout, 
		    vertex.label.family="Helvetica",
		    edge.label.family="Helvetica",
		    vertex.shape=v.shape, 
		    vertex.label=v,
		    vertex.label.cex=control$cex,
		    vertex.label.color="black",
		    #vertex.color = v.color,
		    #vertex.size=v.size,
		    edge.width=e.width,
		    edge.label=e.label,
		    edge.label.cex=control$cex*.6,
		    edge.color=e.color,
		    edge.arrow.size=control$arrowSize,
		    main=control$main,...
		    )

	    mtext(legend, adj=1,cex=control$cex*.8)
	
	}else {
	    
	    tkplot(g, layout=control$layout, 
		    #vertex.shape=v.shape, 
		    vertex.label=v,
		    #vertex.label.cex=.7,
		    #vertex.label.color="black",
		    #vertex.color = v.color,
		    vertex.size=v.size,
		    #edge.width=e.width,
		    edge.label=e.label,
		    #edge.label.cex=.5,
		    edge.color=e.color,
		    #main=control$main
		    )
	}
	return(invisible(g))

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

	e.list <- cbind(c(from_lhs, from_rhs), c(to_lhs, to_rhs))
	v.labels <- data.frame(
		name=c(as.character(itemNodes), ruleNodes),
		label=nodeLabels,
		stringsAsFactors = FALSE)

	g <- graph.data.frame(e.list, directed=TRUE, vertices=v.labels)
	
	if(!control$plot) return(g)
	
	for(m in names(quality(rules))) {
	    g <- set.vertex.attribute(g, m, which(type==2)-1, 
		    quality(rules)[[m]])
	}

	
	v <- v.labels[,1]
	v[type==2] <- ""
	v.shape <- c("rectangle","circle")[type]
	if(control$itemLabels) {
	    v <- v.labels[,2]
	    v.shape <- c("none","circle")[type]
	}else{
	    writeLines("items")
	    print(v.labels[type==1,])
	}
	
	e.width <- 1
	e.color <- gray_hcl(.6,control$alpha)
	
	m <- NA
	if(!is.na(measure)) {
	    m <- quality(rules)[[measure]]
	    v.size <- c(rep(15, length(itemNodes)),
			    map(m, c(5,20)))
	}
	
	s <- NA
	if(!is.na(shading)) {
	    s <- quality(rules)[[shading]]
	    v.color <- c(rep(control$nodeColors[1], length(itemNodes)),
		    gray_hcl(map(s, c(0.9,0.1)), alpha=control$alpha)) 
									     
	
	}

	if(control$measureLabels) {
	    if(is.na(m) || is.na(s)) {
		if(!is.na(m)) e.label <- round(m, control$precision)
		if(!is.na(s)) e.label <- round(s, control$precision)
	    }else{
		v[type==2] <- paste(round(m, control$precision),"\n", 
			round(s, control$precision), sep='')
	    }
	}


	legend <- ''
	if(!is.na(measure[1])) legend <- paste(legend,
		"size: ", measure[1], " (",
		paste(round(range(m),control$precision), collapse=' - '), ")\n",
		sep ='')

	if(!is.na(shading)) legend <- paste(legend,
		"color: ", shading, " (",
		paste(round(range(s),control$precision), collapse=' - '), ")",
		sep ='')


	if(!control$interactive) {

	    plot(g, layout=control$layout, 
		    vertex.label.family="Helvetica",
		    edge.label.family="Helvetica",
		    vertex.shape=v.shape, 
		    vertex.label=v,
		    vertex.label.cex=control$cex,
		    vertex.label.color="black",
		    vertex.color = v.color,
		    vertex.size=v.size,
		    edge.width=e.width,
		    #edge.label=e.label,
		    edge.label.cex=control$cex*.6,
		    edge.color=e.color,
		    edge.arrow.size=control$arrowSize,
		    main=control$main, ...
		    )

	    mtext(legend, adj=1,cex=control$cex*.8)
	}else {
	    tkplot(g, layout=control$layout, 
		    #vertex.shape=v.shape, 
		    vertex.label=v,
		    #vertex.label.cex=.7,
		    #vertex.label.color="black",
		    vertex.color = v.color,
		    vertex.size=v.size,
		    #edge.width=e.width,
		    #edge.label=e.label,
		    #edge.label.cex=.5,
		    edge.color=e.color,
		    #main=control$main
		    )
	}

	return(invisible(g))

    } else stop("Unknown plot type.")

}

graph_arules_is <- function(itemsets, measure = "support", shading = NULL, 
	control=NULL, ...) {

    control <- .get_parameters(list(
		    main =paste("Graph for", length(itemsets), "itemsets"),
		    nodeColors = gray_hcl(.8, 
			    if(!is.null(control$alpha)) control$alpha else .8),
		    edgeColors = gray_hcl(.3, 
			    if(!is.null(control$alpha)) control$alpha else .8),
		    #itemLabels = TRUE,	    ### not implemented yet
		    #measureLabels = FALSE,
		    cex = 1,
		    precision = 3,
		    type = "items",
		    layout = layout.fruchterman.reingold,
		    alpha = .8,
		    interactive = FALSE,
		    plot = TRUE
		    ), control)

    opar <- par(mar = c(0,0.1,4,0.1))
    on.exit(par(opar))


    if(control$type=="items") {
	
	sets <- as.character(1:length(itemsets))
	items <- LIST(items(itemsets))
	e.list <- cbind(unlist(items),rep(sets, size(itemsets)))
	items <- unique(unlist(items))
	type <- c(rep(1,length(sets)), rep(2,length(items)))
	v.labels <- data.frame(
		name=c(sets,items),
		label=c(sets,items),
		stringsAsFactors = FALSE)


	v <- v.labels[,2]
	v[type==1] <- ''

	g <- graph.data.frame(e.list, directed=TRUE, vertices=v.labels)
	
	for(m in names(quality(itemsets))) {
	    g <- set.vertex.attribute(g, m, 
		    (1:length(itemsets))-1, value=quality(itemsets)[[m]])
	}
	
	if(!control$plot) return(g)


	v.shape <- c("circle","none")[type]
	v.color <- control$nodeColors
	
	e.width <- 1
	e.color <- control$edgeColors
	e.label <- NA

	m <- NA
	if(!is.na(measure)) {
	    m <- quality(itemsets)[[measure]]
	    v.size <- c(map(m, c(5,20)), 
		    rep(15, length(items)))
	    #v.color <- c(gray_hcl(map(m, c(0.9,0.1)), alpha=control$alpha), 
	    #	    rep(NA, length(items)))
	}
	

	
	legend <- ''
	if(!is.na(measure))
	    legend <- paste(legend, "size: ", measure[1], " (",
		paste(round(range(m),control$precision), collapse=' - '), ")\n",
		sep='')

	if(!control$interactive) {
	
	    plot(g, layout=control$layout, 
		vertex.label.family="Helvetica",
		edge.label.family="Helvetica",
		vertex.shape=v.shape, 
		vertex.label=v,
		vertex.label.cex=control$cex,
		vertex.label.color="black",
		vertex.color = v.color,
		vertex.size=v.size,
		edge.width=e.width,
		edge.label=e.label,
		edge.label.cex=control$cex*.6,
		edge.color=e.color,
		main=control$main,
		...
		)

	mtext(legend, adj=1,cex=control$cex*.8)
    }else{
    
	    tkplot(g, layout=control$layout, 
		vertex.shape=v.shape, 
		vertex.label=v,
		#vertex.label.cex=.7,
		#vertex.label.color="black",
		vertex.color = v.color,
		vertex.size=v.size,
		#edge.width=e.width,
		edge.label=e.label,
		#edge.label.cex=.5,
		edge.color=e.color,
		#main=control$main
		)
    
    }
	
	return(invisible(g))
    }
}
