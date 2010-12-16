
plot.rules <- function(x, method = NULL, measure = "support", 
	shading = "lift", interactive = FALSE, data = NULL, control = NULL, ...) {
    ## methods
    methods <- c(
            "matrix",       
            "mosaic",
	    "doubledecker",
	    "graph",
	    "paracoord",
            "scatterplot",	## standard
            "grouped",
            "two-key plot",
            "matrix3D",
	    "iplots"
            )

    if(is.null(method)) methodNr <- 6
    else methodNr <- pmatch(tolower(method), tolower(methods))
        if(is.na(methodNr)) stop (paste("Unknown method:",sQuote(method)))

    ## add interactive
    control$interactive <- interactive    

    ## work horses
    if (methodNr == 1) matrix_arules(x, measure = measure, control,...)
    else if (methodNr == 2) doubledecker_arules(x, measure = measure, 
		data = data, c(control, list(type="mosaic")), ...)
    else if (methodNr == 3) doubledecker_arules(x, measure = measure, 
		data = data, control, ...)
    else if (methodNr == 4) graph_arules(x, measure = measure, 
		shading = shading, control, ...)
    else if (methodNr == 5) paracoord_arules(x, measure = measure, 
		shading = shading, control = control,...)
    else if (methodNr == 6) {
	if(length(measure)<2) measure[2] <- "confidence"
	scatterplot_arules(x, measure = measure, 
		shading = shading, control)
    }
    else if (methodNr == 7) clusterplot_arules(x, measure= measure, 
		shading = shading, control=control, ...)
    else if (methodNr == 8) scatterplot_arules(x, 
		measure = c("support", "confidence"), shading = "order", 
	    control, ...)
    else if (methodNr == 9) matrix_arules(x, measure = measure, 
		control = c(control, list(type="3d")), ...)
    else if (methodNr == 10) {
	if(length(measure)<2) measure[2] <- "confidence"
	iplot_arules(x, measure = measure, 
		shading=shading, data=data, control=control, ...)
    }
}

plot.itemsets <- function(x, method = NULL, measure = "support", interactive=FALSE, data = NULL, control = NULL, ...) {
    ## methods
    methods <- c(
	    "graph",
	    "paracoord"
            )

    if(is.null(method)) methodNr <- 1
    else methodNr <- pmatch(tolower(method), tolower(methods))
        if(is.na(methodNr)) stop (paste("Unknown method:",sQuote(method)))

    ## add interactive
    control$interactive <- interactive    

    ## work horses
    if (methodNr == 1) graph_arules_is(x, measure = measure,
	    control= control, ...)
    else if (methodNr == 2) paracoord_items(x, measure = measure,
		control= control, ...)

}
