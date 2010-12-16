doubledecker_arules <- function(rules, measure ="support", data, 
	control=list(), ...) {
    
    if(length(rules) != 1) stop("only can visualize one rule.")
    if(is.null(data)) stop("Data missing.")
    
    control <- .get_parameters(list(
		    main = "Doubledecker plot for 1 rule",
		    type = "doubledecker",
		    interactive = FALSE
		    ), control)

    table <- getTable(rules, data)

    if(control$type=="doubledecker")
    doubledecker(table, margins=c(2,8,length(dim(table) + 2), 2), 
	    main = control$main, ...) 
    else {
    control$main <- "Mosaic plot for 1 rule"
    mosaic(table, highlighting = length(dim(table)),
            main = control$main, ...)
    }
}


