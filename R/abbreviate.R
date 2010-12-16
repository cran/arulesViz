abbreviate.transactions <- function(data, minlength = 4, ..., 
        method = "both.sides"){
    
    ## both sides helps with labels of form variable=level 
    
    itemInfo(data)$labels_orig <- itemInfo(data)$labels

    
    itemInfo(data)$labels<- as.factor(
            abbreviate(itemInfo(data)$labels, minlength, ..., method = method)
            )
    
    data
}

abbreviate.itemMatrix <- function(data, minlength = 4, ..., 
        method = "both.sides"){
    
    ## both sides helps with labels of form variable=level 
    
    itemInfo(data)$labels_orig <- itemInfo(data)$labels

    
    itemInfo(data)$labels<- as.factor(
            abbreviate(itemInfo(data)$labels, minlength, ..., method = method)
            )
    
    data
}




abbreviate.rules <- function(data, minlength = 4, ..., 
        method = "both.sides"){
    
    new("rules", lhs = abbreviate(lhs(data), minlength, ..., method), 
	    rhs = abbreviate(rhs(data), minlength, ..., method),
	quality = quality(data))
}

abbreviate.itemsets <- function(data, minlength = 4, ..., 
        method = "both.sides"){
    
    new("itemsets", items = abbreviate(items(data), minlength, ..., method),
	quality = quality(data))
}


## Make abbrevate generic
abbreviate.default <- abbreviate
abbreviate <- function(...) UseMethod("abbreviate")
