## save rules in graphviz dot format

saveDot <- function(x, file) {
    l <- labels(lhs(x))$elements
    r <- labels(rhs(x))$elements
    u <- as.factor(unique(c(l, r)))

    f <- file(file, "w")
    
    ### write preamble
    writeLines("digraph foo {", f)
   
    ### write items
    writeLines(paste(as.integer(u), ' [label="',u, '"];', sep=""), f)

    ### write rules
    writeLines(paste(as.integer(u)[match(l, u)], ' -> ', 
		    as.integer(u)[match(r, u)], ';', sep=""), f)

    ### end
    writeLines("}", f)
    close(f)
}


## save rules in GraphML format
saveGraphML <- function(x, file, trans = NULL) {
    
    l <- lhs(x)
    r <- rhs(x)
    u <- unique(c(l,r))
    
    if(!is.null(trans)) isSupport <- support(u, trans)
    else isSupport <- NULL

    sourceLabel <- labels(l)$elements
    targetLabel <- labels(r)$elements
    nodeLabel <- labels(u)$elements
   
    q <- quality(x)
    edgeWeightName <- names(q) 

    f <- file(file, "w", encoding="UTF-8")

    ### write the xml version and links
    writeLines('<?xml version="1.0" encoding="UTF-8"?>', f)
    writeLines('<graphml xmlns="http://graphml.graphdrawing.org/xmlns" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd">', f)
    
    ### define the attributes
    #writeLines('<key id="label" for="node" attr.name="label" attr.type="string"/>', f)
    if(!is.null(isSupport)) {
	writeLines('<key id="support" for="node" attr.name="support" attr.type="double"/>', f)
    }
    
    writeLines(paste('<key id="',edgeWeightName,'" for="edge" attr.name="',
		    edgeWeightName,'" attr.type="double"/>', sep=''), f)
    
    ### start directed graph
    writeLines('<graph id="G" edgedefault="directed">', f)

    ### write the nodes
    writeLines(paste('<node id="',nodeLabel,'">','</node>', sep=""), f)
    #writeLines(paste('<node id="',nodeLabel,'">','<data key="name">',u, 
    #		    '</data></node>', sep=""), f)

    ### write the rules

    #paste('<edge source="', frame$source, '" target="', frame$target, '">')
    
    for(i in 1:length(x)) {
	writeLines(paste('<edge source="',sourceLabel[i],
			'" target="',targetLabel[i],'">', sep=""), f)

	for(j in 1:ncol(q)) {
	    writeLines(paste('<data key="',edgeWeightName[j],'">',q[i,j],
			    '</data>', sep=""), f)
	}

	writeLines('</edge>', f)

    }

    writeLines("</graph>", f)
    writeLines("</graphml>", f)

    ### end
    close(f)

}
