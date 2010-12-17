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

saveGraphML <- function(x, file, measure="support") {
    l <- labels(lhs(x))$elements
    r <- labels(rhs(x))$elements
    u <- as.factor(unique(c(l, r)))

    f <- file(file, "w")

    ### write the xml version and links
    writeLines("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", f)
    writeLines("<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">", f)
    
    ### write the attributes
    writeLines("<key id=\"name\" for=\"node\" attr.name=\"name\" attr.type=\"string\"/>", f)
    writeLines("<key id=\"weight\" for=\"edge\" attr.name=\"weight\" attr.type=\"double\"/>", f)
    writeLines("<graph id=\"G\" edgedefault=\"directed\">", f)

    ### write the rules
    writeLines(paste('<node id=" ',as.integer(u),' ">','<data key="name">',u, '</data></node>', sep=""), f)
    writeLines(paste('<edge source="',as.integer(u)[match(l,u)],'" target="',as.integer(u)[match(r, u)], '"><data key="weight">',quality(x)[[measure]],'</data></edge>') , f)


    writeLines("</graph>", f)
    writeLines("</graphml>", f)

    ### end
    close(f)

}
