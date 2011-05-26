## save rules/itemsets as a graph for external programs 

saveAsGraph <- function(x, file, type=NULL, format="graphml") {
    g <- plot(x, method="graph", control=list(plot=FALSE, type=type))
    write.graph(g, file, format=format)
}


