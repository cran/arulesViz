iplot_arules <- function(rules,
	measure = c("support", "confidence"), shading="lift", data=NULL, 
	control = NULL, ...) {
   
    control <- .get_parameters(list(
		    interactive = FALSE
		    ), control)

    if(!require("iplots")) stop ("iplots requires package 'iplots'")
    
    iplot(quality(rules)[[measure[1]]], quality(rules)[[measure[2]]],
	    xlab = measure[1], ylab = measure[2])
    ihist(quality(rules)[[measure[1]]], 
	    main = paste("Histogram (", measure[1], ")", sep=''))
    ihist(quality(rules)[[measure[2]]],
	    main = paste("Histogram (", measure[2], ")", sep=''))
    ihist(quality(rules)[[shading]],
	    main = paste("Histogram (", shading, ")", sep=''))
   
    ## parallel coordinate
    ## does not seem so helful
    #l <- LIST(lhs(rules), decode=FALSE)
    #r <- LIST(rhs(rules), decode=FALSE)
    #
    #lk<-list()
    #for(i in 1:length(l)) lk[i]<-list(c(l[[i]],r[[i]]))
    #
    #len<-length(lk[[max(length(lk))]])
    #lk1<- sapply(lk, function(x) {length(x) <-len; x})
    #ll <- as.data.frame(t(lk1))
    #names(ll) <- 1:length(ll)
    #ipcp(ll, main ="Parallel Coord. (rules)")
    
    ## imosaic
    #l2 <- LIST(lhs(rules))
    #r2 <- LIST(rhs(rules))
    #lk2<-0
    #for(i in 1:length(l2))
    #{
    #        lk2[i]<-list(c(l2[[i]],r2[[i]]))
    #}
    #len<-length(lk2[[max(length(lk2))]])
    #lk3<- sapply(lk2, function(x) {length(x) <-len; x})
    #ll2 <- as.data.frame(t(lk3))
    #attach(ll2)
    #imosaic(ll2,type="mul")
   
    if(control$interactive) {
	cat("\n","Interactive mode.","\n",
		"Press 's' to show the selected rules,",
		"'r' to return the selected rules or",
		"'x' to exit interactive mode.", "\n")

	while(1) {
	    y<-scan(n=1, what = '', quiet=TRUE)

	    if(y=='x') {
		## close all iplots
		while(iplot.cur()) iplot.off(iplot.cur())
		return(invisible(NULL))
	    }

	    sel <-  iset.selected()
	    r_sel <- rules[sel]

	    if(y=='s') {
		print(r_sel)
		inspect(head(sort(r_sel, by=shading)))
	    }

	    if(y=='r') {
		## close all iplots
		while(iplot.cur()) iplot.off(iplot.cur())
		return(r_sel)
	    }
	}
    }
}
