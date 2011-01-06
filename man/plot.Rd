\name{plot}
\Rdversion{1.1}
\alias{plot}
\alias{plot.itemsets}
\alias{plot.rules}
\title{
Plot method to visualize association rules.
}
\description{
This is the S3 method to visualize association rules and itemsets.
Implemented are several popular visualization methods 
including scatter plots with shading (two-key plots), 
graph based visualizations
and doubledecker plots.
}
\usage{
\method{plot}{rules}(x, method = NULL, measure = "support", shading = "lift", interactive = FALSE, data = NULL, control = NULL, ...)
\method{plot}{itemsets}(x, method = NULL, measure = "support", interactive=FALSE, data = NULL, control = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class "rules" or "itemsets". }
  \item{method}{ a string with value "scatterplot", "two-key plot", "matrix", 
  "matrix3D",  "mosaic", 
  "doubledecker", "graph", "paracoord" or "grouped", "iplots" selecting the 
  visualization method (see details section).}
  
  \item{measure}{ measure(s) of interestingness 
  (e.g., "support", "confidence", "lift") used in the visualization. Some 
  visualization methods need one measure, others take a vector with two 
  measures (e.g., scatterplot). In some plots (e.g., graphs) \code{NA} 
  can be used to suppress using a measure. }
  
  \item{shading}{ measure of interestingness used
  for the color of the points/arrows visualized
  (e.g., "support", "confidence", "lift"). The default is "lift".
  \code{NA} can be often used to suppress shading.}
  
  \item{interactive}{ enable interactive exploration (not implemented for
  each method).}
  
  \item{control}{ a list of control parameters for the plot. The available
  control parameters depends on the visualization technique. Some 
  parameters are 
  col, kind, itemlabels, reorder, orderBy, interactive, xscale, 
  yscale, pch and size.}
  
  \item{data}{  the dataset (class "transactions") 
  used to generate the rules/itemsets. Only 
  "mosaic" and "doubledecker" require the original data.}
  
  \item{\dots}{ further arguments are typically passed on to the 
  used plotting function. }
}
\details{
Here are some details for each visualization method.

\emph{scatterplot, two-key plot} 
This visualization method draws a two dimensional scatterplot with different
measures of interestingness (parameter "measure") on the axes and a third 
measure (parameter "shading") is represented by the points color. 
There is a special value for shading called "order". With this value
the color of the points represents the length (order) of the rule.
This is used for two-key plots.
The list of control parameters for this method is "pch" 
(use filled symbols: 20--25),
"cex", "xlim" and "ylim", "main", "jitter" 
(a number greater than 0 adds jitter to the points).
Interactive manipulations are available.

\emph{matrix, matrix3D} 
Arranges the association rules as a matrix with the itemsets in the antecedents
on one axis and the itemsets in the consequent on the other.  The interest
measure is either visualized by a color (darker means a higher value for the
measure) or as the height of a bar (\code{type="3D"} for control).
For convenience the method name "matrix3D" is also provided.
The list of control parameters for this method is
"main", "col", "reorder", "orderBy", "reorderMethod", "reorderControl" 
and "type".
Type can be "grid", "image" or "3D" (scatterplot3D) and defines the way the data is rendered. 
\code{...} arguments are passed on to the respective plotting function.

If reorder is set to TRUE, then the itemsets on the x and y-axes
are reordered to bring rules with similar values for the interest measure
closer together and make the plot clearer. Orderby specifies the measure of 
interest for reordering (default is the visualized measure). 
ReorderMethod and reorderControl
contain the seriation method and control arguments
used for reordering (see \code{seriation()} method in \pkg{seriation}).

\emph{grouped}
Grouped matrix-based visualization. Antecedents (columns) in the matrix are
grouped using clustering. 
Interactive manipulations are available. 

\emph{graph}
Represents the rules (or itemsets) as a graph.
Control arguments are "main", "type", "nodeColors",
"nodeLabels" and "edgeLabels".
Type can be "itemsets" (default), "items" or "bipartite". 
Control arguments are "main",
"nodeColors",
"nodeLabels" (\code{TRUE} - use item/itemset names instread of ids),
"edgeLabels" (\code{FALSE} - plot values of interest measures),
"precision" (for plotting measures),
"layout" (use a different graphviz layout, e.g., dot, neato, fdp, circo),

%% itemsets missing

\emph{doubledecker, mosaic}
Represents a single rule as a doubledecker or mosaic plot.
Parameter data has to be specified to compute the needed contingency
table. Control parameters are "main".

\emph{paracoord}
Represents the rules (or itemsets) as a parallel coordinate plot.


\emph{iplots}
Uses interactive plots (package iplots) which support selection, highlighting, 
brushing, etc. Currently plots a scatterplot (support vs. confidence) and
several histograms. 
Interactive manipulations are available. 

}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
%%  ~~who you are~~
Michael Hahsler and Sudheer Chelluboina. Some visualizations are based on 
the implementation by Martin Vodenicharov.
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%scatterplot3d, levelplot, mosaic, doubledecker
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
data(Groceries)
rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.5))
rules

## scatterplot
plot(rules)
## try: sel <- plot(rules, interactive=TRUE)

## Two-key plot is a scatterplot with shading = "order"
plot(rules, shading="order", control=list(main = "Two-key plot"))

## the following techniques work better with fewer rules
subrules <- rules[quality(rules)$confidence > 0.8]

## 2D matrix with shading
plot(subrules, method="matrix", measure="lift")
plot(subrules, method="matrix", measure="lift", control=list(reorder=TRUE))

## 3D matrix
plot(subrules, method="matrix3D", measure="lift")
plot(subrules, method="matrix3D", measure="lift", control=list(reorder=TRUE))

## matrix with two measures
plot(subrules, method="matrix", measure=c("lift", "confidence"))
plot(subrules, method="matrix", measure=c("lift", "confidence"), 
	control=list(reorder=TRUE))

## try: plot(subrules, method="matrix", measure="lift", interactive=TRUE, control=list(reorder=TRUE))

## grouped matrix plot
plot(rules, method="grouped")
plot(rules, method="grouped", control=list(k=30))
## try: sel <- plot(rules, method="grouped", interactive=TRUE)

## graphs only work with very few rules
subrules2 <- sample(rules, 10)
plot(subrules2, method="graph")
plot(subrules2, method="graph", 
	control=list(type="bipartite"))
plot(subrules2, method="graph", 
	control=list(type="items"))

## parallel coordinates plot
plot(subrules2, method="paracoord")
plot(subrules2, method="paracoord", control=list(reorder=TRUE))

## Doubledecker plot only works for a single rule
oneRule <- sample(rules, 1)
plot(oneRule, method="doubledecker", data = Groceries)

## use iplots
## try: sel <- plot(rules, method="iplots", interactive=TRUE)


## for itemsets
itemsets <- eclat(Groceries, parameter = list(support = 0.02))
plot(itemsets, method="paracoord", control=list(alpha=.5, reorder=TRUE))
plot(itemsets, method="graph")
}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
