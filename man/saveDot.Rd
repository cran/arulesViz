\name{saveDot}
\Rdversion{1.1}
\alias{saveDot}
\alias{saveGraphML}
\title{
Save rule set as a graph description.
}
\description{
This function 
saves a rule sat as a graph description in Graphviz's dot or GraphML format.
}
\usage{
saveDot(x, file)
saveGraphML(x, file, measure="support")
}
\arguments{
  \item{x}{ an object of class "rules"}
  \item{file}{ file name}
  \item{measure}{ an interest measure used as the attribure for edges 
    (e.g. \code{"support"})}
}
                            
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
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
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
