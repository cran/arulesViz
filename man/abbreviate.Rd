\name{abbreviate}
\Rdversion{1.1}
\alias{abbreviate}
\alias{abbreviate.transactions}
\alias{abbreviate.rules}
\alias{abbreviate.itemMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Abbreviate function for item names in transactions, itemMatrix and rules.
}
\description{
This function in overloaded by the defined function. This actually takes the transactions and abbreviate their item names.%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{abbreviate}{transactions}(data, minlength = 4, ...,method = "both.sides")
\method{abbreviate}{rules}(data, minlength = 4, ...,method = "both.sides")
\method{abbreviate}{itemMatrix}(data, minlength = 4, ...,method = "both.sides")
}
% - maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ an object of class "transactions", "itemMatrix" and "rules"
%%     ~~Describe \code{rules} here~~
}
  \item{minlength}{ size of abbreviation
%%     ~~Describe \code{measure} here~~
}
  \item{method}{ to apply both sides for level and value }
  \item{\dots}{ further arguments; currently unused }
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
\author{
%%  ~~who you are~~
Sudheer Chelluboina and Sudheer Chelluboina Enhancements based on ideas and code by Martin Vodenicharov.
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
data(Adult)
Adult <- abbreviate(Adult, 9)
}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
