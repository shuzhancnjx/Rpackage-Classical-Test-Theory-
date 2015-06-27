\name{Spearman.Brown}
\alias{Spearman.Brown}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Functions for Spearman.Brown reliability}
\description{
 This function calculates either a predicted reliability for a measure 
give the original reliability and a new test length or the required test length to achieve a desired level of reliability. }
\usage{
spearman.brown(r.xx, input = 2, n.or.r = "n")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r.xx}{ The original relibility }
  \item{input}{ The new test length or a desired level of reliability, depending on "n.or.r=n" }
  \item{n.or.r}{ If n.or.r="n", the function will return a new reliability; 
if n.or.r="r", the function will return the factor by which the test length must change to achieve a desired level of reliability.}
}
\details{
 If n.or.r="n", the function will return a new reliability and input should be the factor by which the test length is to be changed.
 If n.or.r="r", the function will return the factor by which the test length must change to achieve a desired level of reliability (provided in input). 
}
\references{ ~put references to the literature/web site here ~ }
\author{ John Willse, Zhan Shu }
\examples{

# old relibility is 0.6, the test is extened by 2 times, the relibility of new test is:
spearman.brown(0.6,2,"n")

# old relibility is 0.5, the new test's reliability is 0.8, the test length is:
spearman.brown(0.5, 0.8, "r")
}
