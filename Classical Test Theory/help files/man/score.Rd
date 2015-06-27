\name{Score}
\alias{score}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to score the response files }
\description{
  This function can score the dichotomous item responses. 
  The function can also call and return results from function reliability.
}
\usage{
score(items, key = NA, output.scored = F, ID = NA, rel = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{items}{ The item response }
  \item{key}{ The answer key }
  \item{output.scored}{ If output.scored=F (the default) only a vector of scores is returned. 
                        If output.scored=T a matrix containing scored items is returned.  }
  \item{ID}{ If respondent IDs are provided scores are labeled appropriately.}
  \item{rel}{ If rel=T, the function will call the function reliability and provide that output as well. }
}
\references{ ~put references to the literature/web site here ~ }
\author{ John Willse, Zhan Shu }
\seealso{ reliability }
\examples{
# x is the response file, y is the key
x:(data frame is preferred)
x<-data.frame(matrix(c( C,C,C,B,B,
 C,C,D,C,B,
 C,C,C,B,B,
 D,C,C,B,B,
 D,C,D,C,B,
 C,C,C,B,C,
 C,B,C,B,B,
 C,B,D,B,C,
 C,C,C,C,C,
 C,B,D,B,B),nrow=10,ncol=5,byrow=TRUE,
       dimnames=list(c(paste("P",c(1:10),sep="")),c(paste("I",c(1:5),sep="")))))

y<-c(D,B,D,C,C)

# the score of every preson is needed 
score(x,y)

# the score, scored file and relibility are needed
score(x,y,output.scored=T,rel=T)

# if the ID is available
score(x,y,ID="file.name")

# x is a scored file, no key is provided:
  X: (data.frame is preferred)
x<-data.frame(matrix(c(0,0,0,0,0,
       0,0,1,1,0,
       0,0,0,0,0,
       1,0,0,0,0,
       1,0,1,1,0,
       0,0,0,0,1,
       0,1,0,0,0,
       0,1,1,0,1,
       0,0,0,1,1,
       0,1,1,0,0),nrow=10,ncol=5,byrow=TRUE,
       dimnames=list(c(paste("P",c(1:10),sep="")),c(paste("I",c(1:5),sep="")))))

score(x)
  }
