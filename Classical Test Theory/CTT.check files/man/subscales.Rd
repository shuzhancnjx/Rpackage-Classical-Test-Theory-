\name{Subscales}
\alias{subscales}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to investiage the scores under cenrtain quality matrix }
\description{
  This convenience function is provided to facilitate extracting subscales from a single set of item responses. 
}
\usage{
subscales(items, scales, scale.names = NA, score.scales = F, check.reliability = F,key=NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{items}{ The item response(scored or not) }
  \item{scales}{ A design matrix, with items represented in rows and separate subscales represented in columns.
  An item may appear in more than one subscale.}
  \item{scale.names}{ Optional names for the subscales. }
  \item{score.items}{ If responses are not scored, the score.items=T, and key should be provided }
  \item{check.reliability}{ If check.reliability=T, the reliability for each subscale will be calculted }
  \item{key}{ Optional key, required only if score.scales=T.}
}
\details{
  This function provides an easy way to create new datasets from a single set of item responses. 
This function is also a front end for score and reliability, enabling the item responses to be partitioned into separate scales, scored, and reliability analyses performed using this one function.
}
\value{
  A list is returned. Results for each subscale (i.e., column in the scales matrix) are provided as sparate elements in that list.
  \item{$Q.1$score}{Each examinee's score on the associated subscale}
  \item{$Q.1$reliablity}{Reliability results(if requested)for the associated subscale}
  \item{$Q.1$scored}{The scored item responses(if required)for each respondent for the associated subscale}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ John Willse, Zhan Shu}
\seealso{ relibility }
\examples{

# the x is the socred file, y is the quality matrix with items as rows, check.reliability is required:
x:(data.frame is preferred)
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
y:(data.frame is preferred)
y<-data.frame(matrix(c(0,1,0,1,0,1,1,0,1,0),nrow=5,ncol=2,byrow=TRUE,
                dimnames=list(c(paste("I",c(1:5),sep="")),c("Q.1","Q.2"))))
  
subscales(x,y,check.reliability=T)

# the x is not scored, y the quality matrix, check.reliability is required, key1 is the key of response:
x:(data.frame is preferred)
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

y<-data.frame(matrix(c(0,1,0,1,0,1,1,0,1,0),nrow=5,ncol=2,byrow=TRUE,
                dimnames=list(c(paste("I",c(1:5),sep="")),c("Q.1","Q.2")))) 
key1<-c(D,B,D,C,C)

subscales(x,y,score.items=T,check.reliability=T,key=key1)

}
