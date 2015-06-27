\name{Reliability}
\alias{reliability}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for item reliability analysis  }
\description{
 This function performs reliability analysis, including coefficient alpha,and item statistics.
}
\usage{
reliability(items, itemal = F, NA.Delete = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{items}{ The scored response file with "0" and "1" }
  \item{itemal}{ If itemal=F (the default) no item analyses are conducted. 
   If itemal=T, the function will provide item means, item total correlations, and alpha is item removed.}
  \item{NA.Delete}{If NA.Delete=T (the default) records and deleted listwise if there are missing responses. 
                   If NA.Delete=F  all NA values are changed to 0s. }
}
\details{
The input files must be scored files with "0" and "1" or numeric scales (e.g., Likert Type scales). 
Only basic scale information is returned to the screen. Use str() to view additional statistics that are available. }
\value{
  \item{number of items}{The number of items}
  \item{number of person}{The sample size used in calculating the values}
  \item{alpha}{Crobach's alpha}
  \item{scale.mean}{average total socre}
  \item{scale.sd}{Standard deviation of total sum score}
  \item{alpha.if.deleted}{Cronbach's alpha if the corresponding item were deleted}
  \item{pbis}{The item total correlation, with the item's contribution removed from the total}
  \item{item.mean}{Average of each item}
}
\references{ Cronbach, L. J. (1951). Coefficient alpha and the internal structure of tests. Psychometika, 16, 297-334. }
\author{ John Willse, Zhan Shu }
\seealso{ score }
\examples{

# if the input file is the scored response file
x: (data frame is preferred)
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
reliability(x, itemal=T)

# if the input file is unscored file:
x<-data.frame(matrix(c( "C","C","C","B","B",
 "C","C","D","C","B",
 "C","C","C","B","B",
 "D","C","C","B","B",
 "D","C","D","C","B",
 "C","C","C","B","C",
 "C","B","C","B","B",
 "C","B","D","B","C",
 "C","C","C","C","C",
 "C","B","D","B","B"),nrow=10,ncol=5,byrow=TRUE,
       dimnames=list(c(paste("P",c(1:10),sep="")),c(paste("I",c(1:5),sep="")))))
key1<-c("D","B","D","C","C")

scored1<-score(x,key1,outputed.scored=T)$scored
reliability(scored1)

To see more detailed item statistics:
str(reliability(scored1,itemal=T))
}

