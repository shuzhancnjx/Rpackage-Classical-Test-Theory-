\name{Score.transform}
\alias{score.transform}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for transform score into different scales }
\description{
  The function transforms the score metric by setting new scales' mean, standard deviation, and normalizing the distribution.
}
\usage{
score.transform(scores, mu.new = 0, sd.new = 1, normalize = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scores}{ Vector for examinee scores }
  \item{mu.new}{ Desired mean of the scale }
  \item{sd.new}{ Desired standard deviation of scales }
  \item{normalize}{ If normailize=True, the score will be normalized applying the inverse of the cumulative distribution function 
of the normal distribution to the respondents percentile score.}
\value{
  The function returns a list with two vectors: new.scores is the transformed score 
and p.scores is the percentile rank of every examinee. 
}
\references{ ~put references to the literature/web site here ~ }
\author{John Willse, Zhan Shu }
\examples{

# x is the response file, y is the key

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

scores<-score(x,y) # obtain the socres of examinee

# the targeted mean=3, standard deviation=1

score.transform(scores,3,1)

# the score should be transformed by normalized precentile
score.transform(scores,3,1,True)
}
