\name{Distractor.analysis}
\alias{distractor.analysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for item distractor analysis }
\description{
  This function performs distractor analysis for each item.
}
\usage{
distractor.analysis(items, key = NA, scores = NA, p.table = F, write.csv = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{items}{ The unscored item response from a multiple-choice test }
  \item{key}{ The answer key for the items }
  \item{scores}{ An optional set of person scores associated with the item data. If scores are not provided (default) the scores are calculated using the item data and key.  }
  \item{p.table}{ If p.table=F  (the default) the function returns the counts of examinees who provide each answer.  
                  If p.table=T the function returns the proportion of examinees who provide each answer.  }
  \item{write.csv}{ If the optional file name is provided the function will save a .csv file with the results. }
}
\details{
 The scores are used to split respondents into terciles. The number (or proportion if p.table=T) of examinees in each tercile giving each response is reported. 
 The correct answer is indicated with an ¡°*¡±.
}
\value{
 If p.table=F counts of respondents in each tercile who chose each answer is returned as a list of tables. Each item is a separate element in the list. 
 If p.table=T the tables contain the proportion of respondents who chose each corresponding answer. }
\references{ Allen, M. J. & Yen, W. M. (1979). Introduction to Measurement Theory. Lon Grove, Illinois: Waveland Press, INC. }
\author{ John Willse, Zhan Shu}
\examples{

items<-data.frame(matrix(c( C,C,C,B,B,
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

key<-c(D,B,D,C,C)
distractor.analysis(items,key)

# For results written to a file and proportions
distractor.analysis(items,key,p.table=T,write.csv="distractor.csv") 

}
