`reliability` <-
function(items, itemal=F, NA.Delete=F){

if(!is.numeric(items)) { items <- apply(items,c(1,2),as.numeric)
          warning("Data is not numeric. Data has been coerced to be numeric.")}

if(NA.Delete==TRUE) {items <-ifelse(is.na(items),0,items)
                     warning("Missing values or NA values are converted to zeros.")} 
     

items <- na.omit(items)
s <- apply(items,2,var)
N <- ncol(items)

X <- rowSums(items)
alpha <- (N/(N-1))*(1 - sum(s)/var(X))

if(itemal){
  alphad <- array(dim=N)
  pbis <- array(dim=N)
  for(i in 1:N){
    Xd <- rowSums(items[,-i])
    pvalu <- colMeans(items)
    alphad[i] <- ((N-1)/(N-2))*(1 - sum(s[-i])/var(Xd))
    pbis[i] <- cor(items[,i],Xd)
    out <- list(N_item=N,N_person=nrow(items),alpha=alpha, scale.mean=mean(X), scale.sd=sd(X),
                alpha.if.deleted=alphad, pbis=pbis, P.value=pvalu)
  }
} 
else out <- list(N_item=N,N_person=nrow(items),alpha=alpha, scale.mean=mean(X), scale.sd=sd(X))
class(out) <- "reliability"
out
}

