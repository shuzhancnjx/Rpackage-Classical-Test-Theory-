##########################################################
reliability <- function(items, itemal=F, NA.Delete=T){

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

print.reliability<- function(x){   # define the characteristics of the output of the class "Reliability"
                   cat("\n Number of Items \n", unlist(x[1]),"\n")
                   cat("\n Number of Examinee \n",unlist(x[2]),"\n")
                   cat("\n Coefficient Alpha \n", round(unlist(x[3]),3),"\n")
                   invisible(x)}



###########################################################
score <- function(items,key=NA,output.scored=F,ID=NA,rel=F){ 
  
  t<- as.vector(ID)                                          
  t<- table(ID)  
  if(any(t>1)){ for(i in 1:length(ID)){
                   for(j in 1:length(subset(t,t>1))){
                   if(ID[i]==(names(subset(t,t>1)))[j])
                   {ID[i]<- paste(ID[i],"_",i,sep="")}}}
        warning("Duplicate ID exists; the duplicate ID has been renamed and retained in the calculation")
               }
  
   if(!missing(ID)){
     if(length(ID)==nrow(items)) rownames(items) <- ID
        else warning("The length of ID vector does not match the sample size.")}
               
   if(is.na(key)){
    warning("No key provided, assuming pre-scored data.")
    scored <- items
  } 
  else {
    if(length(key)==ncol(items)) scored <- t(apply(items,1,function(X){ifelse(X==(key),1,0)}))
    else stop("Number of items is not equal to the length of key.")
  }
 scores <- rowSums(scored)
 names(scores)<- rownames(items) 
 if(!rel==F)reli<-reliability(scored)
 if(output.scored==F&rel==F) out<-list(score=scores)
 if(output.scored==F&rel==T)out<-list(score=scores,reliability=reli) 
 if(output.scored==T&rel==F)out<-list(score=scores,scored=scored)
 if(output.scored==T&rel==T) out<- list(score=scores,reliability=reli, scored=scored)
 out
}

#######################################################################################
distractor.analysis <- function(items,key=NA,scores=NA,p.table=F,write.csv=NA){

items <- as.data.frame(items)
if(length(key)==1) key<-c(rep(key,ncol(items)))

if(missing(scores)) scores<- score(items,key)$score
if(missing(key)) warning("Answer key is not provided")
    else if(! length(key)==ncol(items)) {warning("Answer key is not provided or some item keys are missing.")}          

scores<- as.data.frame(scores)
 score.level <- quantile(scores[,1],c(0,1/3,2/3,1))
 score.level <- cut(scores[,1],score.level,include.lowest=T,labels=c("lower","middle","upper"))
 
 
 itemtab <- function(response) {xtabs(~ response + score.level)}
 itemtabp <- function(response) {round(prop.table(xtabs(~ response + score.level),2),3)}
 all.levels<- sort(unique(unlist(items)))
 for(i in 1:ncol(items)){
    items[,i]<-factor(items[,i],levels=all.levels,
                                   labels=ifelse(all.levels==key[i],paste("*",all.levels,sep=""),paste(" ",all.levels,sep="")))
}
  
   out<-list() 
   if(p.table==F) for(i in 1:ncol(items)){
     out[[i]]<-itemtab(items[,i])
    }
    else for(i in 1:ncol(items)){
      out[[i]]<-itemtabp(items[,i])
    }
 names(out) <- colnames(items)
 
if(! is.na(write.csv=="NA")){
   response<- ifelse(all.levels==key[i],paste("*",all.levels,sep=""),paste(" ",all.levels,sep=""))
   item<-c(rep(NA,length(all.levels)))
   
   for(i in 1:ncol(items)){
         x<-as.data.frame(cbind(item,response,as.vector(out[[i]][,1]),as.vector(out[[i]][,2]),as.vector(out[[i]][,3])))
         suppressWarnings(x <- rbind(x,c("","","","","")))
         names(x)<- c(paste("item_",i),"response","lower","middle","upper")
         
         suppressWarnings(write.csv(x,write.csv,row.names=F,na=" ",append=T))
   }
}
names(out)<-paste("item_",c(seq(1:ncol(items))),sep="")
out 
 }

########################################################################################
score.transform <- function(scores, mu.new=0, sd.new=1, normalize=FALSE){

percentile <- trunc(rank(scores))/length(scores)

if(normalize) scores.new <- qnorm(percentile) 
else{
  mu.old <- mean(scores)
  sd.old <- sd(scores)
  scores.new <- (scores - mu.old)/sd.old
}

scores.new <- (scores.new*sd.new) + mu.new
out <- list(new.scores = scores.new, p.scores = percentile)
out
}
#################################################################
spearman.brown <- function(r.xx, input=2, n.or.r = "n"){

if(n.or.r == "n"){
  r.new <- input*r.xx/(1+(input-1)*r.xx)
  out <- list(r.new=r.new)
}
if(n.or.r == "r"){
  n.new <- input*(1-r.xx)/(r.xx*(1-input))
  out <- list(n.new=n.new)
}
out
}

#################################################################################################
subscales <- function(items, scales, scale.names=NA, score.items=F, check.reliability=F, key=NA){
n.scales <- ncol(scales)

if(score.items) {
  save.names <- colnames(items)
  items <- as.data.frame(score(items,key,output.scored=T)$scored)
  colnames(items) <- save.names
}

sets <- apply(scales,2,function(XXX) items[,XXX==1])
suppressWarnings(if(! is.na(scale.names)) names(sets) <- scale.names  else names(sets)<-paste("Q.",c(seq(1:n.scales))))

if(check.reliability){
  for(i in 1:n.scales){
   sets[[i]] <- suppressWarnings(score(sets[[i]], output.scored=T,rel=T))
  }}
  else {  for(i in 1:n.scales){ sets[[i]] <- suppressWarnings(score(sets[[i]], output.scored=T)) }}
sets
}

######################################################################################################
disattenuated.cor<- function(r.xy, r.xx, new.r.xx=1){

   new.r <- function(rxy, rxx, nrxx) sqrt(prod(nrxx))*rxy/sqrt(prod(rxx))
   
   if(! is.matrix(r.xy)){
     new.r.xy <- new.r(r.xy, r.xx, new.r.xx)  
   } else{
     new.r.xy <- r.xy
     for(i in 2:ncol(r.xy)){
      for(j in 1:(i-1)){
       if(length(new.r.xx)==1) nr <- new.r.xx else nr <- new.r.xx[c(i,j)]
       new.r.xy[i,j] <- new.r(r.xy[i,j], r.xx[c(i,j)], nr) 
      }
     }
     diag(new.r.xy) <- r.xx
     }
   new.r.xy
}

package.skeleton(name="CTT",list=c("reliability","score","score.transform","subscales","disattenuated.cor","distractor.analysis","spearman.brown"))