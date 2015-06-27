`score` <-
function(items,key=NA,output.scored=F,ID=NA,rel=F){ 
  
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

