
key1 <- t(read.table("key.txt"))
d1 <- read.fwf("test data.txt",rep(1,20))

s1 <- score(d1,key=key1, rel=T)
str(s1)

s1 <- score(d1,key=key1,ID=paste("Test",1:100,sep=""))
s1

s1 <- score(d1,key=key1)
s1

s1 <- score(d1,key=key1,ID=paste("Test",c(1:99,99),sep=""))
s1

s1 <- score(d1,key=key1, rel=T, output.scored=T)
str(s1)


r1 <- reliability(s1$scored)
str(r1)

dis1 <- distractor.analysis(d1, key=key1)
dis1

distractor.analysis(d1, key=key1,p.table=F,write.csv="test_new.csv")

mean(s1$score)
sd(s1$score)

str(score.transform(s1$score,50,10))

mean(score.transform(s1$score,50,10)$new.scores)
sd(score.transform(s1$score,50,10)$new.scores)

hist(score.transform(s1$score)$new.scores)
hist(score.transform(s1$score,normalize=T)$new.scores)

spearman.brown(.7)
spearman.brown(.7,3)
spearman.brown(.7,.875,"r")


q <- matrix(c(rep(1,10),rep(0,20),rep(1,10)),ncol=2)
q[18,1] <- 1
q

subscales(d1,q)
subscales(d1,q,scale.names=c("first","second"))
subscales(d1,q,scale.names=c("first","second"),score.scales=T,key=key1)
subscales(d1,q,scale.names=c("first","second"),score.scales=T,check.reliability=T,key=key1)
subscales(d1,q,scale.names=c("first","second"),F,T,key=key1)
subscales(d1,q,scale.names=c("first","second"),T,T)
subscales(data.frame(score(d1,key=key1, rel=T, output.scored=T)$scored),q,scale.names=c("first","second"),T,T)
subscales(d1,q,scale.names=c("first","second"),T,T,key=key1)$second$reliability



library(MASS)

r1 <- cor(s1$scored[,1:4])

disattenuated.cor(cor(s1$scored[,1:4]),c(.8,.8,.8,.9))
disattenuated.cor(cor(s1$scored[,1:4])[3,2],c(.8,.8))
disattenuated.cor(cor(s1$scored[,1:4])[4,1],c(.8,.9))

disattenuated.cor(cor(s1$scored[,1:4]),c(.8,.8,.8,.9),c(.9,.9,.9,.97))
disattenuated.cor(cor(s1$scored[,1:4])[4,1],c(.8,.9),c(.9,.9))
disattenuated.cor(cor(s1$scored[,1:4])[4,1],c(.8,.9),c(.9,.97))
disattenuated.cor(cor(s1$scored[,1:4])[4,1],c(.8,.9),c(1,1))

