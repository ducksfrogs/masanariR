library(psych)
myfa<-function(X,m=1,...)
 {fa(r=X,nfactors=m,fm='ml',rotate='promax',...)}

(理科試験dat <- read.csv('dat/理科試験.csv',header=T,row.names="生徒"))
(r_t_fa<-myfa(理科試験dat))
r_t_fa$scores

(SDdat <- read.csv('dat/ski.csv',header=T,row.names=1))
(SD_fa<-myfa(SDdat,3))

SD_sc <- SD_fa$scores
colnames(SD_sc) <-  c('評価','活動性','力量')
round(SD_sc,2)

plot(SD_sc[,'評価'],SD_sc[,'活動性'],type='n',xlab="評価",ylab="活動性")
text(SD_sc[,'評価'],SD_sc[,'活動性'],rownames(SD_sc),cex=0.8)

myscree<-function(r,b="固有値の順位")
   {plot(fa(r)$e.values,type='o',xlab=b,ylab="固有値")}

myscree(理科試験dat)
myscree(SDdat)

(YGdat <- read.csv('dat/YG3.csv',header=T,sep=','))
myscree(YGdat)
myfa(YGdat,1);myfa(YGdat,2);myfa(YGdat,3);myfa(YGdat,4)
