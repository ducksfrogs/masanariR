library(psych)
myfa<-function(X,m=1,...)
 {fa(r=X,nfactors=m,fm='ml',rotate='promax',...)}

(���Ȏ���dat <- read.csv('dat/���Ȏ���.csv',header=T,row.names="���k"))
(r_t_fa<-myfa(���Ȏ���dat))
r_t_fa$scores

(SDdat <- read.csv('dat/ski.csv',header=T,row.names=1))
(SD_fa<-myfa(SDdat,3))

SD_sc <- SD_fa$scores
colnames(SD_sc) <-  c('�]��','������','�͗�')
round(SD_sc,2)

plot(SD_sc[,'�]��'],SD_sc[,'������'],type='n',xlab="�]��",ylab="������")
text(SD_sc[,'�]��'],SD_sc[,'������'],rownames(SD_sc),cex=0.8)

myscree<-function(r,b="�ŗL�l�̏���")
   {plot(fa(r)$e.values,type='o',xlab=b,ylab="�ŗL�l")}

myscree(���Ȏ���dat)
myscree(SDdat)

(YGdat <- read.csv('dat/YG3.csv',header=T,sep=','))
myscree(YGdat)
myfa(YGdat,1);myfa(YGdat,2);myfa(YGdat,3);myfa(YGdat,4)