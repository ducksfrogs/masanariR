library(psych);library(GPArotation)

myfa<-function(X,m=1)
   {fa(r=X,nfactors=m,fm='ml',rotate='promax')}

#�f�[�^�̓ǂݍ���
SDdat  <- read.csv('dat/ski.csv',header=T,row.names=1)
YGdat  <- read.csv('dat/YG3.csv',header=T,sep=',')
Holdat <- read.csv('dat/Hol.csv',header=T,sep=',')
BFIdat <- read.csv('dat/BFI.csv',header=T,sep=',')

#�X�N���[�e�X�g
myscree<-function(r,b="�ŗL�l�̏���")
   {plot(fa(r)$e.values,type='o',xlab=b,ylab="�ŗL�l")}
myscree(SDdat,b="�X�L�[��f�[�^")
myscree(Holdat,b="�m�\�����f�[�^")
myscree(YGdat,b="YG���i�����f�[�^")
myscree(BFIdat,b="���i�]���f�[�^")

#�K�b�g�}���
myeigen<-function(X)
   {cat("�ŗL�l(�K�b�g�}���)","\n")
    round(eigen(cor(X))$values,3)}
myeigen(SDdat)
myeigen(Holdat)
myeigen(YGdat)
myeigen(BFIdat)

#SMC�
mySMC<-function(X,b="")
   {cat("�ŗL�l(SMC�𗘗p�����ŗL�l)","\n")
    cor.X <- cor(X)
    diag(cor.X) <- smc(X)
    print(round(eigen(cor.X)$values,3))
    plot(eigen(cor.X)$values,type='o',xlab=b,ylab="�ŗL�l")}
mySMC(SDdat,b="�X�L�[��f�[�^")
mySMC(Holdat,b="�m�\�����f�[�^")
mySMC(YGdat,b="YG���i�����f�[�^")
mySMC(BFIdat,b="���i�]���f�[�^")

#���s����
fa.parallel(SDdat ,fm="ml",fa="pc", n.iter=100,main="�X�L�[��f�[�^")
fa.parallel(Holdat,fm="ml",fa="pc", n.iter=100,main="�m�\�����f�[�^")
fa.parallel(YGdat ,fm="ml",fa="pc", n.iter=100,main="YG���i�����f�[�^")
fa.parallel(BFIdat,fm="ml",fa="pc", n.iter=100,main="���i�]���f�[�^")

#MAP�e�X�g
VSS(SDdat,n=5)
VSS(Holdat,n=5)
VSS(YGdat,n=7)
VSS(BFIdat,n=18)

#VSS�
VSS(SDdat, n = 5, rotate = "promax", fm = "ml",title="�X�L�[��f�[�^")
VSS(Holdat,n = 5, rotate = "promax", fm = "ml",title="�m�\�����f�[�^")
VSS(YGdat, n = 7, rotate = "promax", fm = "ml",title="YG���i�����f�[�^")
VSS(BFIdat,n = 15,rotate = "promax", fm = "ml",title="���i�]���f�[�^")

#���ʋK��
myIC <- function(x,M){
   IC_M <- matrix(0,2,M)
   for(k in 1:M){IC_M[1,k] <- myfa(x,m=k)$STATISTIC - 2*myfa(x,m=k)$dof}
   for(k in 1:M){IC_M[2,k] <- myfa(x,m=k)$BIC}
   rownames(IC_M) <- c("AIC","BIC")
   return(IC_M)}
myIC(x=SDdat ,M=5)
myIC(x=Holdat,M=5)
myIC(x=YGdat ,M=7)
myIC(x=BFIdat,M=18)

#RMSEA
myRMSEA <- function(x,M){
   RMSEA_M <- numeric(M)
   for(k in 1:M){RMSEA_M[k] <- myfa(x,m=k)$RMSEA[1]}
   return(RMSEA_M)}
myRMSEA(x=SDdat ,M=5)
myRMSEA(x=Holdat,M=5)
myRMSEA(x=YGdat ,M=7)
myRMSEA(x=BFIdat,M=18)

#�ő���q����������֐�
maxfacn <- function(p){floor( ((2*p+1)-sqrt(8*p+1))/2 )}
maxfacn(3:15)