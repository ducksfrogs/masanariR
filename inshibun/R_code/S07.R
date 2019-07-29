library(psych)
myfa<-function(X,m=1)
   {fa(r=X,nfactors=m,fm='ml',rotate='promax')}

### �f�[�^�̓ǂݍ��݂ƈ��q���͂̎��s
(SDdat <- read.csv('dat/ski.csv',header=T,row.names=1))
(SD_fa<-myfa(SDdat,3))

### �d��1�̕��@
Z<-scale(SDdat)
Wu<-matrix(c(
1,0,0,
1,0,0,
1,0,0,
0,1,0,
0,1,0,
0,1,0,
0,0,1,
0,0,1,
0,0,-1),9,3,byrow=T)
(F0<-Z%*%Wu)

colnames(F0) <-  c('�]��','������','�͗�')
round(F0,2)

### ���q�X�R�A�̐���l�̎Z�o
## Harman�̕��@
factor.scores(x=SDdat,f=SD_fa,method="Harman")

# ���q���ׂ����̂܂܏d�݂Ƃ�����@
factor.scores(x=SDdat,f=SD_fa,method="components")

## Thurstone�̕��@
factor.scores(x=SDdat,f=SD_fa,method="Thurstone")

# �X�N���v�g�������ŏ����ꍇ�i��̌��ʂƈ�v����j
Z<-scale(SDdat);R<-cor(SDdat);iR<-solve(R);A<-SD_fa$loadings
(Wt<-iR %*% A)
(F13 <- Z %*% Wt)

## Bartlett�̕��@
factor.scores(x=SDdat,f=SD_fa,method="Bartlett")


### �ƍ߃f�[�^�̕���
(crime <- read.csv('dat/�ƍ�.csv',header=T,row.names=1))
(crimedat<-log(crime))
(crime_fa<-myfa(crimedat,2))
cor(crimedat)

## Bartlett�̕��@�ɂ����q�X�R�A�̐���l�̎Z�o
Z<-scale(crimedat);A<-crime_fa$loadings 
D2<-diag(crime_fa$uniquenesses); iD2<-solve(D2)
(Wb<-iD2%*%A%*%solve(t(A)%*%iD2%*%A))
(F3<-Z%*%Wb)

## �}�̍쐬
plot(F3,type="n",ylim=c(-3.5,3.5),xlim=c(-3.5,3.5),xlab="�Ε��ƍ�",ylab="�ΐl�ƍ�")
text(F3[,1],F3[,2],rownames(crimedat),cex=0.8)