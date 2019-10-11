library(psych)
myfa<-function(X,m=1)
   {fa(r=X,nfactors=m,fm='ml',rotate='promax')}

### データの読み込みと因子分析の実行
(SDdat <- read.csv('dat/ski.csv',header=T,row.names=1))
(SD_fa<-myfa(SDdat,3))

### 重み1の方法
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

colnames(F0) <-  c('評価','活動性','力量')
round(F0,2)

### 因子スコアの推定値の算出
## Harmanの方法
factor.scores(x=SDdat,f=SD_fa,method="Harman")

# 因子負荷をそのまま重みとする方法
factor.scores(x=SDdat,f=SD_fa,method="components")

## Thurstoneの方法
factor.scores(x=SDdat,f=SD_fa,method="Thurstone")

# スクリプトを自分で書く場合（上の結果と一致する）
Z<-scale(SDdat);R<-cor(SDdat);iR<-solve(R);A<-SD_fa$loadings
(Wt<-iR %*% A)
(F13 <- Z %*% Wt)

## Bartlettの方法
factor.scores(x=SDdat,f=SD_fa,method="Bartlett")


### 犯罪データの分析
(crime <- read.csv('dat/犯罪.csv',header=T,row.names=1))
(crimedat<-log(crime))
(crime_fa<-myfa(crimedat,2))
cor(crimedat)

## Bartlettの方法による因子スコアの推定値の算出
Z<-scale(crimedat);A<-crime_fa$loadings 
D2<-diag(crime_fa$uniquenesses); iD2<-solve(D2)
(Wb<-iD2%*%A%*%solve(t(A)%*%iD2%*%A))
(F3<-Z%*%Wb)

## 図の作成
plot(F3,type="n",ylim=c(-3.5,3.5),xlim=c(-3.5,3.5),xlab="対物犯罪",ylab="対人犯罪")
text(F3[,1],F3[,2],rownames(crimedat),cex=0.8)
