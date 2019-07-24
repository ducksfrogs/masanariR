library(psych);library(GPArotation)

myfa<-function(X,m=1)
   {fa(r=X,nfactors=m,fm='ml',rotate='promax')}

#データの読み込み
SDdat  <- read.csv('dat/ski.csv',header=T,row.names=1)
YGdat  <- read.csv('dat/YG3.csv',header=T,sep=',')
Holdat <- read.csv('dat/Hol.csv',header=T,sep=',')
BFIdat <- read.csv('dat/BFI.csv',header=T,sep=',')

#スクリーテスト
myscree<-function(r,b="固有値の順位")
   {plot(fa(r)$e.values,type='o',xlab=b,ylab="固有値")}
myscree(SDdat,b="スキー場データ")
myscree(Holdat,b="知能検査データ")
myscree(YGdat,b="YG性格検査データ")
myscree(BFIdat,b="性格評価データ")

#ガットマン基準
myeigen<-function(X)
   {cat("固有値(ガットマン基準)","\n")
    round(eigen(cor(X))$values,3)}
myeigen(SDdat)
myeigen(Holdat)
myeigen(YGdat)
myeigen(BFIdat)

#SMC基準
mySMC<-function(X,b="")
   {cat("固有値(SMCを利用した固有値)","\n")
    cor.X <- cor(X)
    diag(cor.X) <- smc(X)
    print(round(eigen(cor.X)$values,3))
    plot(eigen(cor.X)$values,type='o',xlab=b,ylab="固有値")}
mySMC(SDdat,b="スキー場データ")
mySMC(Holdat,b="知能検査データ")
mySMC(YGdat,b="YG性格検査データ")
mySMC(BFIdat,b="性格評価データ")

#平行分析
fa.parallel(SDdat ,fm="ml",fa="pc", n.iter=100,main="スキー場データ")
fa.parallel(Holdat,fm="ml",fa="pc", n.iter=100,main="知能検査データ")
fa.parallel(YGdat ,fm="ml",fa="pc", n.iter=100,main="YG性格検査データ")
fa.parallel(BFIdat,fm="ml",fa="pc", n.iter=100,main="性格評価データ")

#MAPテスト
VSS(SDdat,n=5)
VSS(Holdat,n=5)
VSS(YGdat,n=7)
VSS(BFIdat,n=18)

#VSS基準
VSS(SDdat, n = 5, rotate = "promax", fm = "ml",title="スキー場データ")
VSS(Holdat,n = 5, rotate = "promax", fm = "ml",title="知能検査データ")
VSS(YGdat, n = 7, rotate = "promax", fm = "ml",title="YG性格検査データ")
VSS(BFIdat,n = 15,rotate = "promax", fm = "ml",title="性格評価データ")

#情報量規準
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

#最大因子数を見つける関数
maxfacn <- function(p){floor( ((2*p+1)-sqrt(8*p+1))/2 )}
maxfacn(3:15)