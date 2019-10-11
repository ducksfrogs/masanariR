## パッケージ，実習用関数，データの読み込み
library("polycor")
library("psych")
myfa<-function(X,m=1,...)
  {fa(r=X,nfactors=m,fm='ml',rotate='promax',...)}
myscree<-function(r,b="固有値の順位")
{plot(fa(r)$e.values,type='o',xlab=b,ylab="固有値")}
data.cont <- read.csv('dat/6科目学力(量的).csv', header=TRUE)
data.disc <- read.csv('dat/6科目学力(質的).csv', header=TRUE)


## テトラコリック相関係数
tetran <- polychor(x=data.disc$古典, y=data.disc$地学, std.err=TRUE, ML=TRUE)
tetran$rho; tetran$row.cuts; tetran$col.cuts; tetran$var


## ポリコリック相関係数
pola <- polychor(x=data.disc$古典, y=data.disc$現代文, std.err=TRUE, ML=TRUE)
pola$rho; pola$row.cuts; pola$col.cuts; pola$var

polychor(x=data.disc$古典, y=data.disc$物理, std.err=FALSE, ML=TRUE)
polychor(x=data.disc$地学, y=data.disc$現代文, std.err=FALSE, ML=TRUE)
polychor(x=data.disc$地学, y=data.disc$物理, std.err=FALSE, ML=TRUE)
polychor(x=data.disc$現代文, y=data.disc$物理, std.err=FALSE, ML=TRUE)


## バイシリアル相関係数
bicle <- polycor::polyserial(x=data.disc$英語, y=data.disc$古典, std.err=TRUE, ML=TRUE)
bicle$rho; bicle$cuts; bicle$var

polycor::polyserial(x=data.disc$数学, y=data.disc$古典, std.err=FALSE, ML=TRUE)
polycor::polyserial(x=data.disc$英語, y=data.disc$地学, std.err=FALSE, ML=TRUE)
polycor::polyserial(x=data.disc$数学, y=data.disc$地学, std.err=FALSE, ML=TRUE)


## ポリシリアル相関係数
polastar <- polycor::polyserial(x=data.disc$英語, y=data.disc$現代文, std.err=TRUE, ML=TRUE)
polastar$rho; polastar$cuts; polastar$var

polycor::polyserial(x=data.disc$数学, y=data.disc$現代文, std.err=FALSE, ML=TRUE)
polycor::polyserial(x=data.disc$英語, y=data.disc$物理, std.err=FALSE, ML=TRUE)
polycor::polyserial(x=data.disc$数学, y=data.disc$物理, std.err=FALSE, ML=TRUE)


## ピアソンの積率相関係数
cor(x=data.disc$英語, y=data.disc$数学, use="complete.obs")


## 質的相関係数を利用した相関行列の計算
data.disc2 <- data.disc
data.disc2$現代文 <- as.factor(data.disc$現代文)
data.disc2$古典 <- as.factor(data.disc$古典)
data.disc2$物理 <- as.factor(data.disc$物理)
data.disc2$地学 <- as.factor(data.disc$地学)

discor <- hetcor(data=data.disc2, std.err=TRUE, ML=TRUE, use="complete.obs")
(cormt.disc <- discor$correlations)


##「6科目データ(量的)」の相関行列
(cormt.cont <- cor(data.cont)) 


## 「6科目データ(質的)」から積率相関係数を用いて求めた不適切な相関行列
(cormt.bad <- cor(data.disc))


## 質的因子分析の実行
myscree(cormt.disc)
(result.disc <- myfa(X=cormt.disc, m=2, n.obs=1000))


## 質的尺度を含むデータに，通常の因子分析を行う
(result.bad <- myfa(X=data.disc, m=2))


## 連続尺度のみを含むデータに，通常の因子分析を行う
(result.cont <- myfa(X=data.cont, m=2))
