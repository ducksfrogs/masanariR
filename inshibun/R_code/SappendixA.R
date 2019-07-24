#ライブラリーとデータの読み込み
library(psych)
(理科試験dat <- read.csv('dat/理科試験.csv',header=T,row.names="生徒"))

# ヒストグラム
par(mfrow = c(2, 2))
rika <- c("物理","化学","生物","地学")
for(i in 1:4){
hist(理科試験dat[,i],main=rika[i],breaks=seq(0,100,10),xlab="得点",ylab="度数",xlim=c(0,100),ylim=c(0,5))
}
dev.off()

# 散布図
plot(理科試験dat,xlim=c(20,100),ylim=c(20,100))

# 算術平均
mean(理科試験dat)

# 中央値
apply(理科試験dat,2,median)

# 標本分散(「理科試験データ」の「物理」の場合)
sum((理科試験dat[1]-colMeans(理科試験dat)[1])^2)/10
# 不偏分散については関数varを用いて，var(理科試験dat[1])によって求められる

# 多変量データの標本分散共分散を算出する
bvar <- function(x){(var(x)*(nrow(x)-1))/nrow(x)}   
bvar(理科試験dat)

# 標準偏差
sqrt(diag(bvar(理科試験dat)))

# データの標準化
standscale <- function(x){t((t(x)-colMeans(x))/sqrt(diag(bvar(x))))}
standscale(理科試験dat)

# 相関係数
cor(理科試験dat)

# 下位集団の相関係数
cor(理科試験dat[c(6,7,8,9,10),])