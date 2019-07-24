#1.4節で定義されたmyfa関数
library(psych)
library(psy)
 myfa<-function(X,m=1)
    {fa(r=X, nfactors=m, fm='ml', rotate='promax', scores=T)}

#データの読み込み(全37項目)
SRdat<-read.csv("dat/SR2.csv", header=T)

#omega係数の定義
omega.coef<-function(x,nfactors=1,fm="ml",...){
  1 - ( sum(fa(x,nfactors=nfactors,fm=fm,...)$uniquenesses*diag(var(x))) /
        sum(var(x)) )
}

#1因子解を算出する
myfa(SRdat)

#全37項目のα信頼性係数とω信頼性係数
cronbach(SRdat)
omega.coef(SRdat)

##スクリーテスト
myscree<-function(r,b="心理的ストレス反応データ")
　　{plot(fa(r)$e.values,type='o', xlab=b, ylab="固有値")}
myscree(SRdat)

#6つの下位尺度ごとに因子分析
A_fac<-SRdat[c(4,10,12,16,19,27)]
myfa(A_fac,1)
cronbach(A_fac)
omega.coef(A_fac)

B_fac<-SRdat[c(18,24,30,32,35)]
myfa(B_fac,1)
cronbach(B_fac)
omega.coef(B_fac)

C_fac<-SRdat[c(1,2,5,7,8,36,37)]
myfa(C_fac,1)
cronbach(C_fac)
omega.coef(C_fac)

D_fac<-SRdat[c(3,14,17,21,22)]
myfa(D_fac,1)
cronbach(D_fac)
omega.coef(D_fac)

E_fac<-SRdat[c(23,25,26,28)]
myfa(E_fac,1)
cronbach(E_fac)
omega.coef(E_fac)

F_fac<-SRdat[c(6,9,11,13,15,20,29,31,33,34)]
myfa(F_fac,1)
cronbach(F_fac)
omega.coef(F_fac)

#因子分析（1因子解から10因子解まで）
myfa(SRdat,2);myfa(SRdat,3);myfa(SRdat,4);myfa(SRdat,5);
myfa(SRdat,6);myfa(SRdat,7);myfa(SRdat,8);myfa(SRdat,9);myfa(SRdat,10)