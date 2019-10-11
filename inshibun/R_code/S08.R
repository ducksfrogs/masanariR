### データ読み込み
YGdat<-read.csv("dat/YG3.csv",header=TRUE)

### 単変量ヒストグラム
par(mfrow=c(4,3)) 
for(i in 1:ncol(YGdat)){
	hist(YGdat[,i],breaks=seq(0,20,by=1),ylim=c(0,150),xlab="得点",main=names(YGdat)[i],ylab="度数")
}

### 歪度・尖度の算出
library(moments)
round(skewness(YGdat),2)
round(kurtosis(YGdat),2)


### 歪度・尖度の検定
library(fBasics)
X<-YGdat                  
for(i in 1:ncol(X)){
	cat(paste(colnames(X)[i],"の検定結果"))
	print(dagoTest(X[,i]))
}

### 多変量外れ値
round(cor(YGdat),2)
round(solve(cor(YGdat)),2)
Z<-apply(YGdat,2,scale)
gi2<-diag(as.matrix(Z)%*%solve(cor(YGdat))%*%t(as.matrix(Z)))
par(mfrow=c(1,1)) 
hist(gi2,ylim=c(0,200),breaks=15,xlim=c(0,40),ylab="度数",xlab="",main="")
(out<-boxplot(gi2,horizontal=TRUE))
(1:length(gi2))[gi2==max(out$out)]

### 反イメージ相関行列
Antii<-function(R){
	S<-diag(sqrt(1/diag(solve(R))))
	Antiimat<-S%*%solve(R)%*%S
	return(Antiimat)
	}
round(Antii(cor(YGdat)),2)

### MSA
MSA<-function(R){
    S<-diag(sqrt(1/diag(solve(R))));Antiimat<-S%*%solve(R)%*%S
    diag(R)<-0; MSA_1<-t(as.vector(R))%*%as.vector(R)
    diag(Antiimat)<-0;MSA_2<-t(as.vector(Antiimat))%*%as.vector(Antiimat)
    MSA<-MSA_1/(MSA_1+MSA_2)
    return(MSA)
	}
round(MSA(cor(YGdat)),2)

### MSAj
MSAj<-function(R){
    S<-diag(sqrt(1/diag(solve(R))));Antiimat<-S%*%solve(R)%*%S
    diag(Antiimat)<-0;MSAj_2<-diag(t(Antiimat)%*%Antiimat)
    diag(R)<-0;MSAj_1<-diag(t(R)%*%R)
    MSAj<-MSAj_1/(MSAj_1+MSAj_2)
    return(MSAj)
    }
round(MSAj(cor(YGdat)),2)

### 信頼区間
library(psych)
uniquenessCI<-function(dat,nfac,digits=3){
	X<-dat;D_sig<-diag(sqrt(diag(var(X))));sig_inv<-1/(diag(var(X)))
	result<-fa(r=X,nfactors=nfac,rotate="varimax",residuals=TRUE,fm="ml")	
	Ahat<-matrix(result$loadings[1:(ncol(X)*nfac)],ncol(X),nfac,byrow=FALSE)
	Ahat_ast<-D_sig%*%Ahat
	D2hat<-diag(result$uniqueness);D2hat_ast<-D_sig%*%D2hat%*%D_sig
	Theta<-{(solve(D2hat_ast)-solve(D2hat_ast)%*%Ahat_ast%*%
		solve(t(Ahat_ast)%*%solve(D2hat_ast)%*%Ahat_ast)%*%t(Ahat_ast)%*%solve(D2hat_ast))}
	Lambda<-(2/(nrow(X)-1))*((sig_inv%*%t(sig_inv))*solve(Theta*Theta))
	+(2/(nrow(X)-1))*((result$uniqueness%*%t(result$uniqueness))*(cor(X)^2-2*D2hat))
	SEuniqueV<-sqrt(diag(Lambda))
	CI<-rbind(diag(D2hat)+1.96*SEuniqueV,diag(D2hat),diag(D2hat)-1.96*SEuniqueV)
	rownames(CI)<-c("上限値","推定値","下限値");colnames(CI)<-colnames(X)
	print(round(CI,digits))
}
uniquenessCI(dat=YGdat,nfac=3,digits=2)

### 残差相関行列
result<-fa(r=YGdat,nfactors=3,rotate="promax",residuals=TRUE,fm="ml")
round(result$residual,2)

### 推定に影響を与える観測対象と最も影響の大きな観測対象を除いた場合の推定値比較
X<-YGdat;nfac<-3
result1<-fa(r=X,nfactors=nfac,rotate="promax",residuals=TRUE,fm="ml")
Ahat<-matrix(result1$loadings[1:(ncol(X)*nfac)],ncol(X),nfac,byrow=FALSE)
Phihat<-result1$Phi
D2hat<-diag(result1$uniqueness)
Rhat<-Ahat%*%Phihat%*%t(Ahat)+D2hat
Z<-apply(YGdat,2,scale)
gi_2<-diag(Z%*%solve(Rhat)%*%t(Z))
hist(gi_2,breaks=15,ylim=c(0,200),xlim=c(0,40),ylab="度数",xlab="",main="")
BAfac<-function(dat,nfac,newdat=FALSE,digits=4){
	X<-dat;d<-digits
	result1<-fa(r=X,nfactors=nfac,rotate="promax", residuals=TRUE, fm="ml")
	Ahat1<-matrix(result1$loadings[1:(ncol(dat)*nfac)],ncol(dat),nfac,byrow=FALSE)
	Phihat1<-result1$Phi
	D2hat1<-diag(result1$uniqueness)
	Rhat1<-Ahat1%*%Phihat1%*%t(Ahat1)+D2hat1
	Z<-apply(X,2,scale); ddi_2<-diag(Z%*%solve(Rhat1)%*%t(Z))
	max.ddi_2<-(1:length(ddi_2))[ddi_2==max(ddi_2)]
	X2<-X[-max.ddi_2,]
	result2<-fa(r=X2,nfactors=nfac,rotate="promax", residuals=TRUE, fm="ml")
	Ahat2<-matrix(result2$loadings[1:(ncol(dat)*nfac)],ncol(dat),nfac,byrow=FALSE)
	Phihat2<-result2$Phi
	Ahat<-list(Before=round(Ahat1,d),After=round(Ahat2,d))
	Phihat<-list(Before=round(Phihat1,d),After=round(Phihat2,d))
	if(newdat==TRUE){
		BA<-list(Ahat=Ahat,Phihat=Phihat,max.ddi_2,X2=X2)
	}else{
		BA<-list(Ahat=Ahat,Phihat=Phihat,max.ddi_2)
	}
	return(BA)
}
BAfac(dat=YGdat,nfac=3,newdat=FALSE,digits=2)

### SMCと共通性
round(smc(YGdat),2)
result<-fa(r=YGdat,nfactors=3,rotate="promax",residuals=TRUE,fm="ml")
round(result$communality,2)

### 因子数の変化による共通性の変化
comchan<-function(dat,max_nf){
	plot(smc(dat),pch=19,ylim=c(0,1),type="b",lty=1,xlab="変数",ylab="独自性",axes=FALSE)
	cat("SMC\n");print(round(smc(dat),2))
	axis(1,at=1:ncol(dat),labels=1:ncol(dat),cex.axis=1.3)
	axis(2,at=seq(0,1,by=0.1),labels=seq(0,1,by=0.1),cex.axis=1.1)
	for(i in 1:max_nf){
		par(new=TRUE)
		result<-fa(r=dat,nfactors=i,rotate="promax",residuals=TRUE,fm="ml")
		cat(paste("因子数=",i,"\n",sep=""));print(round(result$communality,2))
		plot(result$communality,ylim=c(0,1),pch=i+1,type="c",lty=(i+1),xlab="",ylab="",axes=FALSE)
		text(1:ncol(dat),result$communality,labels=i)
	}
}
comchan(YGdat,max_nf=5)










