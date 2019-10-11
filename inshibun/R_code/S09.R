library(lavaan)
###「スキー場データ」
SDdat <- read.csv('dat/ski.csv',header=T,row.names=1)
colnames(SDdat) <-  c('x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9')
SD.cfa <- ' f1 =~ x1 + x2 + x3
            f2 =~ x4 + x5 + x6
            f3 =~ x7 + x8 + x9 '
##モデル1
SD.fit_1 <- cfa(SD.cfa, data = SDdat, orthogonal=TRUE)
summary(SD.fit_1, fit.measures=TRUE, rsquare=TRUE)
##モデル2
SD.fit_2 <- cfa(SD.cfa, data = SDdat)
summary(SD.fit_2, fit.measures=TRUE, rsquare=TRUE)

#GFI,AGFI,AIC,BICを計算するための関数GF
GF <- function(fit){
   S <- fit@Sample@cov[[1]]
   Shat <- fit@Fit@Sigma.hat[[1]]
   p <- ncol(S)
   df <- fit@Fit@test[[1]]$df[[1]]
   GFI <- round(1-(sum(diag((solve(Shat)%*%(S-Shat))^2))/
                     sum(diag((solve(Shat)%*%S)^2))), 3)
   AGFI <- round(1-((p*(p+1)/(2*df))*(1-GFI)), 3)
   chi <- fit@Fit@test[[1]]$stat
   n <- fit@Sample@nobs[[1]]
   AIC <- round(chi - 2*df, 3)
   BIC <- round(chi - log(n)*df, 3)
return(cbind(GFI, AGFI, AIC, BIC))
}
GF(SD.fit_1); GF(SD.fit_2)
(CFI.SD1 <- round(1-(32.476-27)/(280.847-36), 3))
(RMSEA.SD1 <- round(sqrt(32.476/(27*24)-1/24), 3))
(CFI.SD2 <- round(1-(26.414-24)/(280.847-36), 3))
(RMSEA.SD2 <- round(sqrt(26.414/(24*24)-1/24), 3))

#因子間共分散に関する信頼区間(モデル2)
1.086+1.96*0.690; 1.086-1.96*0.690
-0.599+1.96*0.696; -0.599-1.96*0.696
-1.079+1.96*0.656; -1.079-1.96*0.656

#修正指標
subset(modindices(SD.fit_1), mi>3)

##モデル3
SD.cfa_mod  <- ' f1 =~ x1 + x2 + x3
                 f2 =~ x4 + x5 + x6 + x7 + x9
                 f3 =~ x7 + x8 + x9'
SD.fit_3 <- cfa(SD.cfa_mod, data = SDdat, orthogonal=TRUE)
summary(SD.fit_3, fit.measures=TRUE, rsquare=TRUE)
GF(SD.fit_3)


###「性格評価データ」
BFIdat <- read.csv('dat/BFI.csv',header=T)
Big5.cfa <- ' Agr =~A1 + A2 + A3 + A4 + A5
		        Con =~ C1 + C2 + C3 + C4 + C5
		        Ext =~ E1 + E2 + E3 + E4 + E5
		        Neu =~ N1 + N2 + N3 + N4 + N5
		        Ope =~ O1 + O2 + O3 + O4 + O5 '
Big5.fit_1 <- cfa(Big5.cfa, data=BFIdat, orthogonal=TRUE)
summary(Big5.fit_1, fit.measures=TRUE, rsquare=TRUE)
Big5.fit_2 <- cfa(Big5.cfa, data=BFIdat)
summary(Big5.fit_2, fit.measures=TRUE, rsquare=TRUE)
GF(Big5.fit_1); GF(Big5.fit_2)

residuals(Big5.fit_2, type="normalized")  #残差行列

BFIdat_mod <- BFIdat[,-c(1, 19, 20, 22, 24, 25)]
Big5.cfa_mod <- ' Agr =~ A2 + A3 + A4 + A5
                  Con =~ C1 + C2 + C3 + C4 + C5
                  Ext =~ E1 + E2 + E3 + E4 + E5
                  Neu =~ N1 + N2 + N3
                  Ope =~ O1 + O3 '
Big5.fit_3 <- cfa(Big5.cfa_mod, data=BFIdat_mod)
summary(Big5.fit_3, fit.measures=TRUE, rsquare=TRUE)
GF(Big5.fit_3)
