A <- read.csv("R_code/dat/rika-no-name.csv",header = t, row.names = 1)
A <- read.csv("R_code/dat/rika-no-name.csv",header = t, row.names = 1)
A <- read.csv("R_code/dat/rika-name.csv", header = T, row.names = 1)
library(psych)
myfa <-function(X,m=1,...){
fa(r=X,nfactors = m, fm='ml', rotate = 'promax', ... )
}
r_t_fa<- myfa(A)
r_t_fa$scores
View(r_t_fa)
Ski <- read.csv("R_code/dat/Ski_utf8.csv", header = T, row.names = 1)
summary(Ski)
SD_fa <- myfa(Ski, 3)
View(SD_fa)
View(SD_fa)
View(SD_fa)
SD_fa$loadings
SD_fa$factors
SD_fa$Phi
cor(Ski)
SD_sc <- SD_fa$scores
colnames(SD_sc)<- c('hyouka', 'katudo', 'rikiryo')
rount(SD_sc,2)
round(SD_sc,2)
plot(SD_sc[,'hyouka'], SD_sc[,'katsudo'],type='n',
xlab = "hyouka", ylab = 'katudo')
plot(SD_sc[,'hyouka'], SD_sc[,'katudo'],type='n',
xlab = "hyouka", ylab = 'katudo')
text(SD_sc[,'hyouka'],SD_sc[,'katudo'],rownames(SD_sc), cex = 0.8)
myscree<-function(r,b='eigen'){
plot(fa(r)$e.values, type = 'o',xlab = b, ylab = "eigenV")
}
myscree(A)
myscree(Ski)
ave(c(2,2,3
))
ave(2,3)
