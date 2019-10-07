c1 <- read.table("data/pi0.dat",h=T,row.names = 1)
pie(c1$freq, labels = rownames(c1),clockwise = T)
barplot(c1$freq,names=rownames(c1))

f1 <- function(x) 1/(1+exp(-x))
plot(f1)
