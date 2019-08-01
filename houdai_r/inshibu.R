w1 <- read.table("./data/w2utf8.dat",header = T,row.names = 1)
head(data)
w2 <- cor(w1)
w2
plot(eigen(w2)$values) 
abline(h=1)
eig <- eigen(w2)
eig
