summary(cars)
w1 <- read.table("data/w1.dat",header = T, row.names = 1)
w2 <- scale(w1)
w3 <- prcomp(w2)
biplot(w1)
head(w1)
length(w1)
str(w1)
length(w1$height)
var(w1$height)
plot(w1$height, w1$weight)
w2
