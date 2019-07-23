#from みんなR
install.packages('ggplot2')
require(ggplot2)
data(diamonds)
head(diamonds)
hist(diamonds$carat, main = "Carat Histgram", xlab = "Carat")
plot(price~carat, data = diamonds)
boxplot(diamonds$carat)
summary(diamonds)


ggplot(data = diamonds) +geom_histogram(aes(x=carat))
ggplot(data = diamonds) +geom_density(aes(x=carat) )

g<- ggplot(diamonds, aes(x=carat, y=price))
g2<- ggplot(diamonds, aes(x=carat))
g+geom_point(aes(color=color))
g2+geom_histogram()+facet_wrap(~color)
ggplot(diamonds,aes(y=carat, x=cut)) + geom_boxplot()

ggplot(diamonds,aes(y=carat, x=cut)) + geom_violin()


ggplot(economics, aes(x=date, y = pop)) +geom_line()

require(lubridate)


theMatrix <- matrix(1:9, nrow = 3)
apply(theMatrix, 1, sum)
rowSums(theMatrix)
colSums(theMatrix)
apply(theMatrix, 2, sum)
theMatrix[2,1] <-NA
apply(theMatrix, 1, sum)
apply(theMatrix, 2, sum)
apply(theMatrix, 1, sum, na.rm=TRUE)
rowSums(theMatrix, na.rm=TRUE)
rowSums(theMatrix)
theList <- list(A =matrix(1:9,3), B = 1:5, C= matrix(1:4,2), D=2)
lapply(theList, sum)
sapply(theList, sum)
theNames <- c("Jared", "Deb", "Paul")
lapply(theNames, nchar)
firstList <- list(A=matrix(1:16, 4), B = matrix(1:16, 2), C=1:5)
secondList <- list(A= matrix(1:16, 4), B=matrix(1:16, 8), C=15:1)
mapply(identical, firstList, secondList)

#aggregate

aggregate(price~cut, diamonds, mean)
aggregate(price~cut+color, diamonds, mean)
aggregate(cbind(price, carat)~ cut, diamonds, mean)
aggregate(cbind(price, carat) ~ cut + color, diamonds, mean)
require(plyr)
data("baseball")
head(baseball)
baseball$sf[baseball$year < 1954] <-0
any(is.na(baseball$sf))
baseball$hbp[is.na(baseball$hbp)] <- 0
any(is.na(baseball$hbp))

obp <- function(data){
  c(OBP = with(data, sum(h+bb+hbp)/sum(ab+bb+hbp+sf)))
}

carrerOBP <- ddply(baseball, .variables = "id", .fun = obp)
carrerOBP <- carrerOBP[order(carrerOBP$OBP, decreasing = TRUE),]
head(carrerOBP, 10)
