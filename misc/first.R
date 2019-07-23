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
