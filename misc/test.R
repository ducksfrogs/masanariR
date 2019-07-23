require(ggplot2)
head(economics)
cor(economics$pce, economics$psavert)
plot(economics$pce, economics$psavert)
cor(economics[,c(2,4:6)])
load(GGally)
install.packages("GGally")
require(GGally)
GGally::ggpairs(economics, economics[,c(2,4:6)] )
data(tips, package = "reshape2")
head(tips)
GGally::ggpairs(tips)
head(tips)
unique(tips$sex)
unique(tips$day)
t.test(tips$tip, alternative = "two.sided", mu=2.5)
require(ggplot2)

randT <- rt(3000, df=NROW(tips)-1)
tipTest <- t.test(tips$tip, alternative = "two.sided", mu=2.50)
ggplot(data.frame(x=randT))+
  geom_density(aes(x=x), fill='grey', color='grey')+
  geom_vline(xintercept = tipTest$statistic)+
  geom_vline(xintercept = mean(randT)+ c(-2,2), linetype=2)

aggregate(tip~sex, data = tips, var)
shapiro.test(tips$tip)
shapiro.test(tips$tip[tips$sex=='Female'])
shapiro.test(tips$tip[tips$sex=='Male'])

ggplot(tips, aes(x=tip, fill=sex)) +
  geom_histogram(binwidth = .5, alpha=1/2)
  
ansari.test(tip~sex, tips)

t.test(tip~sex,data=tips, var.equal=TRUE)

require(UsingR)
require(ggplot2)