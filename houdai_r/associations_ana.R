install.packages("arules")
library(arules)
data(Groceries)
g0 <- Groceries

g0
head(g0)
apriori(g0)
itemFrequencyPlot(g0)
grule2 <- apriori(g0, parameter = list(confidence=0.5, support=0.01))
grule2
inspect(grule2)

grule3 <- sort(grule2, d=T, by="confidence")
inspect(grule3)
  