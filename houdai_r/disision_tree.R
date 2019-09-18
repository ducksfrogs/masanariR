data <- read.csv("data/exam.csv", row.names = 1)
install.packages("rpart")
head(data)
library(rpart)
s2 <- rpart(score~age+sex+trial, data = data, method = 'class')
s2
par(xpd=NA)
s3 <- rpart(score~., data = data, method = 'class')
plot(s2)
text(s2, pretty = 0, all = T)
s2
summary(data)
table(data$score, data$trial)

data2 <- read.csv("data/exam0.csv", header = T, row.names = 1)
head(data2)
data2$score3 <- cut(data2$score2, breaks = c(-Inf, 60, Inf ), right = F, labels = c("Under60", "Over60"))
head(data2)
hist(data2$score2)
