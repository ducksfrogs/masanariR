data <- read.csv("data/test01utf8.csv", header = T, row.names = 1)
head(data)
table(data)
table1 <- table(data$study, data$result)
table1
table2 <- addmargins(table1)
prop.table(table1,1)
