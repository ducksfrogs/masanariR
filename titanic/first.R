train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")
alldata <- bind_rows(train, test)

glimpse(alldata)
summary(alldata)

train$Pclass <- as.factor(train$Pclass)
train$Pclass
train$Sex <- as.factor(train$Sex)
train$Survived <- factor(train$Survived, levels = c(0,1), labels = c("Died", "Survived"))

SP <- table(train$Survived, train$Pclass)
SP
