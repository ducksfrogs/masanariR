require(usingR)
require(ggplot2)
head(father.son)

ggplot(father.son, aes(x=fheight, y=sheight)) + geom_point()+
  geom_smooth(method = "lm")+ labs(x="Fathers", y="sons")


heightsLM <- lm(sheight~fheight, data = father.son)

heightsLM

summary(heightsLM)











































acs <- read.csv("../data/acs_ny.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
head(acs)
require(ggplot2)
require(useful)
acs$income <- with(acs, FamilyIncome >=150000)
head(acs)
ggplot(acs, aes(x=FamilyIncome)) +
  geom_density(fill="grey", color="grey") +
  geom_vline(xintercept = 15000) +
  scale_x_continuous(labels = multiple.dollar, limits = c(0,1000000))

income1 <- glm(income~HouseCosts + NumWorkers + OwnRent + NumBedrooms
               + FamilyType,
               data = acs, family = binomial(link="logit"))

summary(income1)
require(coefplot)
coefplot(income1)