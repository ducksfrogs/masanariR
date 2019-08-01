yamate1 <- read.csv("./data/yamateutf8.csv", header = T, row.names = 1)
yamate0 <- as.dist(yamate1)
yamate0
yamate2 <- cmdscale(yamate0)
plot(yamate2, type="n")
text(yamate2, rownames(yamate2))
yamate3 <- cmdscale(yamate0, k=3, eig = T)
yamate3
