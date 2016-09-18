library(igraph)
library(ggplot2)

g <- read.graph('/Users/rutuja/Documents/Graphs and Network Flows/Project1/facebook_combined.txt', format="ncol", directed=FALSE)

is.connected(g)       #Connectivity of Graph

# Diameter
d = diameter(g)

#Degree Distribution
degreeDist = degree.distribution(g)
plot(degreeDist, type= "h", main="Degree Distribution", xlab="degree", ylab="density")
avg_degree = mean(degree(g))
# Average Degree
avg_degree


h = hist(degree(g), breaks=seq(0, by=1 , length.out=max(degree(g))+2))
df1 = data.frame(x=h$mids, y=h$density)
model1 = nls(y ~ I(1/x*a) + b*x, data = df1, start = list(a = 0, b = 0))
model2 = nls(y ~ I(exp(1)^(a + b * x)), data=df1, start = list(a=0,b=0))
model3 = nls(y ~ I(1/x*a) + b, data=df1, start = list(a=0,b=0))
ggplot(df1, aes(x, y)) + geom_point(size = 1)+
  stat_smooth(method = "nls", method.args = list(formula = y ~ I(1/x*a) + b*x, start = list(a=0,b=0)), size = 1, se = FALSE, colour = "magenta") +
  stat_smooth(method = "nls", method.args = list(formula = y ~ I(exp(1)^(a + b * x)), start = list(a=0,b=0)), size = 1, se = FALSE, colour = "blue") +
  stat_smooth(method = "nls", method.args = list(formula = y ~ I(1/x*a) + b, start = list(a=0,b=0)), size = 1, se = FALSE, colour = "green")
summary(model2)

#Since model2 is better with a=-3.5940045 b =-0.0291488
df2 = data.frame(x=h$mids, y=exp(1)^(-3.5940045-0.0291488*h$mids))
mse = sum((df2$y-df1$y)^2)/max(degree(g))
mse

