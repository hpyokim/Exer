
library(igraph) 
library(arules) 
library(arulesViz)
library(foreach) 

groc_raw = readLines("../data/groceries.txt")

head(groc_raw, 5)

# n = length(groc_raw)
# id = seq(1, n)

groc = strsplit(groc_raw, ",")

groc[[10]]

str(groc)

groc = lapply(groc, unique)
groctrans = as(groc, "transactions") 
summary(groctrans)

itemFrequencyPlot(groctrans, topN=20)


cart = apriori(groctrans, 
                     parameter=list(support=.01, confidence=.1, maxlen=10))

inspect(subset(cart, lift > 2 & confidence > 0.2))

plot(shoppingcart)
plot(shoppingcart, measure = c("support", "lift"), shading = "confidence")
plot(shoppingcart, method='two-key plot')


sub1 = subset(shoppingcart, subset=confidence > 0.1 & support > 0.01)
plot(head(sub1, 100, by='lift'), method='graph')

saveAsGraph(sub1, file = "shoppingcart.graphml")

