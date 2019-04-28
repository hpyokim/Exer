library(ggplot2)
library(LICORS)
library(foreach)
library(mosaic)
library(factoextra)
library(cluster)

# data
wine = read.csv("../data/wine.csv")

ncol(wine)
nrow(wine)

## split data into x vars and y

X = wine[,-(12:13)]
X = scale(X, center =TRUE, scale = TRUE) ### center and scale the data

wine_RW = wine[,13]
wine_LV = wine[,12]

# PCA

## princial components
pcX = prcomp(X)

summary(pcX)
biplot(pcX, xlabs=rep("¡¤", nrow(pcX)))

fviz_pca_biplot(pcX, geom.ind = "point")
ggbiplot(pcX)

pcX$rotation[,1:2]
scores = pcX$x

loadings[,1:4]

## how many components? by screenplot(Kaiser-Harris) and culmulitive variance
par(mfrow=c(1,2))
screeplot(pcX, type = "l", npcs = 6, main = "Screeplot")
cumpro <- cumsum(pcX$sdev^2 / sum(pcX$sdev^2))
plot(cumpro[0:10], ylab = "variance", main = "Cumulative variance")




#### PCA to cluster

muPC = attr(pcx,"scaled:center")
sigmaPC = attr(pcx,"scaled:scale")

clstPC2 = kmeanspp(pcx, k=2, nstart=25)
clstPC7 = kmeanspp(pcx, k=7, nstart=25)

table(clstPC2$cluster, wine$color)
table(clstPC7$cluster, wine$quality)


fviz_cluster(clstPC2, data = pcx, 
             ellipse.type = "convex", palette="jco", ggtheme = theme_minimal())

fviz_cluster(clstPC7, data = pcx, 
             ellipse.type = "convex", palette="jco", ggtheme = theme_minimal())

qplot(scores[,1], scores[,4], facets = ~ wine_RW, 
      xlab='PC 1', ylab='PC 2',
      xlim=c(-5,5), ylim=c(-5,5))

qplot(scores[,1], scores[,2], facets = ~ wine_LV, 
      xlab='PC 1', ylab='PC 2',
      xlim=c(-5,5), ylim=c(-5,5))



## plotting
qplot(scores[,1], scores[,2], color = wine_RW, xlab='PC1', ylab='PC2',
      xlim=c(-7.5,7.5), ylim=c(-7.5,7.5))

qplot(scores[,1], scores[,2], facets = ~ wine_RW, xlab='PC1', ylab='PC2',
      xlim=c(-7.5,7.5), ylim=c(-7.5,7.5))

summary(factor(wine_LV))

qplot(scores[,1], scores[,2], color = wine_LV, xlab='PC1', ylab='PC2',
      xlim=c(-7.5,7.5), ylim=c(-7.5,7.5))
qplot(scores[,1], scores[,2], facets = ~ wine_LV, xlab='PC1', ylab='PC2',
      xlim=c(-7.5,7.5), ylim=c(-7.5,7.5))
qplot(scores[,3], scores[,4], facets = ~ wine_LV, xlab='PC3', ylab='PC4',
      xlim=c(-7.5,7.5), ylim=c(-7.5,7.5))

## check
#length(which(scores[,1] > 0)) 

#r = subset(scores[,1], wine_RW=="red")
#w = subset(scores[,1], wine_RW=="white")
#qdata(r,p=0.95)
#qdata(w,p=0.05)

RW = ifelse(scores[,1] < -1, "red", "white")
table(RW, wine_RW)

RW = ifelse((scores[,2] - 2*scores[,1]) > 2, "red", "white")
table(RW, wine_RW)

round((1550+4824)/(1550+4824+74+49)*100)

#pcRW = glm(wine_RW ~ scores, family = binomial)

#pcLV = lm(wine_LV ~ scores)
#plot(fitted(pcLV), wine_LV)


# clustering

## K means clustering



### choose factors from scaled data
mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")

### white or red
summary(factor(wine$color))

clstRW = kmeans(X, 2, nstart = 25)

# clusterin key
clstRW$center[1,]*sigma + mu
clstRW$center[2,]*sigma + mu

# number of each cluster
length(which(clstRW$cluster == 1))
length(which(clstRW$cluster == 2))

# matching prob
length(which(wine[which(clstRW$cluster == 1),(13)] == "red")) / 
  length(which(clstRW$cluster == 1))
length(which(wine[which(clstRW$cluster == 2),(13)] == "white"))/ 
  length(which(clstRW$cluster == 2))

### wine level
# how many level
summary(factor(wine$quality))

clstLV = kmeans(X, 7, nstart = 25)

# match?
summary(factor(clstLV$cluster))
summary(factor(wine[which(clstLV$cluster == 1),(12)]))
summary(factor(wine[which(clstLV$cluster == 2),(12)]))
summary(factor(wine[which(clstLV$cluster == 3),(12)]))
summary(factor(wine[which(clstLV$cluster == 4),(12)]))
summary(factor(wine[which(clstLV$cluster == 5),(12)]))
summary(factor(wine[which(clstLV$cluster == 6),(12)]))

# matching prob?
length(which(wine[which(clstLV$cluster == 1),(12)] == "6")) / 
  length(which(clstLV$cluster == 1))
length(which(wine[which(clstLV$cluster == 2),(12)] == "6")) / 
  length(which(clstLV$cluster == 2))
length(which(wine[which(clstLV$cluster == 3),(12)] == "5")) / 
  length(which(clstLV$cluster == 3))

### clustering can not match the level of wine, because most of wines have levels between 5 and 7  

clstLV$center[1,]*sigma + mu
clstLV$center[2,]*sigma + mu


## K means++ clustering

### red or white?
clstRW2 = kmeanspp(X, k=2, nstart=25)

# matching prob
length(which(wine[which(clstRW2$cluster == 1),(13)] == "red")) / 
  length(which(clstRW2$cluster == 1))
length(which(wine[which(clstRW2$cluster == 2),(13)] == "white"))/ 
  length(which(clstRW2$cluster == 2))


### wine level
clstLV2 = kmeanspp(X, k=7, nstart=25)

# match?
summary(factor(clstLV2$cluster))
summary(factor(wine[which(clstLV2$cluster == 1),(12)]))
summary(factor(wine[which(clstLV2$cluster == 2),(12)]))
summary(factor(wine[which(clstLV2$cluster == 3),(12)]))
summary(factor(wine[which(clstLV2$cluster == 4),(12)]))
summary(factor(wine[which(clstLV2$cluster == 5),(12)]))
summary(factor(wine[which(clstLV2$cluster == 6),(12)]))

## kmeans vs kmeans++
clstRW$withinss
clstRW2$withinss
sum(clstRW$withinss)
sum(clstRW2$withinss)

clstRW$tot.withinss
clstRW2$tot.withinss
clstRW$betweenss
clstRW2$betweenss

clstLV$tot.withinss
clstLV2$tot.withinss
clstLV$betweenss
clstLV2$betweenss

clst1 = kmeanspp(X, k=3, nstart = 25)
clst1$tot.withinss
clst1$betweenss

## table
table(clstRW2$cluster, wine$color)
table(clstLV2$cluster, wine$quality)

## plotting 
fviz_cluster(clstRW2, data = X, 
             ellipse.type = "convex", palette="jco", ggtheme = theme_minimal())

fviz_cluster(clstLV2, data = X, 
             ellipse.type = "convex", palette="jco", ggtheme = theme_minimal())

### plotting some features

qplot(fixed.acidity, volatile.acidity, data=wine, color=factor(clstRW2$cluster))
qplot(fixed.acidity, citric.acid, data=wine, color=factor(clstRW2$cluster))

qplot(fixed.acidity, volatile.acidity, data=wine, color=factor(clstLV2$cluster))
qplot(fixed.acidity, citric.acid, data=wine, color=factor(clstLV2$cluster))


## which K is best for data set? -> just exercise 

# elbow plot
k_grid = seq(2, 20, by=1)
SSE_grid = foreach(k = k_grid, .combine='c') %do% {
  cluster_k = kmeans(X, k, iter.max = 20, nstart=25)
  cluster_k$tot.withinss
}
plot(k_grid, SSE_grid)

# CH index
N = nrow(X)
CH_grid = foreach(k = k_grid, .combine='c') %do% {
  cluster_k = kmeans(X, k, iter.max = 20, nstart=25)
  W = cluster_k$tot.withinss
  B = cluster_k$betweenss
  CH = (B/W)*((N-k)/(k-1))
  CH
}
plot(k_grid, CH_grid)

# gat_stat -> the result was weird. why K=5? the difference between fviz_nbclust and clusGap
cars_gap = clusGap(X, FUN = kmeans, nstart = 25, K.max = 10, B = 100)
plot(cars_gap) # K =5 ??

## fviz_nbclust(X, kmeans, method="gap_stat")  # K=3 ??
