library(ggplot2)
library(tidyverse)
library(cluster)
library(LICORS)
library(factoextra)
library(foreach)



# data
Tweet = read.csv("../data/social_marketing.csv", header = TRUE, row.names=1)

# delete chatter, spam, and adult
length(which(Tweet$spam>0))
length(which(Tweet$adult>0))

tweet = Tweet[,-c(1,5,35,36)]
length(which(rowSums(tweet)==0)) # no omitted data

tweet_R = tweet / rowSums(tweet) # get the proportions of keyword within one's tweet

#Z = apply(tweet_R, 2, scale)

Z = scale(tweet_R, center=TRUE, scale=TRUE) # normailize

head(Z[1:5,1:5])

# assoiation : impossible to weight the number of work
### select some keywords that are frequently tweeted per person
### criteria : the proportion of that keyword in his tweet 
###           + the distribution of keywords' proportion
###            (eg. who tends to tweet 'business' a lot?) => use z-stat

tweet_P = ifelse(Z>0.95, 1, 0) # check the only frequent tweets

summary(rowSums(tweet_P))
which(rowSums(tweet_P)==0)
Z[3666,]

zz = if(Z>0.95){
  print(colnames(Z))
}

colnames(Z)[3]
       
split(tweet_P, rownames(tweet_P))

which(tweet_P==0)

list(ifelse(Z>0.95, colnames(Z)), "")

i_grid = seq(from = 1, to = 32, by = 1)
tweet_c = foreach(i = i_grid, .combine = 'c') %do% {
  ifelse(Z[,i]>0.95, colnames(Z)[i], "")
}


ZN = if(Z>0.95)colnames(Z)

colnames(Z)[5]
str(Z)
tweet_P[1,]

str(tweet_P[3666,])

str(tweet_P)


tweet_I = factor(data.matrix(tweet_P)%*%
                   1:ncol(tweet_P), labels = names)

which(Z[,7]>2)
Z[7,]

ggplot(tweet_R, aes(trt, outcome)) +
  geom_col()


### not working
# PCA
pc = prcomp(Z, rank=10)
summary(pc)
loadings = pc$rotation
scores = pc$x

qplot(scores[,1], scores[,2], xlab='Component 1', ylab='Component 2')
qplot(scores[,3], scores[,4])

## PCA + h clustering
dist_z = dist(scores[,1:5])
hclust_z = hclust(dist_z, method='complete')
plot(hclust_z)

# clustering 

mu = attr(Z,"scaled:center")
sigma = attr(Z,"scaled:scale")


## which K is best for data set? -> just exercise 

### elbow plot
k_grid = seq(2, 20, by=1)
SSE_grid = foreach(k = k_grid, .combine='c') %do% {
  cluster_k = kmeans(Z, k, iter.max = 20, nstart=25)
  cluster_k$tot.withinss
}
plot(k_grid, SSE_grid)

### CH index
N = nrow(Z)

CH_grid = foreach(k = k_grid, .combine='c') %do% {
  cluster_k = kmeans(Z, k, iter.max = 20, nstart=25)
  W = cluster_k$tot.withinss
  B = cluster_k$betweenss
  CH = (B/W)*((N-k)/(k-1))
  CH
}

plot(k_grid, CH_grid)

### gat_stat -> the result was weird
cars_gap = clusGap(Z, FUN = kmeans, nstart = 25, K.max = 10, B = 100)
plot(cars_gap) # K =5 ??

fviz_nbclust(Z, kmeans, method="gap_stat") 

## clustering

clustKM = kmeanspp(Z, k=6, nstart=25)
fviz_cluster(clustKM, data = Z, 
             ellipse.type = "convex", palette="jco", ggtheme = theme_minimal())



## analysis

clst1 = clustKM$center[1,]*sigma + mu
sort(clst1, decreasing = TRUE) %>% head(5)

clst2 = clustKM$center[2,]*sigma + mu
sort(clst2, decreasing = TRUE) %>% head(5)

clst3 = clustKM$center[3,]*sigma + mu
sort(clst3, decreasing = TRUE) %>% head(5)

clst4 = clustKM$center[4,]*sigma + mu
sort(clst4, decreasing = TRUE) %>% head(5)

clst5 = clustKM$center[5,]*sigma + mu
sort(clst5, decreasing = TRUE) %>% head(5)

clst6 = clustKM$center[6,]*sigma + mu
sort(clst6, decreasing = TRUE) %>% head(5)
