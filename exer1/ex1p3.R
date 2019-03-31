# loading packages
library(tidyverse)
library(FNN)

# import dataset 
sclass = read.csv("../data/sclass.csv")
head(sclass)

# transform data frame
scl = select(sclass, trim, mileage, price)
scl_350 = subset(scl, trim=="350")
scl_65AMG = subset(scl, trim=="65 AMG")

# plot
ggplot() +
  geom_point(data=scl_350, aes(mileage, price), color="red") +
  geom_point(data=scl_65AMG, aes(mileage, price), color="blue") +
  labs(title="mileage vs price", caption="red: S Class 350  blue: S Class 65 AMG")

# make a train-test split
N350 = nrow(scl_350)
N350_train = floor(0.8*N350)
N350_test = N350-N350_train

N65M = nrow(scl_65AMG)
N65M_train = floor(0.8*N65M)
N65M_test = N65M-N65M_train

# slpit
train_ind_350 = sample.int(N350, N350_train, replace=F)
D350_train = scl_350[train_ind_350,]
D350_test = scl_350[-train_ind_350,]

train_ind_65M = sample.int(N65M, N65M_train, replace=F)
D65M_train = scl_65AMG[train_ind_65M,]
D65M_test = scl_65AMG[-train_ind_65M,]

# Arrange
D350_test = arrange(D350_test, mileage)
D65M_test = arrange(D65M_test, mileage)

# seperate X and Y
x350_train = select(D350_train, mileage)
y350_train = select(D350_train, price)
x350_test = select(D350_test, mileage)
y350_test = select(D350_test, price)

x65M_train = select(D65M_train, mileage)
y65M_train = select(D65M_train, price)
x65M_test = select(D65M_test, mileage)
y65M_test = select(D65M_test, price)

# rmse function
rmse = function(y, ypred) {
  sqrt(mean(data.matrix((y-ypred)^2)))
}

# predict
## 350
### 
K = 3

knn350 = knn.reg(train = x350_train, test=x350_test, y=y350_train, k=K)
D350_test$ypred_knn350 = knn350$pred
ggplot(data = D350_test) +
  geom_point(mapping = aes(mileage, price), color='red') + 
  geom_point(aes(mileage, knn350$pred), color='blue')+ 
  geom_path(aes(mileage, knn350$pred), color='light blue')

rmse(y350_test, knn350$pred)

### 
K = 6

knn350 = knn.reg(train = x350_train, test=x350_test, y=y350_train, k=K)
D350_test$ypred_knn350 = knn350$pred
ggplot(data = D350_test) +
  geom_point(mapping = aes(mileage, price), color='red') + 
  geom_point(aes(mileage, knn350$pred), color='blue')+ 
  geom_path(aes(mileage, knn350$pred), color='light blue')

rmse(y350_test, knn350$pred)

### 
K = 20

knn350 = knn.reg(train = x350_train, test=x350_test, y=y350_train, k=K)
D350_test$ypred_knn350 = knn350$pred
ggplot(data = D350_test) +
  geom_point(mapping = aes(mileage, price), color='red') + 
  geom_point(aes(mileage, knn350$pred), color='blue')+ 
  geom_path(aes(mileage, knn350$pred), color='light blue')

rmse(y350_test, knn350$pred)


### 
K = 60

knn350 = knn.reg(train = x350_train, test=x350_test, y=y350_train, k=K)
D350_test$ypred_knn350 = knn350$pred
ggplot(data = D350_test) +
  geom_point(mapping = aes(mileage, price), color='red') + 
  geom_point(aes(mileage, knn350$pred), color='blue')+ 
  geom_path(aes(mileage, knn350$pred), color='light blue')

rmse(y350_test, knn350$pred)

### 
K = 150

knn350 = knn.reg(train = x350_train, test=x350_test, y=y350_train, k=K)
D350_test$ypred_knn350 = knn350$pred
ggplot(data = D350_test) +
  geom_point(mapping = aes(mileage, price), color='red') + 
  geom_point(aes(mileage, knn350$pred), color='blue')+ 
  geom_path(aes(mileage, knn350$pred), color='light blue')

rmse(y350_test, knn350$pred)

### 
K = 332

knn350 = knn.reg(train = x350_train, test=x350_test, y=y350_train, k=K)
D350_test$ypred_knn350 = knn350$pred
ggplot(data = D350_test) +
  geom_point(mapping = aes(mileage, price), color='red') + 
  geom_point(aes(mileage, knn350$pred), color='blue')+ 
  geom_path(aes(mileage, knn350$pred), color='light blue')

rmse(y350_test, knn350$pred)

## 65AMG
###
K=3
knn65M = knn.reg(train = x65M_train, test=x65M_test, y=y65M_train, k=K)
D65M_test$ypred_knn65M = knn65M$pred

ggplot(data = D65M_test) +
  geom_point(mapping = aes(mileage, price), color='red') +
  geom_point(aes(mileage, knn65M$pred), color='blue') + 
  geom_path(aes(mileage, knn65M$pred), color='light blue')

rmse(y65M_test, knn65M$pred)

###
K=5
knn65M = knn.reg(train = x65M_train, test=x65M_test, y=y65M_train, k=K)
D65M_test$ypred_knn65M = knn65M$pred

ggplot(data = D65M_test) +
  geom_point(mapping = aes(mileage, price), color='red') +
  geom_point(aes(mileage, knn65M$pred), color='blue') + 
  geom_path(aes(mileage, knn65M$pred), color='light blue')

rmse(y65M_test, knn65M$pred)

###
K=10
knn65M = knn.reg(train = x65M_train, test=x65M_test, y=y65M_train, k=K)
D65M_test$ypred_knn65M = knn65M$pred

ggplot(data = D65M_test) +
  geom_point(mapping = aes(mileage, price), color='red') +
  geom_point(aes(mileage, knn65M$pred), color='blue') + 
  geom_path(aes(mileage, knn65M$pred), color='light blue')

rmse(y65M_test, knn65M$pred)

###
K=40
knn65M = knn.reg(train = x65M_train, test=x65M_test, y=y65M_train, k=K)
D65M_test$ypred_knn65M = knn65M$pred

ggplot(data = D65M_test) +
  geom_point(mapping = aes(mileage, price), color='red') +
  geom_point(aes(mileage, knn65M$pred), color='blue') + 
  geom_path(aes(mileage, knn65M$pred), color='light blue')

rmse(y65M_test, knn65M$pred)

###
K=100
knn65M = knn.reg(train = x65M_train, test=x65M_test, y=y65M_train, k=K)
D65M_test$ypred_knn65M = knn65M$pred

ggplot(data = D65M_test) +
  geom_point(mapping = aes(mileage, price), color='red') +
  geom_point(aes(mileage, knn65M$pred), color='blue') + 
  geom_path(aes(mileage, knn65M$pred), color='light blue')

rmse(y65M_test, knn65M$pred)

###
K=233
knn65M = knn.reg(train = x65M_train, test=x65M_test, y=y65M_train, k=K)
D65M_test$ypred_knn65M = knn65M$pred

ggplot(data = D65M_test) +
  geom_point(mapping = aes(mileage, price), color='red') +
  geom_point(aes(mileage, knn65M$pred), color='blue') + 
  geom_path(aes(mileage, knn65M$pred), color='light blue')

rmse(y65M_test, knn65M$pred)

