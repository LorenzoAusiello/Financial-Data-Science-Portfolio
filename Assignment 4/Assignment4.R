
##Problem 1
  
#store data in a variable
setwd('C:/Users/loaus/OneDrive - stevens.edu/STEVENS/Foundations of Financial Data Science/Assignment/Assignment4/HW4_data')
dataset <- read.csv('OJ.csv')

#split dataset in a training dataset and a test dataset (randomly)
dataset$Purchase <- as.factor(dataset$Purchase)
summary(dataset)
set.seed(123)
train <- sample(1:nrow(dataset),800)
training <- dataset[train,]
test <- dataset[-train,]

#fit a tree
library(tree)
tree.purchase = tree(Purchase~., dataset, subset = train)
summary(tree.purchase)
plot(tree.purchase)
text(tree.purchase,pretty=0)
tree.purchase

#training error rate
tree.pred=predict(tree.purchase,training,type="class")
table(tree.pred,training$Purchase)
1-mean(tree.pred == training$Purchase)

#test error rate
tree.pred=predict(tree.purchase,test,type="class")
table(tree.pred,test$Purchase)
1-mean(tree.pred == test$Purchase)

#cv to determine optimal tree
cv.purchase=cv.tree(tree.purchase,FUN=prune.misclass)
cv.purchase
par(mfrow=c(2,2))
plot(cv.purchase$size,cv.purchase$dev,type="b")
plot(cv.purchase$k,cv.purchase$dev,type="b")
plot(cv.purchase$size,cv.purchase$dev/800,type="b")
plot(cv.purchase$k,cv.purchase$dev/800,type="b")
prune.purchase=prune.misclass(tree.purchase,best=5)
plot(prune.purchase)
text(prune.purchase,pretty=0)
tree.pred=predict(prune.purchase,test,type="class")
table(tree.pred,test$Purchase)
1-mean(tree.pred == test$Purchase)

prune.purchase=prune.misclass(tree.purchase,best=8)
plot(prune.purchase)
text(prune.purchase,pretty=0)
tree.pred=predict(prune.purchase,test,type="class")
table(tree.pred,test$Purchase)
1-mean(tree.pred == test$Purchase)

##Problem 2

#store data in a variable
dataset <- read.csv('CARAVAN.csv')

#split dataset in a training dataset and a test dataset (randomly)
dataset$Purchase <- as.factor(dataset$Purchase)
summary(dataset)
Purchase<-rep(0,5822)
Purchase[dataset$Purchase=='Yes'] <- 1
dataset$Purchase <- Purchase
set.seed(1)
train <- sample(1:nrow(dataset),1000)
training <- dataset[train,]
test <- dataset[-train,]

#fit a boosting model
library(gbm)
boost=gbm(Purchase~.,data=training,distribution="bernoulli",n.trees=1000,shrinkage=0.01,verbose=F)
summary(boost)

par(mfrow=c(1,2))
plot(boost,i="PPERSAUT")
plot(boost,i="MAUT2")

#predictions
yhat.boost=predict(boost,newdata=test,n.trees=1000)
gbm.probs<-plogis(yhat.boost)

gbm.pred=rep("0",dim(test)[1])
gbm.pred[gbm.probs>0.2]="1"

#confusion matrix
table(gbm.pred,test$Purchase)
mean(gbm.pred == test$Purchase)

#overall error rate
1-mean(gbm.pred == test$Purchase)


##Logistic regression
glm=glm(Purchase~.,data=training,family=binomial)
summary(glm)

#predicted values and performance check
glm.probs=predict(glm,newdata = test, type="response")
glm.probs[1:10]
glm.pred=rep("0",dim(test)[1])
glm.pred[glm.probs>0.2]="1"

#confusion matrix
table(glm.pred,test$Purchase)
mean(glm.pred == test$Purchase)

#overall error rate
1-mean(glm.pred == test$Purchase)

#KNN20
test.x=as.matrix(test[,1:85])
training.x=as.matrix(training[,1:85])
library(class)
knn.model=knn(training.x, test.x, training$Purchase, k=20, prob=TRUE)
knn.probs=attr(knn.model, "prob")
knn.probs[1:10]
knn.pred=rep("0",dim(test)[1])
knn.pred[knn.probs<0.8]="1"

#confusion matrix
table(knn.pred ,test$Purchase)
mean(knn.pred == test$Purchase)

#overall error rate
1-mean(knn.pred == test$Purchase)


##Problem 3

#Generate a simulated dataset
set.seed(123)
num_obs <- 20
num_vars <- 50

class_1 <- matrix(rnorm(num_obs * num_vars, mean = 0, sd = 1), nrow = num_obs, ncol = num_vars)
class_2 <- matrix(rnorm(num_obs * num_vars, mean = 1, sd = 1), nrow = num_obs, ncol = num_vars)
class_3 <- matrix(rnorm(num_obs * num_vars, mean = 2, sd = 1), nrow = num_obs, ncol = num_vars)

dataset <- rbind(class_1, class_2, class_3)

dataset <- as.data.frame(dataset)

#Perform PCA
pr.out=prcomp(dataset, scale=TRUE)
head(pr.out$rotation[,1:6])
par(mfrow=c(1,1))
biplot(pr.out, scale=0)

#plot with different colors for different classes
class_colors <- c("red", "blue", "green")
plot(pr.out$x[, 1], pr.out$x[, 2], col = rep(class_colors, each = num_obs), pch = 16, xlab = "PC1", ylab = "PC2")
arrows(0, 0, pr.out$rotation[, 1], pr.out$rotation[, 2], angle = 20, length = 0.1, col = "black")
legend("topright", legend = c("Class 1", "Class 2", "Class 3"), fill = class_colors)

pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')

# K-Means Clustering
km.out=kmeans(dataset,3,nstart=20)
km.out$cluster
true.cluster = c(rep(3,20),rep(2,20),rep(1,20))
table(km.out$cluster,true.cluster)
km.out$tot.withinss

km.out=kmeans(dataset,2,nstart=20)
km.out$cluster
km.out$tot.withinss

km.out=kmeans(dataset,4,nstart=20)
km.out$cluster
km.out$tot.withinss

x <- as.matrix(pr.out$rotation[,c('PC1','PC2')])
dataset2 <- as.matrix(dataset)%*%x
dataset2 <- data.frame(dataset2)

km.out=kmeans(dataset2,3,nstart=20)
km.out$cluster
km.out$tot.withinss

km.out=kmeans(scale(dataset),3,nstart=20)
km.out$cluster
true.cluster = c(rep(1,20),rep(3,20),rep(2,20))
table(km.out$cluster,true.cluster)
km.out$tot.withinss

