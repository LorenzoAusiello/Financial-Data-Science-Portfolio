
#PROBLEM 1

#store data in a variable
setwd('C:/Users/loaus/OneDrive - stevens.edu/STEVENS/Foundations of Financial Data Science/Assignment/Assignment3/HW3_data')
weekly_dir<-read.csv('Weekly.csv')
head(weekly_dir)

#exploratory data analysis
library(ggplot2)
library(gridExtra)
library(doBy)
weekly_dir$Direction<-as.factor(weekly_dir$Direction)
summary(weekly_dir)
contrasts(weekly_dir$Direction)

par(mfrow = c(2, 3))
plot1 <- ggplot(data = weekly_dir, aes(y = Lag1, x = Direction)) +
  geom_boxplot() +
  ggtitle("Box Plot: Lag1 and Direction")
plot2 <- ggplot(data = weekly_dir, aes(y = Lag2, x = Direction)) +
  geom_boxplot() +
  ggtitle("Box Plot: Lag2 and Direction")
plot3 <- ggplot(data = weekly_dir, aes(y = Lag3, x = Direction)) +
  geom_boxplot() +
  ggtitle("Box Plot: Lag3 and Direction")
plot4 <- ggplot(data = weekly_dir, aes(y = Lag4, x = Direction)) +
  geom_boxplot() +
  ggtitle("Box Plot: Lag4 and Direction")
plot5 <- ggplot(data = weekly_dir, aes(y = Lag5, x = Direction)) +
  geom_boxplot() +
  ggtitle("Box Plot: Lag5 and Direction")
plot6 <- ggplot(data = weekly_dir, aes(y = Volume, x = Direction)) +
  geom_boxplot() +
  ggtitle("Box Plot: Volume and Direction")

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6)


{par(mfrow = c(2, 3))
plot(Today ~ Lag1, col = "darkred", data = weekly_dir)
simplelm1 <- lm(Today ~ Lag1, data = weekly_dir)
abline(simplelm1, lwd = 3, col = "darkgreen")
title("Today vs. Lag1")
plot(Today ~ Lag2, col = "darkred", data = weekly_dir)
simplelm2 <- lm(Today ~ Lag2, data = weekly_dir)
abline(simplelm2, lwd = 3, col = "darkgreen")
title("Today vs. Lag2")
plot(Today ~ Lag3, col = "darkred", data = weekly_dir)
simplelm3 <- lm(Today ~ Lag3, data = weekly_dir)
abline(simplelm3, lwd = 3, col = "darkgreen")
title("Today vs. Lag3")
plot(Today ~ Lag4, col = "darkred", data = weekly_dir)
simplelm4 <- lm(Today ~ Lag4, data = weekly_dir)
abline(simplelm4, lwd = 3, col = "darkgreen")
title("Today vs. Lag4")
plot(Today ~ Lag5, col = "darkred", data = weekly_dir)
simplelm5 <- lm(Today ~ Lag5, data = weekly_dir)
abline(simplelm5, lwd = 3, col = "darkgreen")
title("Today vs. Lag5")
plot(Today ~ Volume, col = "darkred", data = weekly_dir)
simplelm6 <- lm(Today ~ Volume, data = weekly_dir)
abline(simplelm6, lwd = 3, col = "darkgreen")
title("Today vs. Volume")}

quant_analysis<-function(x){
  c(mean(x),length(x),min(x),max(x))
}

summary.Lag1.Dir<-summaryBy(Lag1~Direction, data=weekly_dir, FUN=quant_analysis)
colnames(summary.Lag1.Dir)<-c('Direction','Mean','Length','Min','Max')
summary.Lag2.Dir<-summaryBy(Lag2~Direction, data=weekly_dir, FUN=quant_analysis)
colnames(summary.Lag2.Dir)<-c('Direction','Mean','Length','Min','Max')
summary.Lag3.Dir<-summaryBy(Lag3~Direction, data=weekly_dir, FUN=quant_analysis)
colnames(summary.Lag3.Dir)<-c('Direction','Mean','Length','Min','Max')
summary.Lag4.Dir<-summaryBy(Lag4~Direction, data=weekly_dir, FUN=quant_analysis)
colnames(summary.Lag4.Dir)<-c('Direction','Mean','Length','Min','Max')
summary.Lag5.Dir<-summaryBy(Lag5~Direction, data=weekly_dir, FUN=quant_analysis)
colnames(summary.Lag5.Dir)<-c('Direction','Mean','Length','Min','Max')
summary.Volume.Dir<-summaryBy(Volume~Direction, data=weekly_dir, FUN=quant_analysis)
colnames(summary.Volume.Dir)<-c('Direction','Mean','Length','Min','Max')
summary.Lag1.Dir
summary.Lag2.Dir
summary.Lag3.Dir
summary.Lag4.Dir
summary.Lag5.Dir
summary.Volume.Dir

##Logistic regression
glm.direction.lags=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=weekly_dir,family=binomial)
summary(glm.direction.lags)
confint(glm.direction.lags)

#predicted values and performance check
glm.probs=predict(glm.direction.lags,newdata = weekly_dir, type="response")
glm.probs[1:10]
glm.pred=rep("Down",dim(weekly_dir)[1])
glm.pred[glm.probs>0.5]="Up"
summary(as.factor(glm.pred))
prediction.glm=cbind(weekly_dir,glm.pred)
colnames(prediction.glm)[10]="Direction prediction"
head(prediction.glm)

#confusion matrix
table(glm.pred,weekly_dir$Direction)
mean(glm.pred == weekly_dir$Direction)

#overall error rate
1-mean(glm.pred == weekly_dir$Direction)

#percentage of true down not identified
430/(430+54)

#percentage of true down that correctly identified
54/(430+54)

#percentage of true up that are correctly identified
557/(48+557)

#percentage of true up not correctly identified
48/(48+557)

#splitting dataset in training and test data
training=weekly_dir[1:985,]
test=weekly_dir[986:1089,]

##Logistic regression: training data
glm.fit=glm(Direction~Lag2,data=training,family=binomial)
summary(glm.fit)
confint(glm.fit)

#prediction and performance check: test data
glm.probs=predict(glm.fit,newdata = test, type="response")
glm.probs[1:10]
glm.pred=rep("Down",dim(test)[1])
glm.pred[glm.probs>0.5]="Up"
prediction.glm=cbind(test,glm.pred)
colnames(prediction.glm)[10]="Direction prediction"
head(prediction.glm)

#confusion matrix
table(glm.pred,test$Direction)
mean(glm.pred == test$Direction)

#overall error rate
Logistic.overall.er=1-mean(glm.pred == test$Direction)
Logistic.overall.er

Lag2<-seq(-100,100,0.01)
prob<-exp(glm.fit$coefficients[1]+glm.fit$coefficients[2]*Lag2)/(1+exp(glm.fit$coefficients[1]+glm.fit$coefficients[2]*Lag2))
graph<-data.frame(Lag2,prob)
ggplot(graph, aes(x=Lag2,y=prob))+
  geom_ribbon(aes(ymin=0,ymax=1),alpha=0.2)+geom_line(size=1)



##LDA
library(MASS)
lda.fit=lda(Direction~Lag2,data=training)
lda.fit
plot(lda.fit)

#prediction and performance check: test data
lda.pred=predict(lda.fit,test)
lda.class=lda.pred$class
head(lda.class)
head(lda.pred$posterior)
head(lda.pred$x)
prediction.lda=cbind(test,lda.class)
colnames(prediction.lda)[10]="Direction prediction"
head(prediction.lda)

#confusion matrix
table(lda.class ,test$Direction)
mean(lda.class == test$Direction)
sum(lda.pred$posterior [ ,1] >=.5)
sum(lda.pred$posterior [,1]<.5)

#overall error rate
LDA.overall.er=1-mean(lda.class == test$Direction)
LDA.overall.er


#QDA
qda.fit=qda(Direction~Lag2,data=training)
qda.fit

#prediction and performance check: test data
qda.pred=predict(qda.fit,test)
qda.class=qda.pred$class
head(qda.class)
head(qda.pred$posterior)
prediction.qda=cbind(test,qda.class)
colnames(prediction.qda)[10]="Direction prediction"
head(prediction.qda)

#confusion matrix
table(qda.class ,test$Direction)
mean(qda.class == test$Direction)

#overall error rate
QDA.overall.er=1-mean(qda.class == test$Direction)
QDA.overall.er

#KNN1
test.x=matrix(test$Lag2)
training.x=matrix(training$Lag2)
library(class)
knn.pred=knn(training.x, test.x, training$Direction, k=1)
prediction.knn=cbind(test,knn.pred)
colnames(prediction.knn)[10]="Direction prediction"
head(prediction.knn)

#confusion matrix
table(knn.pred ,test$Direction)
mean(knn.pred == test$Direction)

#overall error rate
KNN1.overall.er=1-mean(knn.pred == test$Direction)
KNN1.overall.er

#KNN2
test.x=matrix(test$Lag2)
training.x=matrix(training$Lag2)
library(class)
knn.pred=knn(training.x, test.x, training$Direction, k=2)
prediction.knn=cbind(test,knn.pred)
colnames(prediction.knn)[10]="Direction prediction"
head(prediction.knn)

#confusion matrix
table(knn.pred ,test$Direction)
mean(knn.pred == test$Direction)

#overall error rate
KNN2.overall.er=1-mean(knn.pred == test$Direction)
KNN2.overall.er

#KNN3
test.x=matrix(test$Lag2)
training.x=matrix(training$Lag2)
library(class)
knn.pred=knn(training.x, test.x, training$Direction, k=3)
prediction.knn=cbind(test,knn.pred)
colnames(prediction.knn)[10]="Direction prediction"
head(prediction.knn)

#confusion matrix
table(knn.pred ,test$Direction)
mean(knn.pred == test$Direction)

#overall error rate
KNN3.overall.er=1-mean(knn.pred == test$Direction)
KNN3.overall.er

#KNN4
test.x=matrix(test$Lag2)
training.x=matrix(training$Lag2)
library(class)
knn.pred=knn(training.x, test.x, training$Direction, k=4)
prediction.knn=cbind(test,knn.pred)
colnames(prediction.knn)[10]="Direction prediction"
head(prediction.knn)

#confusion matrix
table(knn.pred ,test$Direction)
mean(knn.pred == test$Direction)

#overall error rate
KNN4.overall.er=1-mean(knn.pred == test$Direction)
KNN4.overall.er

#KNN5
test.x=matrix(test$Lag2)
training.x=matrix(training$Lag2)
library(class)
knn.pred=knn(training.x, test.x, training$Direction, k=5)
prediction.knn=cbind(test,knn.pred)
colnames(prediction.knn)[10]="Direction prediction"
head(prediction.knn)

#confusion matrix
table(knn.pred ,test$Direction)
mean(knn.pred == test$Direction)

#overall error rate
KNN5.overall.er=1-mean(knn.pred == test$Direction)
KNN5.overall.er

#KNN6
test.x=matrix(test$Lag2)
training.x=matrix(training$Lag2)
library(class)
knn.pred=knn(training.x, test.x, training$Direction, k=6)
prediction.knn=cbind(test,knn.pred)
colnames(prediction.knn)[10]="Direction prediction"
head(prediction.knn)

#confusion matrix
table(knn.pred ,test$Direction)
mean(knn.pred == test$Direction)

#overall error rate
KNN6.overall.er=1-mean(knn.pred == test$Direction)
KNN6.overall.er

#KNN10
test.x=matrix(test$Lag2)
training.x=matrix(training$Lag2)
library(class)
knn.pred=knn(training.x, test.x, training$Direction, k=10)
prediction.knn=cbind(test,knn.pred)
colnames(prediction.knn)[10]="Direction prediction"
head(prediction.knn)

#confusion matrix
table(knn.pred ,test$Direction)
mean(knn.pred == test$Direction)

#overall error rate
KNN10.overall.er=1-mean(knn.pred == test$Direction)
KNN10.overall.er

#KNN20
test.x=matrix(test$Lag2)
training.x=matrix(training$Lag2)
library(class)
knn.pred=knn(training.x, test.x, training$Direction, k=20)
prediction.knn=cbind(test,knn.pred)
colnames(prediction.knn)[10]="Direction prediction"
head(prediction.knn)

#confusion matrix
table(knn.pred ,test$Direction)
mean(knn.pred == test$Direction)

#overall error rate
KNN20.overall.er=1-mean(knn.pred == test$Direction)
KNN20.overall.er

#compare results
comparison=data.frame(Logistic.overall.er,LDA.overall.er,QDA.overall.er,KNN1.overall.er,
                      KNN2.overall.er,KNN3.overall.er,KNN4.overall.er,KNN5.overall.er,
                      KNN6.overall.er,KNN10.overall.er,KNN20.overall.er)
rownames(comparison)<-'Overall error rate'
comparison


##Logistic regression: training data
glm.fit=glm(Direction~Lag1+Lag2,data=training,family=binomial)
summary(glm.fit)
confint(glm.fit)

#prediction and performance check: test data
glm.probs=predict(glm.fit,newdata = test, type="response")
glm.probs[1:10]
glm.pred=rep("Down",dim(test)[1])
glm.pred[glm.probs>0.5]="Up"
prediction.glm=cbind(test,glm.pred)
colnames(prediction.glm)[10]="Direction prediction"
head(prediction.glm)

#confusion matrix
table(glm.pred,test$Direction)
mean(glm.pred == test$Direction)

#overall error rate
1-mean(glm.pred == test$Direction)


##LDA
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=training)
lda.fit
plot(lda.fit)

#prediction and performance check: test data
lda.pred=predict(lda.fit,test)
lda.class=lda.pred$class
head(lda.class)
head(lda.pred$posterior)
head(lda.pred$x)
prediction.lda=cbind(test,lda.class)
colnames(prediction.lda)[10]="Direction prediction"
head(prediction.lda)

#confusion matrix
table(lda.class ,test$Direction)
mean(lda.class == test$Direction)
sum(lda.pred$posterior [ ,1] >=.5)
sum(lda.pred$posterior [,1]<.5)

#overall error rate
LDA.overall.er=1-mean(lda.class == test$Direction)
LDA.overall.er



#PROBLEM 2
data<-read.csv('Auto.csv')

#create a binary variable
data$mpg01<-0
data$mpg01[data$mpg>median(data$mpg)]<-1
data$mpg01<-as.factor(data$mpg01)
summary(data)

par(mfrow = c(2, 3))
plot1 <-ggplot(data=data, aes(y=cylinders,x= mpg01))+
  geom_boxplot()+
  ggtitle("Box Plot: Cylinders and mpg01")
plot2 <- ggplot(data=data, aes(y=displacement,x= mpg01))+
  geom_boxplot() +
  ggtitle("Box Plot: displacement and mpg01")
plot3 <- ggplot(data=data, aes(y=horsepower,x= mpg01))+
  geom_boxplot() +
  ggtitle("Box Plot: horsepower and mpg01")
plot4 <- ggplot(data=data, aes(y=weight,x= mpg01))+
  geom_boxplot() +
  ggtitle("Box Plot: weight and mpg01")
plot5 <- ggplot(data=data, aes(y=acceleration,x= mpg01))+
  geom_boxplot() +
  ggtitle("Box Plot: acceleration and mpg01")
plot6 <- ggplot(data=data, aes(y=year,x= mpg01))+
  geom_boxplot() +
  ggtitle("Box Plot: year and mpg01")

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6)

par(mfrow = c(2, 2))
plot1 <-ggplot(data=data, aes(y=mpg,x= cylinders))+
  geom_point()+
  ggtitle("Scatter Plot: Cylinders and mpg01")
plot2 <-ggplot(data=data, aes(y=mpg,x= displacement))+
  geom_point() +
  ggtitle("Scatter Plot: displacement and mpg01")
plot3 <- ggplot(data=data, aes(y=mpg,x= horsepower))+
  geom_point() +
  ggtitle("Scatter Plot: horsepower and mpg01")
plot4 <- ggplot(data=data, aes(y=mpg,x= weight))+geom_point() +
  ggtitle("Scatter Plot: weight and mpg01")

grid.arrange(plot1, plot2, plot3, plot4)

#splitting dataset in training and test data
training<-data[1:250,]
test<-data[251:392,]
test$mpg01<-as.factor(test$mpg01)
training$mpg01<-as.factor(training$mpg01)

#Logistic regression: training data
glm.mpg01=glm(mpg01~cylinders+weight+horsepower+displacement,data=training,family=binomial)
summary(glm.mpg01)
confint(glm.mpg01)

#predicted values and performance check:test data
glm.probs=predict(glm.mpg01,newdata = test, type="response")
glm.probs[1:10]
glm.pred=rep("0",dim(test)[1])
glm.pred[glm.probs>0.5]="1"
prediction.glm=cbind(test,glm.pred)
colnames(prediction.glm)[11]="mpg01 prediction"
head(prediction.glm)

#confusion matrix
table(glm.pred,test$mpg01)
mean(glm.pred == test$mpg01)

#overall error rate
1-mean(glm.pred == test$mpg01)

##LDA
library(MASS)
lda.fit=lda(mpg01~cylinders+weight+horsepower+displacement,data=training)
lda.fit
plot(lda.fit)

#prediction and performance check: test data
lda.pred=predict(lda.fit,test)
lda.class=lda.pred$class
head(lda.class)
head(lda.pred$posterior)
head(lda.pred$x)
prediction.lda=cbind(test,lda.class)
colnames(prediction.lda)[11]="mpg01 prediction"
head(prediction.lda)

#confusion matrix
table(lda.class ,test$mpg01)
mean(lda.class == test$mpg01)

#overall error rate
1-mean(lda.class == test$mpg01)

#QDA
qda.fit=qda(mpg01~cylinders+weight+horsepower+displacement,data=training)
qda.fit

#prediction and performance check: test data
qda.pred=predict(qda.fit,test)
qda.class=qda.pred$class
head(qda.class)
head(qda.pred$posterior)
prediction.qda=cbind(test,qda.class)
colnames(prediction.qda)[11]="mpg01 prediction"
head(prediction.qda)

#confusion matrix
table(qda.class ,test$mpg01)
mean(qda.class == test$mpg01)

#overall error rate
1-mean(qda.class == test$mpg01)

#KNN K=1
test.x=cbind(test$cylinders,test$weight,test$horsepower,test$displacement)
training.x=cbind(training$cylinders,training$weight,training$horsepower,training$displacement)
library(class)
knn.pred=knn(training.x, test.x, training$mpg01, k=1)
prediction.knn=cbind(test,knn.pred)
colnames(prediction.knn)[11]="mpg01 prediction"
head(prediction.knn)

#confusion matrix
table(knn.pred ,test$mpg01)
mean(knn.pred == test$mpg01)

#overall error rate
KNN1=1-mean(knn.pred == test$mpg01)


#KNN K=2
test.x=cbind(test$cylinders,test$weight,test$horsepower,test$displacement)
training.x=cbind(training$cylinders,training$weight,training$horsepower,training$displacement)
library(class)
knn.pred=knn(training.x, test.x, training$mpg01, k=2)
prediction.knn=cbind(test,knn.pred)
colnames(prediction.knn)[11]="mpg01 prediction"
head(prediction.knn)

#confusion matrix
table(knn.pred ,test$mpg01)
mean(knn.pred == test$mpg01)

#overall error rate
KNN2=1-mean(knn.pred == test$mpg01)

#KNN K=3
test.x=cbind(test$cylinders,test$weight,test$horsepower,test$displacement)
training.x=cbind(training$cylinders,training$weight,training$horsepower,training$displacement)
library(class)
knn.pred=knn(training.x, test.x, training$mpg01, k=3)
prediction.knn=cbind(test,knn.pred)
colnames(prediction.knn)[11]="mpg01 prediction"
head(prediction.knn)

#confusion matrix
table(knn.pred ,test$mpg01)
mean(knn.pred == test$mpg01)

#overall error rate
KNN3=1-mean(knn.pred == test$mpg01)

#KNN K=4
test.x=cbind(test$cylinders,test$weight,test$horsepower,test$displacement)
training.x=cbind(training$cylinders,training$weight,training$horsepower,training$displacement)
library(class)
knn.pred=knn(training.x, test.x, training$mpg01, k=4)
prediction.knn=cbind(test,knn.pred)
colnames(prediction.knn)[11]="mpg01 prediction"
head(prediction.knn)

#confusion matrix
table(knn.pred ,test$mpg01)
mean(knn.pred == test$mpg01)

#overall error rate
KNN4=1-mean(knn.pred == test$mpg01)

#KNN K=5
test.x=cbind(test$cylinders,test$weight,test$horsepower,test$displacement)
training.x=cbind(training$cylinders,training$weight,training$horsepower,training$displacement)
library(class)
knn.pred=knn(training.x, test.x, training$mpg01, k=5)
prediction.knn=cbind(test,knn.pred)
colnames(prediction.knn)[11]="mpg01 prediction"
head(prediction.knn)

#confusion matrix
table(knn.pred ,test$mpg01)
mean(knn.pred == test$mpg01)

#overall error rate
KNN5=1-mean(knn.pred == test$mpg01)

#KNN K=6
test.x=cbind(test$cylinders,test$weight,test$horsepower,test$displacement)
training.x=cbind(training$cylinders,training$weight,training$horsepower,training$displacement)
library(class)
knn.pred=knn(training.x, test.x, training$mpg01, k=6)
prediction.knn=cbind(test,knn.pred)
colnames(prediction.knn)[11]="mpg01 prediction"
head(prediction.knn)

#confusion matrix
table(knn.pred ,test$mpg01)
mean(knn.pred == test$mpg01)

#overall error rate
KNN6=1-mean(knn.pred == test$mpg01)

#KNN K=10
test.x=cbind(test$cylinders,test$weight,test$horsepower,test$displacement)
training.x=cbind(training$cylinders,training$weight,training$horsepower,training$displacement)
library(class)
knn.pred=knn(training.x, test.x, training$mpg01, k=10)
prediction.knn=cbind(test,knn.pred)
colnames(prediction.knn)[11]="mpg01 prediction"
head(prediction.knn)

#confusion matrix
table(knn.pred ,test$mpg01)
mean(knn.pred == test$mpg01)

#overall error rate
KNN10=1-mean(knn.pred == test$mpg01)

#KNN K=20
test.x=cbind(test$cylinders,test$weight,test$horsepower,test$displacement)
training.x=cbind(training$cylinders,training$weight,training$horsepower,training$displacement)
library(class)
knn.pred=knn(training.x, test.x, training$mpg01, k=20)
prediction.knn=cbind(test,knn.pred)
colnames(prediction.knn)[11]="mpg01 prediction"
head(prediction.knn)

#confusion matrix
table(knn.pred ,test$mpg01)
mean(knn.pred == test$mpg01)

#overall error rate
KNN20=1-mean(knn.pred == test$mpg01)

#compare results
data.frame(KNN1, KNN2, KNN3, KNN4, KNN5, KNN6,KNN10,KNN20,row.names='Overall error rate')


