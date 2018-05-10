# Jig14947

setwd("C:\\Data Science with R\\Assignments\\Graded Assignments\\Topic 10 - Logistic Regression")

data<-read.csv("goodforu-class12.csv")
data_dic<-read.csv("goodforu-variable-descriptions-class12.csv")
View(data)
str(data)
summary(data)
sum(is.na(data))


library(dplyr)
a<-data.frame(data$X2,data$X9,data$X16,data$X23,data$X30)  #Brand A
summary(data2)
str(a)

#Perciption of brand A rate 4 less
table(a$data.X23)
sum(2836,2218,3384,3722)/sum(2836,2218,3384,3722,5831,2874,1451,1040,416,342)

x<-a %>% filter(a$data.X23<=4) %>% count()
x
y<-(x/count(a))*100
y
p<-a %>% filter(a$data.X23>=5,1,0)
p
a$goodbad<-ifelse(a$data.X23>=5,1,0)
a<-a[,c(-4)]
str(a)
a$goodbad<-as.factor(a$goodbad)
str(a)

#nm<-c("farm.in","zero.tran.fat","natural.oils","good.bad","Processed") #10 = minimally, 1 = Heavily
#colnames(data2)<-nm
#colnames(data2)
#head(data2)
#data2<-data2 %>% mutate(target=ifelse(good.bad>5,1,0))
#data2<-data2 %>% mutate(Proc_mini_heav=if else(Processed>5,"minimally","Heavily"))
#View(data2)
#table(data2$Proc_mini_heav)
#table(data2$target)
#names(data2)
#data2<-data2[,-c(4,5)]
#data2

#list<-names(data2)
#list<-list[-4]
#list
#str(data2)

#Convert into factor
#data2[,list]<-lapply(data2[,list],factor)#
#str(data2)
#summary(data2)



set.seed(200)
index<-sample(nrow(a),0.70*nrow(a),replace = F)
train<-a[index,]
test<-a[-index,]
dim(train)
dim(test)


#Question <-- 8

library(gains)
library(irr)
library(caret)

#Building Logisti_Regression_Model

mod1<-glm(goodbad~.,data = a,family="binomial")
summary(mod1)

exp(coefficients(mod1))/1+exp(coefficients(mod1))
exp(coefficients(mod1))
step(mod1)

str(train)

#Building Logisti_Regression_Model

mod2<-glm(goodbad~.,data = train,family = "binomial")
summary(mod2)
coefficients(mod2)

pred<-predict(mod2,type = "response",newdata = test)
head(pred)



#Checking Good_Bad_Rate
table(train$goodbad)/nrow(train)
table(test$goodbad)/nrow(test)

# #Good Split because all the 3 data set has same good bad rate
# 
# 
# #Equation for the model is 
# #log(p/1-p) = -1.08394 + (-.37027)*X2 + (-0.41854)*X9 + (-0.48734)*X16 + (1.68645)*X30
# #The summary shows that all the 
# #variables are highly signifivant
# 
# #Model_Diagnostics
# #Predicted_Values
# 
# pred<-predict(mod,type = "response",newdata = test)
# head(pred)
# 
# predicted<-mod$fitted.values
# 
# #Checking the rate of 1, according to that the cut-off probablity value will be set
# 
# table(data2$target)/nrow(data2)

#The cut-off is 0.4957286 now Applying cut-off valued to obtain class labels for all predicted values


pred<-ifelse(pred>=0.4957286,1,0)

#other method
pred1<-predict(mod2,type = "response",newdata = test)
library(InformationValue)
outcut<-optimalCutoff(test$goodbad,pred1)[1]
outcut


#Kappa_Matrix
#library(irr)
kappa2(data.frame(test$goodbad,pred))
#The Kappa matrix is 0.435

#Confusion_Matrix
#library(caret)

confusionMatrix(pred,test$goodbad,positive="1")




#ROC Curve
library(ROCR)
predicted<-mod2$fitted.values
pred1<-prediction(predicted,train$goodbad)
pref<-performance(pred1,"tpr","fpr")
plot(pref)

auc<-performance(pred1,"auc")
auc<-unlist(slot(auc,"y.values"))
auc


test$goodbad<-as.numeric(test$goodbad)
#gains_chart
library(gains)
gains(test$goodbad,predict(mod2,type="response",newdata=test),groups = 10)
test$prob<-predict(mod2,type="response",newdata = test)

#The Gains chart shows that the top 40% of the probabilities contain 70% customers
#Who believe the Brand A snack is good for them


quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))


# summary(test$target)
# test$target<-as.numeric(test$target)
# head(test$target)


#The Confusion_Matrix shows Accuracy : 0.761
#with 1074 correct events and 4432 correct non-events
#946 incorrect events and 783 incorrect non-events

#          Reference
#Prediction    0    1
#         0 4432  783
#         1  946 1074


#Choosing different cutoffs the cutoff value with maximum kappa matrix 
#should be chosen for maximum accuracy.
# s<- seq(0.25,1,0.001)
# n<-1

library(dplyr)
b<-a %>% filter(a$data.X2==1) %>% count()
b
c<-a %>% filter(a$data.X16==1) %>% count()
c
d<-a %>% filter(a$data.X9==1) %>% count()
d

b<-data.frame(data$X2,data$X9,data$X16,data$X23,data$X30)
str(b)
b[]<-lapply(b,factor)
str(b)

mod3<-glm(data.X23~.,data = b,family = "binomial")
summary(mod3)
exp(coefficients(mod1))
step(mod1)
boxplot(a$data.X30)





















# a<-as.vector(length(s))
# 
# for(i in s){
#   print(i)
#   pred2<-ifelse(pred>i,1,0)
#   a[n]<-confusionMatrix(pred2,test$target,positive = "1")$overall[2]
#   print(n)
#   n=n+1
#   
# }
# 
# #Now a has all the diggerent kappa matrix's for diggerent cutoff sotred in it.
# #Extracting the cutoff with max kappa.
# 
# index<-which(a==max(a))
# s[index]
# 
# #The model with cutoff value of 0.39
# 
# pred3<- ifelse(pred>0.337,1,0)
# confusionMatrix(pred3,test$target,positive = "1")
# 
# #now the accuracy  : 0.761 and kappa : 0.3912 for the optimised model
# 
# 
# 
# 
# 
# 
# #Top 30% of the probablity scores lie between 0.25276207 to 0.64623214  (60% to 100%)
# #We can use this probablity to extract the data of customers
# #who think Brand A scacks are good.
# 
# #Q1 :-- Is there an impact due to processing level?
# 
# data2 %>% group_by(Proc_mini_heav) %>% summarise(count=n(),Percent_Count=n()/24114) %>% ungroup() %>% data.frame()
# data2 %>% group_by(target) %>% summarise(count=n(),Percent_Count=n()/24114) %>% ungroup() %>% data.frame()
# 
# 
# #  Proc_mini_heav count Percent_Count
# #1      Heavily 17578     0.7289541
# #2      minimally  6536     0.2710459
# 
# # target count Percent_Count
# #1      0 17991     0.7460811
# #2      1  6123     0.2539189
# 
# #Answer :- 62% customers believe Brand A snacks are heavilyprocessed and is bad for them
# #14% Customers believe Brand A snacks minimally processed and is good for them
# #12% Customers believe that the snack minimally processed and still bad for them
# #11% Customers believe Brand A snacks are heavily processed and are still good for them
# 
# 
# 
# 
# 
# 
# # #Q1:- Are my brand made with farm grown ingredients like potato, corn or wheat?
# # 
# # data2 %>% group_by(farm.in) %>% summarise(count=n(),Percent_Count=n()/24114) %>% ungroup() %>% data.frame()
# # 
# # #Ans :-- 80% Customers are belive that Brand A snacks are made with Grown_Ingredients
# # 
# # #Q2 :-- Do my brands have zero grams trans-fat?
# # 
# # data2 %>% group_by(zero.tran.fat) %>% summarise(Count=n(),Percent_Count=n()/24114) %>% ungroup() %>% data.frame()
# # 
# # #Ans :-- 32% Customers are belive that Brand A have zero grams trans-fat
# # 
# # #Q3 :-- Are my brand made with natural oils?
# 
# # data2 %>% group_by(natural.oils) %>% summarise(Count=n(),Percent_Count=n()/24114) %>% ungroup() %>% data.frame()
# # 
# # #Ans :-- 44% Customers are believe Brand A made with natural oils
# # 
# # #Q4 :-- Is there any impact due to processing level?
# # table(data2$Proc_mini_heav)
# # 
# # #Splitting the data set into train and test saples