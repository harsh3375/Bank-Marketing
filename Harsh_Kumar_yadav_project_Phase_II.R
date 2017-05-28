#Question 1
setwd("/Users/Harsh/Desktop/project_resubmit")
library(dplyr)
d <- read.csv("bank-full.csv",sep = ";",stringsAsFactors = FALSE)
glimpse(d)
str(d)

## grouping and dummy creation
## create Dummy Veriable
Dummies1=function(df,dvar){
        
        t=table(df[,dvar])
        
        t=sort(t)
        
        l2=names(t)[-1]
        
        for(l1 in l2){
                name=paste(dvar,l1,sep=".")
                name=gsub(" ","",name)
                name=gsub("-","",name)
                name=gsub("\\?","Q",name)
                
                df[,name]=as.numeric(df[,dvar]==l1)
        }
        
        df[,dvar]=NULL
        return(df)
}

##### 
str(d)
mydata <- d
mydata <- Dummies1(mydata,"job")
mydata <- Dummies1(mydata,"marital")
mydata <- Dummies1(mydata,"education")
mydata <- Dummies1(mydata,"default")
mydata <- Dummies1(mydata,"housing")
mydata <- Dummies1(mydata,"loan")
mydata <- Dummies1(mydata,"contact")
mydata <- Dummies1(mydata,"month")
mydata <- Dummies1(mydata,"poutcome")
str(mydata)

str(mydata)
sort(table(mydata$y),decending = true)

d.new=mydata %>%
        mutate(y.yes=as.numeric(y== "yes"))%>%
        select(-y)
str(d.new)
cor(d.new)

set.seed(12345)
sam=sample(1:nrow(d.new),0.70*nrow(d.new))
D_train=d.new[sam,]
test=d.new[-sam,]

###answer of  Q 2
## data prepration
library(tidyr)
library(psych)
summary(D_train)
describe(D_train)
### corelation test using H2o librry

#Finding Correlation
library(h2o)
cor(D_train)

##vif check > 5
#importing car to use vif function

library(car)
fit=lm(y.yes~., data = D_train)
summary(fit)
sort(vif(fit),decreasing = TRUE)[1:3]

fit=lm(y.yes~.-month.may,data=D_train)
summary(fit)
sort(vif(fit),decreasing = TRUE)[1:3]

fit=lm(y.yes~.-month.may-job.bluecollar,data=D_train)
summary(fit)
sort(vif(fit),decreasing = TRUE)[1:3]

fit=lm(y.yes~.-month.may-job.bluecollar-poutcome.unknown,data=D_train)
summary(fit)
sort(vif(fit),decreasing = TRUE)[1:3]

fit=lm(y.yes~.-month.may-job.bluecollar-poutcome.unknown-education.secondary,data=D_train)
summary(fit)
sort(vif(fit),decreasing = TRUE)[1:3]

fit=lm(y.yes~.-month.may-job.bluecollar-poutcome.unknown-education.secondary-contact.unknown,data=D_train)
summary(fit)
sort(vif(fit),decreasing = TRUE)[1:3]

### droped those var which vif >5
##building the data frame from D_train data removing the predictor variables month.may,job.bluecollar,poutcome.unknown,education.secondary,contact.unknown

D_train=D_train%>%
        select(month.may-job.bluecollar-poutcome.unknown-education.secondary-contact.unknown)
### que
stion 3
# Buiding a logistic regression model

fit1=glm(y.yes~.,family = "binomial", data=D_train)

summary(fit1)
fit1=step(fit1)
summary(fit1)
formula(fit1)

fit2 = glm(y.yes ~ balance + day + duration + campaign + job.student + job.unemployed + 
                   job.retired + job.services + job.admin. + job.technician + 
                   job.management + marital.single + marital.married + education.primary + 
                   education.tertiary + housing.yes + loan.no + contact.unknown + 
                   contact.cellular + month.mar + month.jan + month.feb + month.apr + 
                   month.nov + month.jun + month.aug + month.jul + month.may + 
                   poutcome.other + poutcome.failure + poutcome.unknown, data = D_train,family = "binomial")
summary(fit2)

fit2 = glm(y.yes ~ balance + day + duration + campaign + job.student + job.unemployed + 
                   job.retired + job.services + job.admin. + job.technician + 
                   job.management + marital.single + marital.married + education.primary + 
                   education.tertiary + housing.yes + loan.no + contact.unknown + 
                   month.mar + month.jan + month.feb + month.apr + 
                   month.nov + month.jun + month.aug + month.jul + month.may + 
                   poutcome.other + poutcome.failure + poutcome.unknown, data = D_train,family = "binomial")

summary(fit2)

fit2 = glm(y.yes ~ balance + day + duration + campaign + job.student + job.unemployed + 
                   job.retired + job.services + job.admin. + job.technician + 
                   job.management + marital.married + education.primary + 
                   education.tertiary + housing.yes + loan.no + contact.unknown + 
                   month.mar + month.jan + month.feb + month.apr + 
                   month.nov + month.jun + month.aug + month.jul + month.may + 
                   poutcome.other + poutcome.failure + poutcome.unknown, data = D_train,family = "binomial")
summary(fit2)

fit2 = glm(y.yes ~ balance + day + duration + campaign + job.student + 
                   job.retired + job.services + job.admin. + job.technician + 
                   job.management + marital.married + education.primary + 
                   education.tertiary + housing.yes + loan.no + contact.unknown + 
                   month.mar + month.jan + month.feb + month.apr + 
                   month.nov + month.jun + month.aug + month.jul + month.may + 
                   poutcome.other + poutcome.failure + poutcome.unknown, data = D_train,family = "binomial")
summary(fit2)

fit2 = glm(y.yes ~ balance + day + duration + campaign + job.student + 
                   job.retired +  job.admin. + job.technician + 
                   job.management + marital.married + education.primary + 
                   education.tertiary + housing.yes + loan.no + contact.unknown + 
                   month.mar + month.jan + month.feb + month.apr + 
                   month.nov + month.jun + month.aug + month.jul + month.may + 
                   poutcome.other + poutcome.failure + poutcome.unknown, data = D_train,family = "binomial")
summary(fit2)

fit2 = glm(y.yes ~ balance + day + duration + campaign + job.student + 
                   job.retired +  job.admin. + job.management + marital.married + 
                   education.primary + 
                   education.tertiary + housing.yes + loan.no + contact.unknown + 
                   month.mar + month.jan + month.feb + month.apr + 
                   month.nov + month.jun + month.aug + month.jul + month.may + 
                   poutcome.other + poutcome.failure + poutcome.unknown, data = D_train,family = "binomial")
summary(fit2)

fit2 = glm(y.yes ~ balance + day + duration + campaign + job.student + 
                   job.retired +  job.admin. + marital.married + 
                   education.primary + 
                   education.tertiary + housing.yes + loan.no + contact.unknown + 
                   month.mar + month.jan + month.feb + month.apr + 
                   month.nov + month.jun + month.aug + month.jul + month.may + 
                   poutcome.other + poutcome.failure + poutcome.unknown, data = D_train,family = "binomial")
summary(fit2)

D_train$score = predict(fit2, newdata = D_train, type = "response")


#PLOT  GRAPHICALLY
library(ggplot2)
ggplot(D_train,aes(y=y.yes,x=score,color=factor(y.yes)))+geom_point()+geom_jitter()



#Question 4
# particular Cutoff
#Assuming an arbitrary Cutoff to be 0.2
# finding cutoff based on KS from train data

cutoff=0.2
predicted=as.numeric(D_train$score>cutoff)
TP=sum(predicted==1 & D_train$y.yes== 1)
FP=sum(predicted==1 & D_train$y.yes== 0)
TN=sum(predicted==0 & D_train$y.yes== 1)
FN=sum(predicted==0 & D_train$y.yes== 1)

#-----------------------------------------------------------------------------------------------------
# calculate total number of real positives and negatives in the data
P=TP+FN
N=TN+FP
#-----------------------------------------------------------------------------------------------------
# Total number of observations
Total <- P+N
#-----------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#Getting optimal cutoff for KS

cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0)
cutoffs=seq(0,1,length=100)
for( cutoff in cutoffs){
        predicted=as.numeric(D_train$score>cutoff)
        TP=sum(predicted==1 & D_train$y.yes== 1)
        FP=sum(predicted==1 & D_train$y.yes== 0)
        TN=sum(predicted==0 & D_train$y.yes== 1)
        FN=sum(predicted==0 & D_train$y.yes== 1)
        cutoff_data = rbind(cutoff_data,c(cutoff,TP,FP,FN,TN))
}

cutoff_data=cutoff_data[-1,]

cutoff_data=cutoff_data%>%
        mutate(P=TP+FN,N=TN+FP,
        SN=TP/P,
        SP=TN/N,
        KS=(SN - (FP/N)),
        accuracy=(TP+TN)/(P+N))%>%
        select(-P,-N)
        

library(tidyr)
cutoff_viz =cutoff_data%>%
        select(cutoff,SN,SP,accuracy,KS)%>%
        gather(Criterian,Value,SN,SP,KS,accuracy ) 

        ggplot(cutoff_viz,aes(x=cutoff,y=Value,color=Criterian))+geom_line()
#-----------------------------------------------------------------------------------------------------
        
#-----------------------------------------------------------------------------------------------------
test$score=predict(fit2,newdata=test,type="response")
        
 #-----------------------------------------------------------------------------------------------------
cutoff_KS=cutoff_data$cutoff[which.max(cutoff_data$KS)][1]
        cutoff_KS
        # KS 0.5252525
        cutoff_accuracy=cutoff_data$cutoff[which.max(cutoff_data$accuracy)][1]
        cutoff_accuracy
        #accuracy  0.3939394
        cutoff_SN=cutoff_data$cutoff[which.max(cutoff_data$SN)][1]
        cutoff_SN
        #SN 0
        cutoff_SP=cutoff_data$cutoff[which.max(cutoff_data$SP)][1]
        cutoff_SP
        # SP 1
#Performance on Test Data
#-----------------------------------------------------------------------------------------------------
# Confusion Matrix for KS cutoff
table(test$y.yes,as.numeric(test$score>cutoff_KS))
 #       0     1
 #       0 11663   299
 #       1  1061   541
        
 # Confusion Matrix for accuracy cutoff
table(test$y.yes,as.numeric(test$score>cutoff_accuracy))
#     0     1
#  0 11523   439
#  1   860   742

# Confusion Matrix for specificity cutoff
table(test$y.yes,as.numeric(test$score>cutoff_SP))
#       0
# 0 11962
# 1  1602

# Confusion Matrix for sensitivity cutoff

table(test$y.yes,as.numeric(test$score>cutoff_SN))
        
#       1
#   0 11962
#   1  1602
        
        
        
#-----------------------------------------------------------------------------------------------------
#AUC Score 
        
library(pROC)
roccurve = roc(D_train$y.yes ~ D_train$score)
plot(roccurve)
auc(roccurve)
#Area under the curve: 0.9075

library(InformationValue)

plotROC(D_train$y.yes ,D_train$score, returnSensitivityMat = TRUE)
#AUROC 0.9062
ks.test(D_train$y.yes ,D_train$score)

#	Two-sample Kolmogorov-Smirnov test

#   data:  D_train$y.yes and D_train$score
#   D = 0.8835, p-value < 2.2e-16
#   alternative hypothesis: two-sided

ks_plot(actuals = D_train$y.yes, predictedScores = D_train$score)

#-------------------------------------------------------------------------------------
#Question 5

#Random forest
library(randomForest)
D_train$y.yes=as.factor(D_train$y.yes)
#Create the forest.
rf=randomForest(y.yes ~.,data=D_train,do.trace = T) #it will build 500 trees by default
rf

#predicting on test data
test.rf=predict(rf,newdata=test)
table(test$y.yes,test.rf)

###  Performance Random forest
library(caret)
table(test$y.yes,test.rf)

varimp <- importance(rf) 
varImpPlot(rf) 
varImp(rf, scale = FALSE)
sort(varimp,decreasing = TRUE)[1:20]
 
#---------------------------------------------------------
#question no 6

varimp <- importance(rf) 
varImpPlot(rf, type = 2)
sort(varimp,decreasing = TRUE)[1:20]
importance(rf, type=1, scale=TRUE)
 varImp(rf, scale = FALSE)
head(varimp)
library(rpart)
fitt=rpart(factor(y.yes)~., D_train)
plot(fitt)
text(fitt)

sorted <- rf$importance
sort(sorted,decreasing = TRUE)[2:7]

#----------
# variable importance based on Generalized cross validation using Earh library
selected <- data.frame(D_train$duration,D_train$balance,D_train$age,D_train$day,D_train$pdays,D_train$campaign,D_train$y.yes)
View(selected)


selected$D_train.y.yes=as.numeric(selected$D_train.y.yes==1)

str(selected)
dd.new = selected
#split into 70% and  30% in train and test data
set.seed(12345)
sam=sample(1:nrow(dd.new),0.70*nrow(dd.new))
dd_train=dd.new[sam,]
dd_test=dd.new[-sam,]


summary(dd_train)
summary(dd_test)
describe(dd_test)
describe(dd_train)
### corelation test using H2o librry
#describe the data & data prepration
##vif check > 5
#Finding Correlation

cor(dd_train)

fit_1=lm(D_train.y.yes~., data = dd_train)
summary(fit_1)
sort(vif(fit_1),decreasing = TRUE)[1:3]

# Buiding a logistic regression model

fit_2=glm(D_train.y.yes~.,family = "binomial", data=dd_train)

summary(fit_2)
fit1=step(fit_2)
summary(fit_2)
formula(fit_2)

fit_3 = glm(D_train.y.yes ~ D_train.duration + D_train.balance + D_train.age + 
                    D_train.day + D_train.pdays + D_train.campaign, data = dd_train,family = "binomial")

summary(fit_3)

fit_3 = glm(D_train.y.yes ~ D_train.duration + D_train.balance + D_train.age +
                    D_train.pdays + D_train.campaign, data = dd_train,family = "binomial")

summary(fit_3)

dd_train$score = predict(fit_3, newdata = dd_train, type = "response")


#PLOT  GRAPHICALLY
library(ggplot2)
ggplot(dd_train,aes(y=D_train.y.yes,x=score,color=factor(D_train.y.yes)))+geom_point()+geom_jitter()


# particular Cutoff
#Assuming an arbitrary Cutoff to be 0.2
# finding cutoff based on KS from train data
new <- dd_train$D_train.y.yes


cutoff1=0.2
predicted1=as.numeric(dd_train$score>cutoff1)
TP=sum(predicted1==1 & new== 1)
FP=sum(predicted1==1 & new== 0)
TN=sum(predicted1==0 & new== 1)
FN=sum(predicted1==0 & new== 1)

#-----------------------------------------------------------------------------------------------------
# calculate total number of real positives and negatives in the data
P=TP+FN
N=TN+FP
#-----------------------------------------------------------------------------------------------------
# Total number of observations
Total <- P+N
#-----------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#Getting optimal cutoff for KS

cutoff_data=data.frame(cutoff1=0,TP=0,FP=0,FN=0,TN=0)
cutoffs=seq(0,1,length=1000)
for( cutoff1 in cutoffs){
        predicted1=as.numeric(dd_train$score>cutoff1)
        TP=sum(predicted1==1 & new== 1)
        FP=sum(predicted1==1 & new== 0)
        TN=sum(predicted1==0 & new== 1)
        FN=sum(predicted1==0 & new== 1)
        cutoff_data = rbind(cutoff_data,c(cutoff1,TP,FP,FN,TN))
}

cutoff_data=cutoff_data[-1,]

cutoff_data=cutoff_data%>%
        mutate(P=TP+FN,N=TN+FP,
               SN=TP/P,
               SP=TN/N,
               KS=(SN - (FP/N)),
               accuracy=(TP+TN)/(P+N))%>%
        select(-P,-N)


library(tidyr)
cutoff_viz =cutoff_data%>%
        select(cutoff,SN,SP,accuracy,KS)%>%
        gather(Criterian,Value,SN,SP,KS,accuracy ) 

ggplot(cutoff_viz,aes(x=cutoff1,y=Value,color=Criterian))+geom_line()
#-----------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------
dd_test$score=predict(fit_3,newdata=dd_test,type="response")

#-----------------------------------------------------------------------------------------------------
cutoff_KS1=cutoff_data$cutoff[which.max(cutoff_data$KS)][1]
cutoff_KS
# KS 0.5252525
cutoff_accuracy=cutoff_data$cutoff[which.max(cutoff_data$accuracy)][1]
cutoff_accuracy
#accuracy  0.4244244
cutoff_SN=cutoff_data$cutoff[which.max(cutoff_data$SN)][1]
cutoff_SN
#SN 0
cutoff_SP=cutoff_data$cutoff[which.max(cutoff_data$SP)][1]
cutoff_SP
# SP 1
#Performance on Test Data
#-----------------------------------------------------------------------------------------------------
# Confusion Matrix for KS cutoff
table(dd_test$D_train.y.yes,as.numeric(dd_test$score>cutoff_KS))
#           0    1
#       0 8330  120
#       1  870  175

# Confusion Matrix for accuracy cutoff
table(dd_test$D_train.y.yes,as.numeric(dd_test$score>cutoff_accuracy))
#       0     1
#  0  8261   189
#  1   816   229

# Confusion Matrix for specificity cutoff
table(dd_test$D_train.y.yes,as.numeric(dd_test$score>cutoff_SP))
#       0
# 0 8450
# 1  1045

# Confusion Matrix for sensitivity cutoff

table(dd_test$D_train.y.yes,as.numeric(dd_test$score>cutoff_SN))

#       1
#   0 8450
#   1  1045



#-----------------------------------------------------------------------------------------------------
#AUC Score 

library(pROC)
roc_curve = roc(dd_train$D_train.y.yes ~ dd_train$score)
plot(roc_curve)
auc(roc_curve)
Area under the curve: 0.8269

library(InformationValue)

plotROC(dd_train$D_train.y.yes ,dd_train$score, returnSensitivityMat = TRUE)
#AUROC  0.8235

ks.test(dd_train$D_train.y.yes ,dd_train$score)
#Two-sample Kolmogorov-Smirnov test
# data:  dd_train$D_train.y.yes and dd_train$score
# D = 0.88073, p-value < 2.2e-16
# alternative hypothesis: two-sided

ks_plot(actuals = dd_train$D_train.y.yes, predictedScores = dd_train$score)
----------------
# Earlier Logistic Regression model =  Area under the curve: 0.9075 and AUROC is 0.9062 and
        
detach("InformationValue")  # detach due to pROC library doesnot work when InformationValue library loaded
library(pROC)
auc(roccurve)
#Area under the curve: 0.9075


library(InformationValue)
plotROC(D_train$y.yes ,D_train$score, returnSensitivityMat = TRUE)
#AUROC 0.9062
#-----------------------------------------------------------------------------------------
## Now after Random Forest Logistic Regression model =  Area under the curve: 0.8269 and AUROC is 0.8235 

auc(roc_curve)
plotROC(new ,dd_train$score, returnSensitivityMat = TRUE)




