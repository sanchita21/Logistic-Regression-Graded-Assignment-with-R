
goodforu<-read.csv("goodforu-class12.csv")
View(goodforu)
library(dplyr)
brandA<-goodforu%>%select(X2,X9,X16,X30,X23)
View(brandA)
brandA$target<-ifelse(brandA$X23>5,1,0)
View(brandA)
#let's check the data for any anamoly and fix it if any
colSums(is.na(brandA))
str(brandA)
#we dont need X23 now for analysis ,so we can remove it 
brandA<-brandA%>%select(-X23)


#let's sample the data
View(brandA)
set.seed(200)
index<-sample(nrow(brandA),nrow(brandA)*0.70,replace = F)
#let's create train and validation data sets
train<-brandA[index,]
test<-brandA[-index,]
#lets's check the dataset
View(train)
#let's build the model taking target as the dependent variable and all others as independent variable
model<-glm(target~.,data = train,family = "binomial")
summary(model)
#in this model all varibales are coming to be significant so we can proceed furthier 
#to validate the model

#validation of the model
#kepa and confusion matrix
pred<-predict(model,type = "response",newdata = test)
head(pred)
View(pred)
View(test)
#let's check the rate of 1,according to that we will set a cutoff value
table(train$target)/nrow(train)
#so we can assume anything with probability of 1 greater than 0.2507401 will be 1 else 0,
#where 1 is good and 0 is bad
pred<-ifelse(pred>0.2507401,1,0)
#now run kappa matrix 
kappa2(data.frame(test$target,pred))
#confusion matrix
test$result<-ifelse(pred>0.2527401 ,1,0)
library(caret)
confusionMatrix(test$result,test$target,positive = "1")
#My accuracy is coming to be 69.58% and my kappa is coming to be 0.3394

#we can further optimize our model with chossing different cutoff,we will choose a cutoff 
#for which kappa should be maximum so that our accuracy will also be maximum
s<-seq(0.25,0.5,0.01)
n<-1 
a<-as.vector(length(s))
for (i in s ) {
  
  print(i)
  test$result<-ifelse(pred>i,1,0)
  a[n]<-confusionMatrix(test$result,test$target,positive = "1")$overall[2]
  
  print(n)
  n=n+1
}
#now a has all the different kappas for different cutoff stored in it
#extracting the max kappa
index<-which(a==max(a))
#so the cutoff value w.r.t the max kappa s[index]
s[index]
#we can proceed further with the model with cutoff value 0.39 as with it we are getting max
#Kappa 
test$result<-ifelse(pred>0.39,1,0)
confusionMatrix(test$result,test$target,positive = "1")
#Accuracy : 0.7689   for my optimised model

#let's check wiht the confidence interval 
confint(model)
#seems fine to me it's not that wide
View(test)

#now Preaparing the Gain_Table
library(dplyr)
tab<-data.frame(test$target,Probability=predict(model,type="response",newdata = test))
View(tab)


tab<-tab[order(-tab$Probability),]
View(tab)
tab$pred<-ifelse(tab$Probability>0.39,1,0)
View(tab)
tab<-tab%>%mutate(quant=ntile(tab$Probability,10))
#tab%>%select(-quantile)->tab
View(tab)
tab%>%filter(test.target==pred)%>%group_by(quant)%>%summarise(response=n())->response
tab%>%group_by(quant)%>%summarise(numebr_of_observatio=n())->numebr_of_cases
gain_table<-cbind(numebr_of_cases,response)
View(gain_table)
#remove duplicate column
gain_table <- gain_table[, !duplicated(colnames(gain_table))]
View(gain_table)
gain_table%>%mutate(cumlateive_response=cumsum(response))->gain_table
gain_table%>%mutate(Pecentage_of_events=(response/sum(response))*100,gain=cumsum(Pecentage_of_events))->gain_table
View(gain_table)
gain_table%>%mutate(cumulative_lift=gain/(quant*10))->gain_table

#gain_chart

library(ggplot2)

View(gain_table)
a<-ggplot(gain_table,aes(x=quant*10))+geom_line(aes(y=gain,linetype="% of cumulative events(model)",colour="% of cumulative events(model)"))+geom_line(aes(y=quant*10,linetype="% of cumulative events(random)",colour="% of cumulative events(random)"))
b<-a+scale_x_continuous(breaks=seq(0,100,10),expand = c(0,0),limits = c(0,100))+scale_y_continuous(breaks=seq(0,100,10),expand = c(0,0),limits = c(0,100))
c<-b+xlab("%of data sets")+ylab("% of events")+guides(linetype=F)+geom_point(aes(y=gain),colour="red")+geom_point(aes(y=quant*10),color="blue")
c+ggtitle("Gains Chart")
#lift Chart
p<-ggplot(gain_table,aes(x=quant*10))+geom_line(aes(y=cumulative_lift,linetype="Lift(Model)", colour="Lift(Model)"))+geom_line(aes(y=rep(1,10),linetype="lift(random)", colour="lift(random)"))
q<-p+scale_x_continuous(breaks=seq(0,100,10),expand = c(0,0),limits = c(0,100))+scale_y_continuous(breaks = seq(0,1.5,0.3),expand = c(0,0),limits = c(0,1.5))
r<-q+xlab("%of data sets")+ylab("lift")+theme_bw()+geom_point(aes(y=cumulative_lift),colour="red")+geom_point(aes(y=rep(1,10)),colour="green")
s<-r+scale_colour_manual(name="linetype", values = c("Lift(Model)" = "red", "lift(random)" = "green"))+guides(linetype=F)
s+ggtitle("Lift Chart")


#Equation For the model is log(p/1-p)=-1.43972 +( -0.38741)*X2+(-0.34811)*X9 +(-0.43805 )*X16+ 0.42569*X30
#From the model we can infer that except from the processing level all other have -ve impact 
# farm grown ingredients has a -ve impact of 0.38741 on the Brand value
# Have zero grams trans fat has -ve impact of 0.34811 on the brand value
# Are made with natural oils has -ve impact of 0.43805 on the brand value
#Processing level has a positve impact of 0.42569 on the brand value