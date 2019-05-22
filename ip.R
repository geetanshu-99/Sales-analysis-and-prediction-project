#load packages and Data
library(data.table)#used for reading and manipulation of data
library(dplyr)     #used for data manipulation and joining
library(ggplot2) #used for ploting
library(lattice)
library(caret)     #used for modeling
library(corrplot)  #used for making correlation plot
#install.packages("cowplot")
library(cowplot)   #used for combining multiple plots

#reading data
data<-read.csv(file.choose())

#Understanding the Data

#Dimensions of Data
dim(data)
#Features of Data will take a quick glance over the feature names of dataaset
names(data)
#structure of data gives the short summary of features present in a dataframe
str(data)
View(data)
#EDA(Exploratory Data Analysis)
#Target Variable
ggplot(data)+geom_histogram(aes(data$Purchase),binwidth = 100,fill="darkgreen")+xlab("purchase")

#independent variable(numeric variables)
p1<-ggplot(data)+geom_histogram(aes(data$Occupation),binwidth=0.5,fill="blue")
p2<-ggplot(data)+geom_histogram(aes(data$Product_Category_1),binwidth=0.005,fill="blue")
p3<-ggplot(data)+geom_histogram(aes(data$Product_Category_2),binwidth=0.005,fill="blue")
p4<-ggplot(data)+geom_histogram(aes(data$Product_Category_3),binwidth=0.005,fill="blue")
plot_grid(p1,p2,p3,p4,ncol=1)#plot_grid() from cowplot package

#independent variables(categorical variables)
p5<-ggplot(data %>% group_by(Gender) %>% summarise(count=n()))+geom_bar(aes(Gender,count),stat="identity",fill="coral1")
p6<-ggplot(data %>% group_by(Age) %>% summarise(count=n()))+geom_bar(aes(Age,count),stat="identity",fill="coral1")
p7<-ggplot(data %>% group_by(City_Category) %>% summarise(count=n()))+geom_bar(aes(City_Category,count),stat="identity",fill="coral1")
p8<-ggplot(data %>% group_by(Stay_In_Current_City_Years) %>% summarise(count=n()))+geom_bar(aes(Stay_In_Current_City_Years,count),stat="identity",fill="coral1")
p9<-ggplot(data %>% group_by(Marital_Status) %>% summarise(count=n()))+geom_bar(aes(Marital_Status,count),stat="identity",fill="coral1")


#Bivariate Analysis
#Target variable vs Independent numeical varibles
#Purchase vs occupation
p10<-ggplot(data)+geom_point(aes(data$Occupation,data$Purchase),colour="violet",alpha=0.3)+theme(axis.title=element_text(size = 8.5))
#Purchase Vs Product_Category_1
p11<-ggplot(data)+geom_point(aes(data$Product_Category_1,data$Purchase),colour="violet",alpha=0.3)+theme(axis.title=element_text(size = 8.5))
#Purchase Vs Prooduct_Category_2
p12<-ggplot(data)+geom_point(aes(data$Product_Category_2,data$Purchase),colour="violet",alpha=0.3)+theme(axis.title=element_text(size = 8.5))
#Purchase Vs Product_Category_3
p13<-ggplot(data)+geom_point(aes(data$Product_Category_3,data$Purchase),colour="violet",alpha=0.3)+theme(axis.title=element_text(size = 8.5))


#Target Variable Vs Independent Categorical Variables

p14<-ggplot(data)+geom_violin(aes(Gender,Purchase),fill="magenta")+ theme(axis.text.x = element_text(angle=45,hjust=1),axis.text = element_text(size = 6),axis.title = element_text(size = 8.5))
p15<-ggplot(data)+geom_violin(aes(Age,Purchase),fill="magenta")+ theme(axis.text.x = element_text(angle=45,hjust=1),axis.text = element_text(size = 6),axis.title = element_text(size = 8.5))
p16<-ggplot(data)+geom_violin(aes(City_Category,Purchase),fill="magenta")+ theme(axis.text.x = element_text(angle=45,hjust=1),axis.text = element_text(size = 6),axis.title = element_text(size = 8.5))
p17<-ggplot(data)+geom_violin(aes(Stay_In_Current_City_Years,Purchase),fill="magenta")+ theme(axis.text.x = element_text(angle=45,hjust=1),axis.text = element_text(size = 6),axis.title = element_text(size = 8.5))
p18<-ggplot(data)+geom_violin(aes(Marital_Status,Purchase),fill="magenta")+ theme(axis.text.x = element_text(angle=45,hjust=1),axis.text = element_text(size = 6),axis.title = element_text(size = 8.5))

#Data Cleaning and Preparation
#Encoding Categorical Variables
data$gender[data$Gender=="M"] <- 1 
data$gender[data$Gender=="F"] <- 2 


data$age[data$Age=="0-17"] <- 1 
data$age[data$Age=="18-25"] <- 2
data$age[data$Age=="26-35"] <- 3
data$age[data$Age=="36-45"] <- 4
data$age[data$Age=="46-50"] <- 5
data$age[data$Age=="51-55"] <- 6 
data$age[data$Age=="55+"] <- 7

data$city[data$City_Category=="A"] <- 1 
data$city[data$City_Category=="B"] <- 2
data$city[data$City_Category=="C"] <- 3 

data$stay_in_city[data$Stay_In_Current_City_Years=="4+"] <- 4 
data$stay_in_city[data$Stay_In_Current_City_Years=="0"] <- 0 
data$stay_in_city[data$Stay_In_Current_City_Years=="1"] <- 1 
data$stay_in_city[data$Stay_In_Current_City_Years=="2"] <- 2
data$stay_in_city[data$Stay_In_Current_City_Years=="3"] <- 3 
View(data)
str(data)
summary(data)

#Data PreProcessing
#Missing value
#install.packages("mice")
library(mice)
md.pattern(data)
#Treatment
library(Hmisc)
data$Imputed_Product_Category_2 <- impute(data$Product_Category_2, median)
data$Imputed_Product_Category_3 <- impute(data$Product_Category_3, median)
#data$Imputed_Purchase <- impute(data$Purchase, mean)
#Delete Initial columns
data$Product_Category_2 <- NULL
data$Product_Category_3 <- NULL
#data$Purchase <- NULL
data$Gender<- NULL
data$Age<- NULL
data$City_Category<- NULL
data$Stay_In_Current_City_Years<- NULL
View(data)


#data$Purchase<-data$Imputed_Purchase
#outlier treatment
stats<- function(x) {
  iqr=IQR(x,na.rm=T)
  q1<-quantile(x,0.25,na.rm=T)
  q2<-quantile(x,0.5,na.rm=T)
  q3<-quantile(x,0.75,na.rm=T)
  UC<-q3+1.5*iqr
  LC<-q1-1.5*iqr
  min<-min(x,na.rm=T)
  max<-max(x,na.rm=T)
  mean<-mean(x,na.rm=T)
  std<-sd(x,na.rm=T)
  return(c(q1=q1, q2=q2, q3=q3,UC=UC, LC=LC, min=min, max=max, mean=mean, std=std))
}

vars<- c( "gender", "age" , "Occupation", "city", "stay_in_city", "Marital_Status", "Product_Category_1", "Imputed_Product_Category_2","Imputed_Product_Category_3","Purchase")

data_stats<-t(data.frame(apply(data[vars],2,stats)))
View(data_stats)
#data$Imputed_Purchase<- NULL
View(data)
data$Product_Category_1[data$Product_Category_1>18.5]<-18.5
data$Imputed_Product_Category_3[data$Imputed_Product_Category_3>14]<-14
data$Purchase[data$Purchase>21400.5]<-21400.5





#Data cleaning is done,now we will again split data into train and test
#install.packages("caTools")
require(caTools)
set.seed(123)
sample_data<-sample.split(data,SplitRatio = 0.70)
train<-subset(data,sample_data==TRUE)
test<-subset(data,sample_data==FALSE)
nrow(train)
nrow(test)
nrow(data)
test[,Purchase:=NULL]
View(train)
str(train)
#sample practice
train1<-train
test1<-test
View(train1)
nrow(train1)
nrow(test1)
train1$User_ID<-NULL
train1$Product_ID<-NULL
test1$User_ID<-NULL
test1$Product_ID<-NULL
#test1[,Purchase:=NULL]
View(test1)
#Correlated Variables
cor_train=cor(train1)
corrplot(cor_train)
corrplot(cor_train,type="lower",tl.cex=0.0000000000001)


#Model Building and Evaluation
View(test)
linear_reg_mod1=lm(Purchase~.,data=train1)
summary(linear_reg_mod1)
require(MASS)
step<- stepAIC(linear_reg_mod1,direction="both")
linear_reg_mod2=lm(Purchase~Occupation+Marital_Status+Product_Category_1+gender+age+city+Imputed_Product_Category_2+Imputed_Product_Category_3,data=train1)
summary(linear_reg_mod2)
#Make predictions on test Data
submission=predict(linear_reg_mod2,test1)
my_solution=data.frame(User_ID=test$User_ID,Purchase=submission)
write.csv(my_solution,"linear_reg_submit3.csv",row.names = F)

#######################Validation by Prediction
options(scipen=999)
t1<-cbind(test1, pred_purchase=(predict(linear_reg_mod2,test1)))
t1<- transform(t1, APE = abs(pred_purchase - Purchase)/Purchase)

#MAPE(Error Rate)
mean(t1$APE)

