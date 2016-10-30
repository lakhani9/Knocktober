#setwd("/home/saurav/Knocktober")

#load required libraries
library('data.table')
library('lubridate')
library('caret')


#load data
train <- fread("Train.csv", na.strings = c("NA"," ",NA))
test <- fread("Test_D7W1juQ.csv",na.strings = c("NA"," ",NA))
camp_detail <- fread("Health_Camp_Detail.csv",na.strings = c("NA"," ",NA))
patient_profile <- fread("Patient_Profile.csv",na.strings = c("NA"," ",NA))
attended_health_camp_1 <- fread("First_Health_Camp_Attended.csv",na.strings = c("NA"," ",NA))
attended_health_camp_2 <- fread("Second_Health_Camp_Attended.csv",na.strings = c("NA"," ",NA))
attended_health_camp_3 <- fread("Third_Health_Camp_Attended.csv",na.strings = c("NA"," ",NA))


#create train and test data based on information given

train <- camp_detail[train,on="Health_Camp_ID"]
test <- camp_detail[test,on="Health_Camp_ID"]

train <- patient_profile[train,on="Patient_ID"]
test <- patient_profile[test,on="Patient_ID"]

#prepare to create and add target variable in train file
attended_health_camp_1[,Target := 1L]
attended_health_camp_2[,Target := 1L]
attended_health_camp_3[Last_Stall_Visited_Number != 0, Target := 1L]
attended_health_camp_1 <- attended_health_camp_1[,.(Health_Camp_ID,Patient_ID,Target)]
attended_health_camp_2 <- attended_health_camp_2[,.(Health_Camp_ID,Patient_ID,Target)]
attended_health_camp_3 <- attended_health_camp_3[,.(Health_Camp_ID,Patient_ID,Target)]

attended_all <- rbindlist(list(attended_health_camp_1,attended_health_camp_2,attended_health_camp_3))

train <- attended_all[train,on=c("Health_Camp_ID","Patient_ID")]
train$Target[is.na(train$Target)] <- 0

#check count of 1 and 0
table(train$Target) #train data ready

# Set Date class ---------------------------------------------------------------

index <- c(11,14,15,19)
train[,(index) := lapply(.SD,function(x) as.Date(x,format="%d-%b-%y")),.SDcols = index]
index <- c(9,13,14,18)
test[,(index) := lapply(.SD, function(x) as.Date(x,format="%d-%b-%y")),.SDcols = index]

# write.csv(train,"trainf.csv",row.names = F)
# write.csv(test,"testf.csv",row.names = F)

#Simple Features + Modeling

#Test Age
test[Age == "30",Age := 31]
sort(unique(test$Age),decreasing = T)
sort(unique(train$Age),decreasing = T)

#creating variables
train[,Camp_Duration := Camp_End_Date - Camp_Start_Date]
train[,Camp_Duration := as.numeric(as.character(Camp_Duration))]

test[,Camp_Duration := Camp_End_Date - Camp_Start_Date]
test[,Camp_Duration := as.numeric(as.character(Camp_Duration))]


train[,Patient_Response := Registration_Date - First_Interaction]
train[,Patient_Response := as.numeric(as.character(Patient_Response))]

test[,Patient_Response := Registration_Date - First_Interaction]
test[,Patient_Response := as.numeric(as.character(Patient_Response))]


train<-as.data.frame(train)
test<-as.data.frame(test)

train$Reg_After<-as.numeric(difftime(train$Registration_Date ,train$First_Interaction , units = c("days")))
test$Reg_After<-as.numeric(difftime(test$Registration_Date ,test$First_Interaction , units = c("days")))

train$Reg_After[is.na(train$Reg_After)]<-median(train$Reg_After,na.rm = T)

train$Income<-as.factor(train$Income)
test$Income<-as.factor(test$Income)

train$Education_Score<-as.factor(train$Education_Score)
test$Education_Score<-as.factor(test$Education_Score)

train$Age<-as.factor(train$Age)
test$Age<-as.factor(test$Age)

train$City_Type<-as.factor(train$City_Type)
test$City_Type<-as.factor(test$City_Type)

train$Category1<-as.factor(train$Category1)
test$Category1<-as.factor(test$Category1)

train$Category2<-as.factor(train$Category2)
test$Category2<-as.factor(test$Category2)

train$Category3<-as.factor(train$Category3)
test$Category3<-as.factor(test$Category3)

train$FI_weekday<-as.factor(weekdays(train$First_Interaction))
train$FI_day<-as.factor(strftime(train$First_Interaction, "%d"))
train$FI_month<-as.factor(strftime(train$First_Interaction, "%m"))
train$FI_year<-as.numeric(strftime(train$First_Interaction, "%y"))

test$FI_weekday<-as.factor(weekdays(test$First_Interaction))
test$FI_day<-as.factor(strftime(test$First_Interaction, "%d"))
test$FI_month<-as.factor(strftime(test$First_Interaction, "%m"))
test$FI_year<-as.numeric(strftime(test$First_Interaction, "%y"))


train$CS_weekday<-as.factor(weekdays(train$Camp_Start_Date))
train$CS_day<-as.factor(strftime(train$Camp_Start_Date, "%d"))
train$CS_month<-as.factor(strftime(train$Camp_Start_Date, "%m"))
train$CS_year<-as.numeric(strftime(train$Camp_Start_Date, "%y"))



test$CS_weekday<-as.factor(weekdays(test$Camp_Start_Date))
test$CS_day<-as.factor(strftime(test$Camp_Start_Date, "%d"))
test$CS_month<-as.factor(strftime(test$Camp_Start_Date, "%m"))
test$CS_year<-as.numeric(strftime(test$Camp_Start_Date, "%y"))

train$CE_weekday<-as.factor(weekdays(train$Camp_End_Date))
train$CE_day<-as.factor(strftime(train$Camp_End_Date, "%d"))
train$CE_month<-as.factor(strftime(train$Camp_End_Date, "%m"))
train$CE_year<-as.numeric(strftime(train$Camp_End_Date, "%y"))



test$CE_weekday<-as.factor(weekdays(test$Camp_End_Date))
test$CE_day<-as.factor(strftime(test$Camp_End_Date, "%d"))
test$CE_month<-as.factor(strftime(test$Camp_End_Date, "%m"))
test$CE_year<-as.numeric(strftime(test$Camp_End_Date, "%y"))

train$Reg_to_camp_start<- as.numeric(difftime(train$Registration_Date ,train$Camp_Start_Date , units = c("days")))
test$Reg_to_camp_start<- as.numeric(difftime(test$Registration_Date ,test$Camp_Start_Date , units = c("days")))

train$Reg_to_camp_end<- as.numeric(difftime(train$Camp_End_Date ,train$Registration_Date , units = c("days")))
test$Reg_to_camp_end<- as.numeric(difftime(test$Camp_End_Date ,test$Registration_Date , units = c("days")))

train$Int_to_camp_start<- as.numeric(difftime(train$Camp_Start_Date ,train$First_Interaction , units = c("days")))
test$Int_to_camp_start<- as.numeric(difftime(test$Camp_Start_Date ,test$First_Interaction , units = c("days")))

library(VIM)
aggr_plot <- aggr(train, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

train$Patient_Response[is.na(train$Patient_Response)]<-median(train$Patient_Response,na.rm=T)

train$Reg_to_camp_start[is.na(train$Reg_to_camp_start)]<-median(train$Reg_to_camp_start,na.rm=T)
train$Reg_to_camp_end[is.na(train$Reg_to_camp_end)]<-median(train$Reg_to_camp_end,na.rm=T)

test$Patient_Response[is.na(test$Patient_Response)]<-median(test$Patient_Response,na.rm=T)

test$Reg_to_camp_start[is.na(test$Reg_to_camp_start)]<-median(test$Reg_to_camp_start,na.rm=T)
test$Reg_to_camp_end[is.na(test$Reg_to_camp_end)]<-median(test$Reg_to_camp_end,na.rm=T)


train$Employer_Category<-as.factor(train$Employer_Category)
test$Employer_Category<-as.factor(test$Employer_Category)

train$Target<-as.factor(train$Target)


test$Target<--1
all<-rbind(train,test)



put <- copy(all)
setDT(all)

all[,count := .N,Patient_ID]
all[,cd := cumsum(count),Patient_ID]
all[,cd1 := cd/count]

all<-as.data.frame(all)
all$cd<-NULL


all$social_media<-all$LinkedIn_Shared+all$Twitter_Shared+all$Facebook_Shared+all$Online_Follower

all$Reg_to_camp_start2<-abs(all$Reg_to_camp_start)
all$Reg_to_camp_start3<-ifelse((all$Reg_to_camp_start)<0,0,all$Reg_to_camp_start)

all$Int_to_camp_start2<-abs(all$Int_to_camp_start)
all$Int_to_camp_start3<-ifelse((all$Int_to_camp_start)<0,0,all$Int_to_camp_start)


train<-all[1:75278,]
test<-all[75279:110527,]

a<-intersect(train$Patient_ID,test$Patient_ID)
train$prop<-0
train$prop[train$Patient_ID %in% a]<-1
test$prop<-0
test$prop[test$Patient_ID %in% a]<-1




train$Education_Score<-NULL
train$Age<-as.integer(train$Age)
train$First_Interaction<-NULL
train$Camp_Start_Date<-NULL
train$Camp_End_Date<-NULL
train$Registration_Date<-NULL
train$Income<-as.numeric(train$Income)
train$Category3<-as.numeric(train$Category3)
train$FI_day<-as.integer(train$FI_day)
train$FI_month<-as.integer(train$FI_month)
train$CS_day<-as.integer(train$CS_day)
train$CS_month<-as.integer(train$CS_month)
train$CE_day<-as.integer(train$CE_day)
train$CE_month<-as.integer(train$CE_month)




test$Education_Score<-NULL
test$Age<-as.integer(test$Age)
test$First_Interaction<-NULL
test$Camp_Start_Date<-NULL
test$Camp_End_Date<-NULL
test$Registration_Date<-NULL
test$Income<-as.numeric(test$Income)
test$Category3<-as.numeric(test$Category3)
test$FI_day<-as.integer(test$FI_day)
test$FI_month<-as.integer(test$FI_month)
test$CS_day<-as.integer(test$CS_day)
test$CS_month<-as.integer(test$CS_month)
test$CE_day<-as.integer(test$CE_day)
test$CE_month<-as.integer(test$CE_month)





a<-intersect(train$Patient_ID,test$Patient_ID)
train$prop<-0
train$prop[train$Patient_ID %in% a]<-1
test$prop<-0
test$prop[test$Patient_ID %in% a]<-1





train$Camp_Duration_by_mean<-train$Camp_Duration/771
test$Camp_Duration_by_mean<-test$Camp_Duration/209


outcomeName <- c('Target')

preds_best<-c('Category2',
              'Category1',
              'Reg_to_camp_start3',
              'CE_day',
              'count',
              'Education_Score',
              'Age',
              'CS_day',
              'FI_day',
              'Income',
              'CE_weekday',
              'CS_month',
              'Var1',
              'FI_month',
              'Var5',
              'CS_weekday',
              'City_Type',
              'varsum',
              'Camp_Duration_by_mean')

model_gbm_best<-train(train[,preds_best],train[,outcomeName],method='gbm')

predictions_best<-predict(object = model_gbm_best,test[,preds_best],type = "prob")
submission_best<-test[,c("Patient_ID","Health_Camp_ID")]
submission_best$Outcome = predictions_best$`1`
write.csv(submission_best,'submission_best.csv',row.names = FALSE)
#	0.813






#For treating Class Imbalance
library('ROSE')



no_ROSE<-c('Health_Camp_ID','Patient_ID','First_Interaction','Camp_Start_Date','Camp_End_Date','Registration_Date','Int_to_camp_start','Reg_to_camp_start','Int_to_camp_start2','Reg_to_camp_start2')



train[,no_ROSE]<-NULL

rose.train <- ROSE(Target ~ ., data = train, seed = 1)$data
table(rose.train$Target)
0     1 
37497 37781 

table(train$Target)

0     1 
54744 20534 


rose_predictors <- names(rose.train)[!names(rose.train) %in% outcomeName]

rose.model_gbm<-train(rose.train[,rose_predictors],rose.train[,outcomeName],method='gbm')




rose.predictions<-predict(object = rose.model_gbm,test[,rose_predictors],type = "prob")
submission<-test[,c("Patient_ID","Health_Camp_ID")]
submission$Outcome = rose.predictions$`1`
#write.csv(submission,'rose_gbm.csv',row.names = FALSE)
#	0.79369
#Didn't helped.





ensemble<-function(training,testing,outcomeName,BaseModels,TopModel)
{
  
  
  
  predictors <- names(training)[!names(training) %in% outcomeName]
  
  
  for(i in 1:length(BaseModels))
  {
    
    model <- caret::train(training[,predictors], training[,outcomeName], method=BaseModels[i])
    
    
    testing[,(ncol(testing)+1)] <- caret::predict.train(object=model, testing[,predictors])
    
    
    
    training[,(ncol(training)+1)] <- caret::predict.train(object=model, training[,predictors],type = "prob")
    
  }
  
  
  
  
  predictors <- names(training)[!names(training) %in% outcomeName]
  
  model_final <- caret::train(training[,predictors], training[,outcomeName], method=TopModel)
  
  predictions <- caret::predict.train(object=model_final, testing[,predictors],type = "prob")
  
  return(predictions)
  
}


outcomeName <- c('Target')
no<-c('Health_Camp_ID','Patient_ID','First_Interaction','Camp_Start_Date','Camp_End_Date','Registration_Date','Education_Score','Target')
predictors <- names(train)[!names(train) %in% no]

#predictions<-ensemble(train[,predictors],test[,predictors],"Target",c('rf','gbm','adaboost','DeepBoost','LogitBoost','nnet'),'gbm')

gbm_model<-train(train[,predictors],train[,outcomeName],method='gbm')

rf_model<-train(train[,predictors],train[,outcomeName],method='rf')

adaboost_model<-train(train[,predictors],train[,outcomeName],method='adaboost')

DeepBoost_model<-train(train[,predictors],train[,outcomeName],method='DeepBoost')

LogitBoost_model<-train(train[,predictors],train[,outcomeName],method='LogitBoost')

nnet_model<-train(train[,predictors],train[,outcomeName],method='nnet')


predictions_gbm<-predict(object = gbm_model,test[,predictors],type = "prob")
predictions_rf<-predict(object = rf_model,test[,predictors],type = "prob")
predictions_adaboost<-predict(object = adaboost_model,test[,predictors],type = "prob")
predictions_DeepBoost<-predict(object = DeepBoost_model,test[,predictors],type = "prob")
predictions_LogitBoost<-predict(object = LogitBoost_model,test[,predictors],type = "prob")
predictions_nnet<-predict(object = nnet_model,test[,predictors],type = "prob")

preds<-data.frame(gbm= numeric(0),rf= numeric(0),adaBoost= numeric(0),DeepBoost= numeric(0),LogitBoost= numeric(0),nnet= numeric(0))

preds$gbm=predictions_gbm
preds$rf=predictions_rf
preds$adaboost=predictions_adaboost
preds$DeepBoost=predictions_DeepBoost
preds$LogitBoost=predictions_LogitBoost
preds$nnet=predictions_nnet

f_predictions1<-(preds$gbm+preds$rf+preds$adaboost+preds$DeepBoost+preds$LogitBoost+preds$nnet)/6


#Couldn't submit this ensemble.Ran Out of time:(
