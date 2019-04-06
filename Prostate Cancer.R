getwd()
setwd("")
prostatedata<- read.csv("/Users/shubpuri/Downloads/Health care/Prostate Cancer/participant_files/training_data.csv")
#Remove ID
prosdata<-prostatedata
prosdata<- prosdata[-1]
sum(is.na(prosdata))
str(prosdata)
dim(prosdata)
summary(prosdata$gleason_score)

#Check Columnwise NA values 
na_count <-sapply(prosdata, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count
prosdata<- prosdata[-1]

#Creating new list for categorical and Numerical variables
prosdata_cat <- subset(prosdata, select=c('diagnosis_date', 'gleason_score','t_score','n_score','m_score','stage','race','height','family_history','first_degree_history',
                                          'previous_cancer','smoker','side','tea','symptoms','rd_thrpy','h_thrpy','chm_thrpy','cry_thrpy','brch_thrpy','rad_rem','multi_thrpy',
                                          'survival_1_year','survival_7_years'))

prosdata_num <- subset(prosdata, select = c('survival_7_years','age','weight','tumor_diagnosis','tumor_6_months','tumor_1_year','psa_diagnosis','psa_6_months','psa_1_year'))


#Chi-square test for all the  Categorical Variables with survival_7_years
chisq.test(prosdata$survival_7_years, prosdata$diagnosis_date)$p.value
chisq.test(prosdata$survival_7_years, prosdata$gleason_score)$p.value
chisq.test(prosdata$survival_7_years, prosdata$t_score)$p.value
chisq.test(prosdata$survival_7_years, prosdata$n_score)$p.value
chisq.test(prosdata$survival_7_years, prosdata$m_score)$p.value
chisq.test(prosdata$survival_7_years, prosdata$stage)$p.value
chisq.test(prosdata$survival_7_years, prosdata$race)$p.value
chisq.test(prosdata$survival_7_years, prosdata$height)$p.value
chisq.test(prosdata$survival_7_years, prosdata$family_history)$p.value
chisq.test(prosdata$survival_7_years, prosdata$first_degree_history)$p.value
chisq.test(prosdata$survival_7_years, prosdata$previous_cancer)$p.value
chisq.test(prosdata$survival_7_years, prosdata$smoker)$p.value
chisq.test(prosdata$survival_7_years, prosdata$side)$p.value
chisq.test(prosdata$survival_7_years, prosdata$tea)$p.value
chisq.test(prosdata$survival_7_years, prosdata$symptoms)$p.value
chisq.test(prosdata$survival_7_years, prosdata$rd_thrpy)$p.value
chisq.test(prosdata$survival_7_years, prosdata$h_thrpy)$p.value
chisq.test(prosdata$survival_7_years, prosdata$chm_thrpy)$p.value
chisq.test(prosdata$survival_7_years, prosdata$cry_thrpy)$p.value
chisq.test(prosdata$survival_7_years, prosdata$brch_thrpy)$p.value
chisq.test(prosdata$survival_7_years, prosdata$rad_rem)$p.value
chisq.test(prosdata$survival_7_years, prosdata$multi_thrpy)$p.value
chisq.test(prosdata$survival_7_years, prosdata$survival_1_year)$p.value

dim(prosdata)



#removing non-significant variables based on Chi-square test-
prosdata<- subset(prosdata, select = -c(diagnosis_date, height, family_history, first_degree_history, smoker, side, tea))
prosdata_cat<- subset(prosdata_cat, select = -c(diagnosis_date, height, family_history, first_degree_history, smoker, side, tea))



#Remove NULL values
#prosdata<- prosdata[!(is.na(prosdata$age)),]




#T-test for all the numerical variables
t.test(x= prosdata$age[prosdata$survival_7_years ==1], y= prosdata$age[prosdata$survival_7_years==0])
t.test(x= prosdata$weight[prosdata$survival_7_years ==1], y= prosdata$weight[prosdata$survival_7_years==0])
t.test(x= prosdata$tumor_1_year[prosdata$survival_7_years ==1], y= prosdata$tumor_1_year[prosdata$survival_7_years==0])
t.test(x= prosdata$psa_1_year[prosdata$survival_7_years ==1], y= prosdata$psa_1_year[prosdata$survival_7_years==0])
str(prosdata)

#remove variables based on T-test:
prosdata<- subset(prosdata, select = -c(age))
prosdata_num<- subset(prosdata_num, select = -c(age))

dim(prosdata)
str(prosdata)


#Correlation Matrix 
install.packages("corrplot")
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(prosdata_num)

prosdata_num<- subset(prosdata_num, select = -c(survival_7_years))

#Variables psa_diagnosis, psa_6_months, psa_1_year are highly correlated to each other
#so removing psa_diagnosis and psa_6_months from the dataset
prosdata<- subset(prosdata, select = -c(psa_diagnosis, psa_6_months))

#Variables tumor_diagnosis, tumor_6_months, tumor_1_year are highly correlated to each other
#Removing tumour_diagnosis, tumour_6_months
prosdata<- subset(prosdata, select = -c(tumor_diagnosis, tumor_6_months))

prosdata_num<- subset(prosdata_num, select = -c(psa_diagnosis, psa_6_months,tumor_diagnosis, tumor_6_months))



#Check Columnwise NA values 
na_count <-sapply(prosdata, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count



###Also removing symptoms from the dataset###

#Treating categorical variables
table(prosdata$gleason_score)
sum(is.na(prosdata$gleason_score))


#gleason_score - Remove 320 NA values
prosdata <- prosdata[!(is.na(prosdata$gleason_score)),]
#prosdata$gleason_score<- droplevels(prosdata$gleason_score)
#prosdata<- prosdata[!(is.na(prosdata$gleason_score)),]



#Now, Condense the data into categories-
#Category 1- 3-5
#Category 2- 6-8
#Category 3- 9,10,12
#Category 4- 13-14

prosdata$gleason_score<- ifelse(prosdata$gleason_score==3 | prosdata$gleason_score ==4 | prosdata$gleason_score ==5, 1, 
                                ifelse(prosdata$gleason_score==6 |prosdata$gleason_score==7 | prosdata$gleason_score==8, 2, 
                                       ifelse(prosdata$gleason_score==9 | prosdata$gleason_score==10 | prosdata$gleason_score==12 , 3, 4 )))


#### WE WILL CONVERT GLEASON SCORE INTO FACTOR LATER #######

#t_score- There are no NA values. We can condense the categories into T1.T2,T3,T4
table(prosdata$t_score)
sum(is.na(prosdata$t_score))
#Category 1- T1a,T1b,T1c
#Category 2- T2a, T2b, T2c
#Cateogory 3- T3a, T3b, T3c
#Category 4- T4
prosdata$t_score<- ifelse(prosdata$t_score=='T1a' | prosdata$t_score == 'T1b' | prosdata$t_score =='T1c', 1, 
                          ifelse(prosdata$t_score=='T2a' |prosdata$t_score=='T2b' | prosdata$t_score=='T2c', 2, 
                                 ifelse(prosdata$t_score=='T3a' | prosdata$t_score=='T3b' | prosdata$t_score=='T3c' , 3, 4 )))


sum(is.na(prosdata$t_score))
summary(prosdata)

#race- There are 161 NA values. We remove them
prosdata<- prosdata[!(is.na(prosdata$race)),]
table(prosdata$race)

str(prosdata)


#previous_cancer- 1536 NA values
table(prosdata$previous_cancer)
prosdata<- prosdata[!(is.na(prosdata$previous_cancer)),]
sum(is.na(prosdata$previous_cancer))


#Tumor_1_year= 1836 NA values
sum(is.na(prosdata$tumor_1_year))
prosdata<- prosdata[!(is.na(prosdata$tumor_1_year)),]
table(prosdata$tumor_1_year)


str(prosdata)

#psa_1_year- 760 NA values
sum(is.na(prosdata$psa_1_year))
prosdata<- prosdata[!(is.na(prosdata$psa_1_year)),]
table(prosdata$psa_1_year)



#weight- 925 NA values
sum(is.na(prosdata$weight))
prosdata<- prosdata[!(is.na(prosdata$weight)),]
table(prosdata$weight)

str(prosdata)


#family_degree_history- 0 NA values
table(prosdata$first_degree_history1)
sum(is.na(prosdata$first_degree_history))
#Condense first_degree_history into 2 Categories
#Category 0- 0 number of members
#Category 1- 1 or more number members
prosdata$first_degree_history1<- ifelse(prosdata$first_degree_history==0,0,1)



#Select columns to convert them into factors
cols<- c( 'gleason_score', 'race', 'previous_cancer','rd_thrpy',
          'h_thrpy', 'chm_thrpy', 'cry_thrpy', 'brch_thrpy', 'rad_rem', 'multi_thrpy', 'survival_1_year', 'survival_7_years','t_score' )

#Converting these variables from into to factors
prosdata[,cols] <-  data.frame(apply(prosdata[cols],2, as.factor))
str(prosdata)



#Also removing symptoms as it has a lot categories and not sure how to treat it.
prosdata<- subset(prosdata, select = -c(symptoms))



##### Univariate Analysis of the dependent variable survival_7_years
tab<-table(prosdata$survival_7_years)
ptab<-prop.table(tab)
#output
#now the table seems to be have better distribution
#Barplot
barplot(tab, main = "Bar Plot", xlab = "survival_7_years", ylab = "Frequency")

barplot(ptab, main = "Bar Plot", 
        xlab = "AccessOnlineRecords", 
        ylab = "Frequency", 
        col=c("orange", "steelblue"), 
        ylim=c(0,1))
box()








################ PREPARING THE TEST DATASET based on the training dataset ################
prostestdataset<- read.csv("~/Downloads/Health care/Prostate Cancer/participant_files/(name)_score.csv")
prostestdata<- prostestdataset
str(prostestdata)
prostestdata<- prostestdata[-1]
#Check Columnwise NA values 
na_count2 <-sapply(prostestdata, function(y) sum(length(which(is.na(y)))))
na_count2 <- data.frame(na_count2)
na_count2


#Removing the variables from the test dataset that were removed from the training dataset
prostestdata<- subset(prostestdata, select = -c(diagnosis_date, height, family_history, first_degree_history, smoker, side, tea, age, 
                                                symptoms, psa_diagnosis, psa_6_months,tumor_diagnosis, tumor_6_months ))


dim(prostestdata)

#Removing NA values-
sum(is.na(prostestdata$gleason_score))
prostestdata<- prostestdata[!(is.na(prostestdata$gleason_score)),]
table(prostestdata$previous_cancer)
prostestdata<- prostestdata[!(is.na(prostestdata$race)),]
prostestdata<- prostestdata[!(is.na(prostestdata$weight)),]
prostestdata<- prostestdata[!(is.na(prostestdata$previous_cancer)),]
prostestdata<- prostestdata[!(is.na(prostestdata$tumor_1_year)),]
prostestdata<- prostestdata[!(is.na(prostestdata$gleason_score)),]
prostestdata<- prostestdata[!(is.na(prostestdata$psa_1_year)),]

#Lets remove survival_1_year's 3598 NA values for now to check. Later we can remove the whole variable instead
prostestdata<- prostestdata[!(is.na(prostestdata$survival_1_year)),]


#Now, Condense gleason_score the data into categories-
#Category 1- 3-5
#Category 2- 6-8
#Category 3- 9,10,12
#Category 4- 13-14
prostestdata$gleason_score<- ifelse(prostestdata$gleason_score==3 | prostestdata$gleason_score ==4 | prostestdata$gleason_score ==5, 1, 
                                ifelse(prostestdata$gleason_score==6 |prostestdata$gleason_score==7 | prostestdata$gleason_score==8, 2, 
                                       ifelse(prostestdata$gleason_score==9 | prostestdata$gleason_score==10 | prostestdata$gleason_score==12 , 3, 4 )))

table(prostestdata$gleason_score)

#### WE WILL CONVERT GLEASON SCORE INTO FACTOR LATER #######

#wr condense t_score into 4 categories
#Category 1- T1a,T1b,T1c
#Category 2- T2a, T2b, T2c
#Cateogory 3- T3a, T3b, T3c
#Category 4- T4
prostestdata$t_score<- ifelse(prostestdata$t_score=='T1a' | prostestdata$t_score == 'T1b' | prostestdata$t_score =='T1c', 1, 
                          ifelse(prostestdata$t_score=='T2a' |prostestdata$t_score=='T2b' | prostestdata$t_score=='T2c', 2, 
                                 ifelse(prostestdata$t_score=='T3a' | prostestdata$t_score=='T3b' | prostestdata$t_score=='T3c' , 3, 4 )))


sum(is.na(prostestdata$t_score))
table(prostestdata$t_score)

str(prostestdata)
#Select columns to convert them into factors
cols2<- c( 'gleason_score', 'race', 'previous_cancer','rd_thrpy',
          'h_thrpy', 'chm_thrpy', 'cry_thrpy', 'brch_thrpy', 'rad_rem', 'multi_thrpy', 'survival_1_year','survival_7_years','t_score' )

#Converting these variables from into to factors
prostestdata[,cols2] <-  data.frame(apply(prostestdata[cols2],2, as.factor))
str(prostestdata)



#Now that the dataset is clean for both train and test , we can apply the model-


######SPLITTING DATA INTO TRAINING AND TEST###### FOR 70-30 SPLIT
set.seed(150)


mdTrn <- prosdata
mdTst <- prostestdata

dim(mdTrn)
dim(mdTst)

str(mdTrn)
str(mdTst)
mdTrn1<- subset(mdTrn, select= -c(survival_7_years))
mdTst1<- subset(mdTst, select= -c(survival_7_years))


####### RANDOM FOREST ##########
library(randomForest)
library(caret)
library(e1071)

#On training data
rfModel = randomForest(mdTrn$survival_7_years ~ ., data=mdTrn, ntree=200, importance=TRUE )
rfModel

# Predicting on train set
predTrain <- predict(rfModel, mdTrn, type = "class")

# Checking classification accuracy
table(predTrain, mdTrn$survival_7_years)  
accuracy<-sum(diag(predTrain))/sum(predTrain)
accuracy

#Another way to check accuracy

conf <- rfModel$confusion
conf
accuracy<-sum(diag(conf))/sum(conf)
accuracy

#Variable importance
importance(rfModel)
varImpPlot(rfModel)

#On test data
rfModel2 <- predict(rfModel, newdata = mdTst, type = "class")
rfModel2



######### LOGISTIC REGRESSION ##########

logmodel<-glm(survival_7_years~tumor_1_year+psa_1_year+gleason_score+
          t_score+n_score+m_score+stage+
          rd_thrpy+chm_thrpy+cry_thrpy+brch_thrpy+multi_thrpy+race+
          rad_rem,data = mdTrn,family =binomial(link="logit"))
summary(logmodel)
predTrain<-predict(logmodel, newdata=mdTrn,type = "response")
results <- ifelse(predTrain > 0.5,1,0)
results <-as.factor(results)
levels(results) <- c("0", "1")

library(caret)
summary(results)
install.packages("e1071")
library(e1071)
confusionMatrix(results,mdTrn$survival_7_years,positive = "1")


# On test data #
predTest<-predict(object=logmodel,newdata = mdTst, type="response")
results_test<-ifelse(predTest > 0.5,1,0)
results_test<-as.factor(results_test)
levels(results_test)<-c("0","1")
summary(results_test)
