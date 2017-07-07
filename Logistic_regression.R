#Here we have dependent variable as revenue grid, the target is to find 
#profitable and non profitable customers for bank- bank has divided customers 
#manually as high net worth customers as 1 and low net worth 
#customers as 2


#Loading libraries
library("dplyr")
R <- read.csv("C:\\Users\\Admin\\Desktop\\Existing Base.csv", stringsAsFactors = F) 
#Above code is reading data in R
glimpse(R) #Looking at data

#Let us prepare data as per our need
#We need to run logistic regression here therefore dependent variable here
#Revenue.grid should be in terms of 0 and 1
R$Revenue.Grid <- ifelse(R$Revenue.Grid==1, 1, 0)
table(R$Revenue.Grid)

#Let us see how many categories are there in children
table(R$children)
R <- R %>% mutate(gsub("Zero","0", substr(children,1,1)))
glimpse(R) #Looking at data how children column has change

table(R$age_band) #Checking how many categories there in age_band
#We will try to reduce the number of categories by looking at 
#percentage distribution
#We will use percentage split here
round(prop.table(table(R$age_band,R$Revenue.Grid),1),2)
#Let us make the categories now
# 1 : 61-65 0.09
# 2 : 45-50, 51-55, 65-70, 71+ 0.10
# 3 : 22-25, 26-30, 31-35, 41-45, 55-60 0.11
# 4 : 36-40 0.13
# 5 : 18-21 0.17

R<- R %>%
  mutate(Age_band_1= as.numeric(age_band %in% c("61-65")),
         Age_band_2= as.numeric(age_band %in% c("45-50","51-55","65-70","71+")),
         Age_band_3=as.numeric(age_band %in% c("22-25","26-30","31-35",
                                               "41-45","55-60")),
         Age_band_4=as.numeric(age_band %in% c("36-40")),
         Age_band_5=as.numeric(age_band %in% c("18-21"))) %>% 
  select(-age_band)

#Next we will be working on status
table(R$status)
unique(R$status) #Unique give us names of the categories clearly
round(prop.table(table(R$status,R$Revenue.Grid),1),2)
#Assigning categories
#1 : Divorced/Separated, Partner, Single/Never Married 0.11

R <-R %>% mutate(Status_1 = as.numeric(status %in% 
                                         c("Divorced/Separated", "Partner", 
                                           "Single/Never Married"))) %>% 
  select(-status)
glimpse(R) #Looking how data is looking

#Next we will work on occupation
table(R$occupation)
#Percentage proportion
round(prop.table(table(R$occupation,R$Revenue.Grid),1),2)
#Bucketing of categories
#1 Retired : 0.10
#2 Secretarial/Admin, Student, Unknown, Manual Worker, Other : 0.11
#3 Housewife : 0.09
R <- R %>% mutate(occupation_1 = as.numeric(occupation %in% c("Retired")),
                  occupation_2 = as.numeric(occupation %in% 
                                              c("Secretarial/Admin", "Student", 
                                                "Unknown", "Manual Worker", "Other")),
                  occupation_3 = as.numeric(occupation %in% c("Housewife"))) %>% 
                    select(-occupation)

#Next Occupation partner
table(R$occupation_partner)
round(prop.table(table(R$occupation_partner, R$Revenue.Grid),1),2)
#Bucketing of categories
#1 Business Manager, Housewife, Manual Worker, Professional : 0.11
#2 Retired, Other, Unknown 0.12
R <- R %>% mutate(occupation_partner_1 = as.numeric(occupation_partner %in% 
                                                      c("Business Manager", 
                                                        "Housewife",
                                                        "Manual Worker", 
                                                        "Professional")),
                  occupation_partner_2 = as.numeric(occupation_partner %in%
                                                  "Retired","Other","Unknown")) %>% 
  select(-occupation_partner)
glimpse(R)

#Next home_status
table(R$home_status)
round(prop.table(table(R$home_status,R$Revenue.Grid),1),2)
#Bucketing categories
#1 Rent Privately, Live in Parental Hom, Own Home : 0.11
#2 Rent from Council/HA
R<-R %>% mutate(home_status_1= as.numeric(home_status %in% 
                                            c("Rent Privately", 
                                              "Live in Parental Hom", "Own Home")),
                home_status_2 = as.numeric(home_status %in%
                                             c("Rent from Council/HA"))) %>% 
  select(-home_status)
glimpse(R)

#Next is family_income
table(R$family_income)
round(prop.table(table(R$family_income,R$Revenue.Grid),1),2)
#Bucketing the categories
#1 "<4,000", "<8,000, >= 4,000" : 0.08
#2 "<12,500, >=10,000", "<25,000, >=22,500","<27,500 >=25,000" : 0.10
#3 "<10,000, >=8,000", "<15,000, >=12,500", "<20,000, >=17,500", " >=35,000" : 0.11
#4 "<17,500, >=15,000", "<22,500, >=20,000", "<30,000, >=27,500" : 0.12
R <- R %>% mutate(family_income_1 = as.numeric(family_income %in% 
                                                 c("Unknown")),
                  family_income_2 = as.numeric(family_income %in% 
                                                 c("<12,500, >=10,000", "<25,000, 
                                                   >=22,500","<27,500 
                                                   >=25,000" )),
                  family_income_3 = as.numeric(family_income %in%
                                                 c("<10,000, >=8,000", "<15,000, 
                                                   >=12,500", 
                                                   "<20,000, >=17,500", 
                                                   " >=35,000")),
                  family_income_4 = as.numeric(family_income %in%
                                                 c("<17,500, >=15,000", "<22,500,
                                                   >=20,000", "<30,000, >=27,500"))) %>% 
  select(-family_income)
glimpse(R)

#Next is self_employed
table(R$self_employed)
R<-R %>% mutate(self_employed_yes = as.numeric(self_employed == "Yes")) %>% 
  select(-self_employed)

#Next is self_employed_partner
table(R$self_employed_partner)
R<-R %>% mutate(self_employed_partner_yes = as.numeric(self_employed_partner == 
                                                         "Yes")) %>% 
  select(-self_employed_partner)                
glimpse(R)

#Next I will take gender
table(R$gender)
R <-R %>% mutate(gender_female = as.numeric(gender == "Female")) %>% 
  select(-gender)
glimpse(R)

#Next I will table region
table(R$region)
round(prop.table(table(R$region,R$Revenue.Grid),1),2)
#Bucketing categories
#1 "South West" : 0.09
#2 "North","East Midlands","South East" : 0.10
#3 "North West","Scotland","Wales","West Midlands" : 0.11
#4 "Northern Ireland","Unknown" : 0.12
#5 "East Anglia" : 0.13
#6 "Channel Islands" : 0.20
R <- R %>%  mutate(region_1 = as.numeric(region %in% c("North",
                                                       "East Midlands","South East")),
                   region_2 = as.numeric(region %in% c("North West","Scotland"
                                                      ,"Wales","West Midlands")),
                   region_3 = as.numeric(region %in% c("Northern Ireland",
                                                       "Unknown")),
                   region_4 = as.numeric(region %in% c("East Anglia", "Channel Islands"))) %>% 
  select(-region)
glimpse(R)

#Next I will see year_last_moved
table(R$year_last_moved)
#Here we have year_last_moved as 0, it means someone forget to put year in data
#We will impute this value of club it with some other year using bucketing
round(prop.table(table(R$year_last_moved,R$Revenue.Grid),1),2)
#Since 1938 and 0 has 0.05 proportion, I will club them
R<-R %>% mutate(year_last_moved=ifelse(year_last_moved==0,1938,year_last_moved))
table(R$year_last_moved) #Check if it happened

#Next we will take TVarea
table(R$TVarea)
round(prop.table(table(R$TVarea,R$Revenue.Grid),1),2)
#Bucketing categories
#1 "Grampian", "HTV" : 0.10
#2 "Anglia","Carlton"," Central","Scottish TV" : 0.11
#3 "Granada","Ulster","Unknown", "Yorkshire" : 0.12
#4 "Border" : 0.14
R <- R %>% mutate(TVarea.1 = as.numeric(TVarea %in% c("Grampian","HTV")),
                  TVarea.2 = as.numeric(TVarea %in% c("Anglia","Carlton","Central",
                                                      "Scottish TV")),
                  TVarea.3 = as.numeric(TVarea %in% c("Granada","Ulster",
                                                      "Unknown","Yorkshire")),
                  TVarea.4 = as.numeric(TVarea %in% c("Border"))) %>% 
  select(-TVarea)
glimpse(R)

#Next I will go for post_code
table(R$post_code)
#As post_code has too many categories, I will drop this variable
R<-R %>% select(-post_code)

#Next I will go for post_area
table(R$post_area)

#As post_area has too many categories, I will drop this variable
R<-R %>% select(-post_area)

#Finally we are done with data cleaning, now we will do modelling
#Please check before starting with modelling that no variable has NA's in summary
#we can check this by using summary(R)

#We will start with sampling for validation
set.seed(1712)
R_=sample(1:nrow(R),0.75*nrow(R)) #This will select 75% of rows from R
R_train=R[R_,]
R_test=R[-R_,]
nrow(R_train) #Checking if we have train data of 75% rows
nrow(R_test) #Checking if we have test data of 25% rows

library(car) #This isuse for checking up VIF 
R_fit <- lm(Revenue.Grid ~ . -REF_NO, data=R_train) 
summary(R_fit)
R_vif <- vif(R_fit) #Calculating VIF
sort(R_vif, decreasing = T)[1:3] #Arranging as per VIF only looking at top 3 

#Dropping the feature with maximum variance
R_fit <- lm(Revenue.Grid ~ . -REF_NO -Investment.in.Commudity, data=R_train) 
R_vif <- vif(R_fit) #Calculating VIF
sort(R_vif, decreasing = T)[1:3] #Arranging as per VIF only looking at top 3 

#Dropping the feature with maximum variance
R_fit <- lm(Revenue.Grid ~ . -REF_NO -Investment.in.Commudity 
            -Investment.in.Derivative, data=R_train) 
R_vif <- vif(R_fit) #Calculating VIF
sort(R_vif, decreasing = T)[1:3] #Arranging as per VIF only looking at top 3 

#Dropping the feature with maximum variance
R_fit <- lm(Revenue.Grid ~ . -REF_NO -Investment.in.Commudity 
            -Investment.in.Derivative
              -Investment.in.Equity
            , data=R_train) 
R_vif <- vif(R_fit) #Calculating VIF
sort(R_vif, decreasing = T)[1:3] #Arranging as per VIF only looking at top 3 

#Dropping the feature with maximum variance
R_fit <- lm(Revenue.Grid ~ . -REF_NO -Investment.in.Commudity 
            -Investment.in.Derivative
            -Investment.in.Equity -Age_band_3
            , data=R_train) 
R_vif <- vif(R_fit) #Calculating VIF
sort(R_vif, decreasing = T)[1:3] #Arranging as per VIF only looking at top 3

#Dropping the feature with maximum variance
R_fit <- lm(Revenue.Grid ~ . -REF_NO -Investment.in.Commudity 
            -Investment.in.Derivative
            -Investment.in.Equity -Age_band_3 -Portfolio.Balance
            , data=R_train) 
R_vif <- vif(R_fit) #Calculating VIF
sort(R_vif, decreasing = T)[1:3] #Arranging as per VIF only looking at top 3

#Dropping the feature with maximum variance
R_fit <- lm(Revenue.Grid ~ . -REF_NO -Investment.in.Commudity 
            -Investment.in.Derivative
            -Investment.in.Equity -Age_band_3 -Portfolio.Balance - home_status_1
            , data=R_train) 
R_vif <- vif(R_fit) #Calculating VIF
sort(R_vif, decreasing = T)[1:3] #Arranging as per VIF only looking at top 3

#For logistic regression we love feature with less than variance =10, so we will
#keep all of the features now 

#We created a new data set here so we can easily write the variables dropped
R_train_new <- R_train %>%  select(-REF_NO, -Investment.in.Commudity, 
                                   -Investment.in.Derivative,
                                   -Investment.in.Equity, -Age_band_3, 
                                   -Portfolio.Balance, - home_status_1)

R_fit_new <- glm(Revenue.Grid ~., family = "binomial",data=R_train_new)
summary(R_fit_new)

#Using AIC (automated function) for better model, we can do this through step function
R_fit_new <- step(R_fit_new)
formula(R_fit_new) #Will give the formula after doing AIC as best 
R_fit_new <- glm(Revenue.Grid ~ children + Average.Credit.Card.Transaction + 
                   Balance.Transfer + 
                   Term.Deposit + Life.Insurance + Medical.Insurance + 
                   Average.A.C.Balance + 
                   Personal.Loan + Investment.in.Mutual.Fund + 
                   Investment.Tax.Saving.Bond + 
                   Home.Loan + Online.Purchase.Amount + family_income_1 + 
                   self_employed_yes + 
                   self_employed_partner_yes + gender_male + TVarea.2 
                 + TVarea.3,family = "binomial", data = R_train_new)
summary(R_fit_new)
#We will now remove insignificant variables based on p-value > 0.05
#Starting with family_income_1
R_fit_new <- glm(Revenue.Grid ~ children + Average.Credit.Card.Transaction + 
                   Balance.Transfer + 
                   Term.Deposit + Life.Insurance + Medical.Insurance + 
                   Average.A.C.Balance + 
                   Personal.Loan + Investment.in.Mutual.Fund + 
                   Investment.Tax.Saving.Bond + 
                   Home.Loan + Online.Purchase.Amount + 
                   self_employed_yes + 
                   self_employed_partner_yes + gender_male + TVarea.2 
                 + TVarea.3,family = "binomial", data = R_train_new)
summary(R_fit_new)
#Next we will delete self_emplyed_yes
R_fit_new <- glm(Revenue.Grid ~ children + Average.Credit.Card.Transaction + 
                   Balance.Transfer + 
                   Term.Deposit + Life.Insurance + Medical.Insurance + 
                   Average.A.C.Balance + 
                   Personal.Loan + Investment.in.Mutual.Fund + 
                   Investment.Tax.Saving.Bond + 
                   Home.Loan + Online.Purchase.Amount + 
                   self_employed_partner_yes + gender_male + TVarea.2 
                 + TVarea.3,family = "binomial", data = R_train_new)
summary(R_fit_new)
#Next we will reduce Investment.in.Mutual.Fund
R_fit_new <- glm(Revenue.Grid ~ children + Average.Credit.Card.Transaction + 
                   Balance.Transfer + 
                   Term.Deposit + Life.Insurance + Medical.Insurance + 
                   Average.A.C.Balance + 
                   Personal.Loan +
                   Investment.Tax.Saving.Bond + 
                   Home.Loan + Online.Purchase.Amount + 
                   self_employed_partner_yes + gender_male + TVarea.2 
                 + TVarea.3,family = "binomial", data = R_train_new)
summary(R_fit_new)
#Next we will drop TVarea.2
R_fit_new <- glm(Revenue.Grid ~ children + Average.Credit.Card.Transaction + 
                   Balance.Transfer + 
                   Term.Deposit + Life.Insurance + Medical.Insurance + 
                   Average.A.C.Balance + 
                   Personal.Loan +
                   Investment.Tax.Saving.Bond + 
                   Home.Loan + Online.Purchase.Amount + 
                   self_employed_partner_yes + gender_male 
                 + TVarea.3,family = "binomial", data = R_train_new)
summary(R_fit_new)
#Next we will drop TVarea.3
R_fit_new <- glm(Revenue.Grid ~ children + Average.Credit.Card.Transaction + 
                   Balance.Transfer + 
                   Term.Deposit + Life.Insurance + Medical.Insurance + 
                   Average.A.C.Balance + 
                   Personal.Loan +
                   Investment.Tax.Saving.Bond + 
                   Home.Loan + Online.Purchase.Amount + 
                   self_employed_partner_yes + gender_male
                 ,family = "binomial", data = R_train_new)
summary(R_fit_new)
#Now all the left over variables looks significant
#The below code will give predicted values for training data
R_train_new$score <- predict(R_fit_new, newdata = R_train_new, type = "response")

#Let us plot the result
library(ggplot2)
ggplot(R_train_new,aes(y=Revenue.Grid,x=score,color=factor(Revenue.Grid)))+
geom_point() + geom_jitter()
#x axis shows that predicted probabilities. 
#We can see True Negative on left on the x axis of the bottom graph
#We can see True Positive on right on the x axis of the top graph

#Let us assume cut off is 0.2
cutoff = 0.2
predicted<- as.numeric(R_train_new$score>cutoff)
glimpse(predicted)
#Calculating TP,TN,FP,FN

glimpse(R_train_new$Revenue.Grid)
glimpse(predicted)
TP =  sum(predicted==1 & R_train_new$Revenue.Grid==1)
FP <- sum(predicted==1 & R_train_new$Revenue.Grid==0)
FN <- sum(predicted==0 & R_train_new$Revenue.Grid==1)
TN <- sum(predicted==0 & R_train_new$Revenue.Grid==0)

#Now we need to find out our total positive and total negatives
# Lets also calculate total number of 1's i.e. positives & 0's i.e. negatives
P=TP+FN # Total number of 1's in reality
N=TN+FP # Total number of 0's in relity
#Total number of points
Total <- P + N 

#We can do this for multiple cutoff values as for only one cutoff = 0.4 is of no use
#Creating table with different cutoff values
cutoff_data = NULL
cutoffs = round(seq(0,1,length=100),3)
cutoffs # A vector of 100 cutoff values

for (cutoff in cutoffs)
{
  predicted=as.numeric(R_train_new$score>cutoff)
  
  TP=sum(predicted==1 & R_train_new$Revenue.Grid==1)
  FP=sum(predicted==1 & R_train_new$Revenue.Grid==0)
  FN=sum(predicted==0 & R_train_new$Revenue.Grid==1)
  TN=sum(predicted==0 & R_train_new$Revenue.Grid==0)
  cutoff_data=rbind.data.frame(cutoff_data,c(cutoff,TP,FP,FN,TN))
}
colnames(cutoff_data) = c("Cutoff", "TP", "FP", "FN", "TN")
View(cutoff_data)

#Now we can make the metrics to find the best cut off
cutoff_data=cutoff_data %>%
  mutate(Sn=TP/P, 
         Sp=TN/N,
         dist=sqrt((1-Sn)**2+(1-Sp)**2),
         P=FN+TP,
         N=TN+FP) %>%
  mutate(KS=abs((TP/P)-(FP/N))) %>%
  mutate(Accuracy=(TP+TN)/(P+N)) %>%
  mutate(Lift=(TP/P)/((TP+FP)/(P+N))) %>%
  mutate(M=(8*FN+2*FP)/(P+N)) %>%
  select(-P,-N)

View(cutoff_data)

#It would be easier for us to determine best cut off if we can see the data in 
#visualization therefore we will plot the metrics however before that
#we must convert matrix into "long format" from "wide format"

library(tidyr)#For tiding the data together

cutoff_viz=cutoff_data %>%
  select(Cutoff,Sn,Sp,dist,KS,Accuracy,Lift,M) %>%
  gather(Criterion,Value,Sn:M)

View(cutoff_viz) # Data is in long format

# Visualization
ggplot(filter(cutoff_viz,Criterion!="Lift"),aes(x=Cutoff,y=Value,color=Criterion))+
  geom_line()

# We'll visualise lift separately because of its scale
 cutoff_viz %>%
  filter(Criterion=="Lift") %>%
 ggplot(aes(x=Cutoff,y=Value,color=Criterion))+geom_line()

# Predict on testset
R_test$score=predict(R_fit_new,newdata = R_test,type = "response")
 
# Cutoff with max KS
KS_cutoff=cutoff_data$Cutoff[which.max(cutoff_data$KS)][1]
KS_cutoff
 
table(R_test$Revenue.Grid,as.numeric(R_test$score>KS_cutoff)) # Confusion matrix
 
#Cutoff with minimum distance
dist_cutoff=cutoff_data$Cutoff[which.min(cutoff_data$dist)][1]
dist_cutoff

table(R_test$Revenue.Grid,as.numeric(R_test$score>dist_cutoff))

# Cutoff with max Accuracy
Acc_cutoff=cutoff_data$Cutoff[which.max(cutoff_data$Accuracy)][1]
Acc_cutoff
table(R_test$Revenue.Grid,as.numeric(R_test$score>Acc_cutoff))

# Cutoff with minimum M ( The hypothetical business criterion)
M_cutoff=cutoff_data$Cutoff[which.min(cutoff_data$M)][1]
M_cutoff
table(R_test$Revenue.Grid,as.numeric(R_test$score>M_cutoff))

# ROC Curve
library(pROC)
roccurve=roc(R_train$Revenue.Grid,R_train_new$score)
plot(roccurve)
auc(roccurve)
roc_data=cutoff_data %>% select(cutoff,Sn,Sp) %>% 
  mutate(TPR=Sn,FPR=1-Sp) %>% select(cutoff,TPR,FPR)

View(roc_data) #Once you have find correct (FPR, TPR) cut-off value will be in table
 
ggplot(roc_data,aes(x=FPR,y=TPR))+geom_line()+ggtitle("My ROC Curve")
