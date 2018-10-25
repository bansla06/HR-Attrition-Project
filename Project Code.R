## All infernces and outputs are printed in the word file. This file contain only the steps which are performed on model ##


setwd('C:\\Users\\shubh\\Desktop\\Attrition')
HRdata =read.csv("Attrition.csv")

names(HRdata)

# Splitting the Dataset into Training and Testing
install.packages('caret')
library(caret)

set.seed(12345)
inTrain = createDataPartition(HRdata$Attrition,p=0.75,list=FALSE)
Training= HRdata[inTrain,]
Testing= HRdata[-inTrain,]

######----------------Exploratory Analysis-------------------#############
str(Training)

# Plotiing Bar Graph for Attrition Variable
library(ggplot2)
ggplot(Training,aes(Attrition,fill=Attrition))+geom_bar()


##### Plotting Stack Bar Graph Between variables and target variables #####
library(grid)
  
# Variable 1: Business Travel
travelplot=ggplot(Training,aes(BusinessTravel,fill=Attrition))+geom_bar()
travelplot

# Variable 2: Department
deptplot=ggplot(Training,aes(Department,fill=Attrition))+geom_bar()
deptplot

# Variable 3: Education
eduplot=ggplot(Training,aes(Education,fill=Attrition))+geom_bar()
eduplot

# Variable 4: Education Field
edufieldplot=ggplot(Training,aes(EducationField,fill=Attrition))+geom_bar()
edufieldplot

# Variable 5 : Gender
genderplot=ggplot(Training,aes(Gender,fill=Attrition))+geom_bar()
genderplot

# Variable 6 : Environment Satisfaction
environmentsatplot=ggplot(Training,aes(EnvironmentSatisfaction,fill=Attrition))+geom_bar()
environmentsatplot


# Variable 7 : Job involvement
jobInvolvementplot=ggplot(Training,aes(JobInvolvement,fill=Attrition))+geom_bar()
jobInvolvementplot

# Variable 8 : Job Level
JobLevelplot=ggplot(Training,aes(JobLevel,fill=Attrition))+geom_bar()
JobLevelplot

# Variable 9 : Job Satisfaction
jobsatplot=ggplot(Training,aes(JobSatisfaction,fill=Attrition))+geom_bar()
jobsatplot


# Variable 10 : Hourly Rate
hourlyplot=ggplot(Training,aes(HourlyRate,fill=Attrition))+geom_bar()
hourlyplot

# Variable 11 : Martial status
Msatplot=ggplot(Training,aes(MaritalStatus,fill=Attrition))+geom_bar()
Msatplot

# Variable 12 : Monthly Income
moniplot=ggplot(Training,aes(MonthlyIncome,fill=Attrition))+geom_density()
moniplot


# Variable 13 : Job Role
jobroleplot=ggplot(Training,aes(JobRole,fill=Attrition))+geom_bar()
jobroleplot

# Variable 14 : OverTime
OverTimeplot=ggplot(Training,aes(OverTime,fill=Attrition))+geom_bar()
OverTimeplot

# Variable 15 : PerformanceRating
PerformanceRatingplot=ggplot(Training,aes(PerformanceRating,fill=Attrition))+geom_bar()
PerformanceRatingplot

# Variable 16 : RelationshipSatisfaction
RelationshipSatisfactionplot=ggplot(Training,aes(RelationshipSatisfaction,fill=Attrition))+geom_bar()
RelationshipSatisfactionplot


# Variable 17 : StockOptionLevel
StockOptionLevelplot=ggplot(Training,aes(StockOptionLevel,fill=Attrition))+geom_bar()
StockOptionLevelplot

# Variable 18 : TotalWorkingYears
TotalWorkingYearsplot=ggplot(Training,aes(TotalWorkingYears,fill=Attrition))+geom_bar()
TotalWorkingYearsplot

# Variable 19:  TrainingTimesLastYear
TrainingTimesLastYearplot=ggplot(Training,aes(TrainingTimesLastYear,fill=Attrition))+geom_bar()
TrainingTimesLastYearplot

# Variable 20 : WorkLifeBalance	
WorkLifeBalanceplot=ggplot(Training,aes(WorkLifeBalance,fill=Attrition))+geom_bar()
WorkLifeBalanceplot

# Variable 21 : YearsAtCompany	
YearsAtCompanyplot=ggplot(Training,aes(YearsAtCompany	,fill=Attrition))+geom_bar()
YearsAtCompanyplot

# Variable 22 : YearsInCurrentRole	
YearsInCurrentRoleplot=ggplot(Training,aes(YearsInCurrentRole,fill=Attrition))+geom_bar()
YearsInCurrentRoleplot

# Variable 23 : YearsSinceLastPromotion	
YearsSinceLastPromotionplot=ggplot(Training,aes(YearsSinceLastPromotion,fill=Attrition))+geom_bar()
YearsSinceLastPromotionplot

# Variable 24 : YearsWithCurrManager

YearsWithCurrManagerplot=ggplot(Training,aes(YearsWithCurrManager,fill=Attrition))+geom_bar()
YearsWithCurrManagerplot

# Variable 25 : MonthlyRate
MonthlyRateplot=ggplot(Training,aes(MonthlyRate,fill=Attrition))+geom_density()
MonthlyRateplot

# Variable 26 : Daily Rate
Dailyplot=ggplot(Training,aes(DailyRate,Attrition))+geom_point(size=4,alpha=0.05)
Dailyplot

# Variable 27 : Distance From Home
distancehomeplot=ggplot(Training,aes(DistanceFromHome,fill=Attrition))+geom_bar()
distancehomeplot

# Variable 28 : Percent Salary Hike
salaryhikeplot=ggplot(Training,aes(PercentSalaryHike,Attrition))+geom_point(size=4,alpha=0.05)
salaryhikeplot

# Variable 29 : Number of companies worked
numcompaniesplot=ggplot(Training,aes(NumCompaniesWorked,fill=Attrition))+geom_bar()
numcompaniesplot

# Variable 30 : Age
ageplot=ggplot(Training,aes(Age,fill=Attrition))+geom_density()+facet_grid(~Attrition)
ageplot


#####------------Finding Collinearity and multicollinearity-------#####
install.packages("corrplot")
library("corrplot")

##Removing Employee Number,Standard hours variables##
HRdata <- (HRdata[ ,-c(9,10,22)])

# COnverting Categorical variable into numeric
HRdatan <- which(sapply(HRdata,is.numeric))

#Checking if there is Multi-Co linearity - High Correlation between 
corrplot(cor(HRdata[HRdatan]),type = "upper",method='color',tl.cex = .7,cl.cex = .7,number.cex = 0.7)

######----------Decision Tree----------##########

# Removing Unuseful variables
HRdata2 <- HRdata[c(-5,,-12,-19,-25,-29,-30,-32,-35)]

# Getting rid of long variable names 
levels(HRdata2$JobRole) <- c("HC", "HR", "Lab", "Man", "MDir", "RsD", "RsSci", "SlEx", "SlRep")
levels(HRdata2$EducationField) <- c("HR", "LS", "MRK", "MED", "NA", "TD")

# Creating train & test sets after removing the variables
n <- nrow(HRdata2)

rnd <- sample(n, n * .70)
train <- HRdata2[rnd,]
test <- HRdata2[-rnd,]

# Modeling
dtree <- rpart(Attrition ~., data = train)
preds <- predict(dtree, test, type = "class")

# Converting Categorical variables into numeric
install.packages("pROC")
library(pROC)
rocv <- roc(as.numeric(test$Attrition), as.numeric(preds))
rocv$auc

# Diplaying Confusion matrix
prop.table(table(test$Attrition, preds, dnn = c("Actual", "Predicted")),1)

#plotting the Decision tree
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(dtree, 
           type = 4, 
           extra = 104,
           leaf.round=1,
           cex=0.5,
           tweak = 1.0, 
           fallen.leaves = T,
           varlen=0)






