setwd('C:/Users/wangqn/Desktop/study/6221/project')
library(tidyverse)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(ggplot2)
library(ggthemes)
library(Hmisc)
library(gridExtra)
library(pROC)
library(plyr)
library(reshape2)
library(caret)

Attrition_data <- read.csv('IBM.csv', header = TRUE)
str(Attrition_data)
describe(Attrition_data)
names(Attrition_data)[1] <- 'Age'

#Visual exploration
#1. Basic identity information
#Relationship between Gender and Attrition
p_Gender <- ggplot(data = Attrition_data, aes(x = Gender)) +
  geom_bar(aes(fill = Attrition), position = "fill") +
  theme_economist() +
  labs(title = "Gender VS Attrition", x = "Gender", y = "Ratio")
p_Gender

#Relationship between Age and Attrition
p_Age <- ggplot(data = Attrition_data, aes(x = Age)) +
  geom_density(aes(fill = Attrition), alpha = 0.7) +
  theme_economist() +
  labs(title = "Age VS Attrition", x = "Age", y = "") +
  scale_x_continuous(breaks = seq(18, 60, 6))
p_Age

#Relationship between Education and Attrition
p_Education  <- ggplot(data = Attrition_data, aes(x = Education)) +
  geom_bar(aes(fill = Attrition), position = "fill") +
  theme_economist() +
  labs(title = "Education VS Attrition", x = "Education", y = "Ratio")
p_Education

#Relationship between MaritalStatus and Attrition
p_MaritalStatus <- ggplot(data = Attrition_data, aes(x = MaritalStatus)) +
  geom_bar(aes(fill = Attrition), position = "fill") +
  theme_economist() +
  labs(title = "MaritalStatus VS Attrition", x = "MaritalStatus", y = "Ratio")
p_MaritalStatus

#Relationship between NumCompaniesWorked and Attrition
p_NumCompaniesWorked <- ggplot(data =Attrition_data, aes(x = NumCompaniesWorked)) +
  geom_density(aes(fill = Attrition), alpha = 0.7) +
  theme_economist() +
  labs(title = "NumCompaniesWorked VS Attrition", x = "NumCompaniesWorked", y = "")+
  scale_x_continuous(breaks = seq(0, 9, 1)) 
p_NumCompaniesWorked
# Analysis results:
# Attrition rate has little relationship with Gender
# People under 33 are more likely to leave
# The higher the education level, the lower the turnover rate
# Unmarried and single people are more likely to leave
# People with more than 5 companies are more likely to leave

#2. Employee company identity information
#Relationship between TotalworkingYears and Attrition
p_TotalWorkingYears <- ggplot(data = Attrition_data, aes(x = TotalWorkingYears)) +
  geom_density(aes(fill = Attrition), alpha = 0.7) +
  theme_economist() +
  labs(title = "TotalworkingYears VS Attrition", x = "TotalworkingYears", y = "") +
  scale_x_continuous(breaks = seq(0, 40, 5))
p_TotalWorkingYears

#Relationship between YearsAtCompany and Attrition
p_YearsAtCompany <- ggplot(data = Attrition_data, aes(x = YearsAtCompany)) +
  geom_density(aes(fill = Attrition), alpha = 0.7) +
  theme_economist() +
  labs(title = "YearsAtCompany VS Attrition", x = "YearsAtCompany", y = "") +
  scale_x_continuous(breaks = seq(0, 40, 5))
p_YearsAtCompany

#Relationship between JobRole and Attrition
p_JobRole <- ggplot(data = Attrition_data, aes(x = JobRole)) +
  geom_bar(aes(fill = Attrition), position = "fill") +
  theme_economist() +
  labs(title = "JobRole VS Attrition", x = "JobRole", y = "Ratio") +
  theme(axis.text.x = element_text(angle = 90))
p_JobRole

#Relationship between JobLevel and Attrition
p_JobLevel <- ggplot(data = Attrition_data, aes(x = JobLevel)) +
  geom_bar(aes(fill = Attrition), position = "fill") +
  theme_economist() +
  labs(title = "JobLevel VS Attrition", x = "JobLevel", y = "Ratio")
p_JobLevel

# Analysis result:
# People with less than 8 years of total service are more likely to leave
# People who have worked in the company for less than 4 years are more likely to leave
# Lower level employees are more likely to leave
# High turnover rate of sales department staff

#3. Salary and benefits information
#(1) Monthly salary, work input and performance score
#Relationship between MonthlyIncome and Attrition
p_MonthlyIncome <- ggplot(data = Attrition_data, aes(x = MonthlyIncome)) +
  geom_density(aes(fill = Attrition), alpha = 0.7) +
  theme_economist() +
  labs(title = "MonthlyIncome VS Attrition", x = "MonthlyIncome", y = "") +
  scale_x_continuous(breaks = seq(0, 20000, 3000)) +
  theme(axis.text.x =  element_text(angle = 15))
p_MonthlyIncome

#Relationship between MonthlyIncome and Attrition
p_JobInvolvement <- ggplot(data = Attrition_data, aes(x = JobInvolvement)) +
  geom_bar(aes(fill = Attrition), position = "fill") +
  theme_economist() +
  labs(title = "JobInvolvement VS Attrition", x = "JobInvolvement", y = "Ratio")
p_JobInvolvement

#Relationship between MonthlyIncome and Attrition
p_PerformanceRating <- ggplot(data = Attrition_data, aes(x = PerformanceRating)) +
  geom_bar(aes(fill = Attrition), position = "fill") +
  theme_economist() +
  labs(title = "PerformanceRating VS Attrition", x = "PerformanceRating", y = "Ratio") +
  scale_x_continuous(breaks = seq(3, 4, 1))
p_PerformanceRating

#Relationship between Employee input-output and Attrition
Attrition_data$JobInvolvement1 <- as.character(Attrition_data$JobInvolvement)
p_JobInvolvement_MonthlyIncome <- ggplot(data = Attrition_data, aes(x = JobInvolvement1, y = MonthlyIncome)) +
  geom_boxplot(aes(fill = Attrition)) +
  theme_economist() +
  labs(title = "JobInvolvement and  MonthlyIncome", x = "JobInvolvement", y = "MonthlyIncome")
p_JobInvolvement_MonthlyIncome

# Analysis result:
# The higher the work input, the lower the turnover rate
# Performance score has little effect on turnover
# The turnover rate of employees with monthly salary below 4000 and around 10000 is higher
# Under the same investment, the monthly salary of the former employees is lower than that of the former employees

#(2)Welfare related variables
#Relationship between StockOptionLevel and Attrition
p_StockOptionLevel <- ggplot(data = Attrition_data, aes(x = StockOptionLevel)) +
  geom_bar(aes(fill = Attrition), position = "fill") +
  theme_economist() +
  labs(title = "StockOptionLevel VS Attrition", x = "StockOptionLevel", y = "Ratio") 
p_StockOptionLevel

#Relationship between PercentSalaryHike and Attrition
p_PercentSalaryHike <- ggplot(data = Attrition_data, aes(x = PercentSalaryHike)) +
  geom_density(aes(fill = Attrition), alpha = 0.7) +
  theme_economist() +
  labs(title = "PercentSalaryHike VS Attrition", x = "PercentSalaryHike", y = "") +
  scale_x_continuous(breaks = seq(0, 26, 2))
p_PercentSalaryHike

#Relationship between TrainingTimesLastYear and Attrition
p_TrainingTimesLastYear <- ggplot(data = Attrition_data, aes(x = TrainingTimesLastYear)) +
  geom_bar(aes(fill = Attrition), position = "fill") +
  theme_economist() +
  labs(title = "TrainingTimesLastYear VS Attrition", x = "TrainingTimesLastYear", y = "Ratio") +
  scale_x_continuous(breaks = seq(0, 6, 1)) 
p_TrainingTimesLastYear

#Relationship between YearsSinceLastPromotion and Attrition
p_YearsSinceLastPromotion <- ggplot(data = Attrition_data, aes(x = YearsSinceLastPromotion)) +
  geom_density(aes(fill = Attrition), alpha = 0.7) +
  theme_economist() +
  labs(title = "YearsSinceLastPromotion VS Attrition", x = "YearsSinceLastPromotion", y = "") +
  scale_x_continuous(breaks = seq(0, 15, 1))
p_YearsSinceLastPromotion

# Analysis result:
# Those with the highest and lowest StockOptionLevel are more likely to leave
# The turnover rate of those with PercentSalaryHike of 15% and 17% and above 22% is higher
# In general, the less training, the more likely to leave
# People who are less than half a year or seven years away from their last salary increase are more likely to leave

#4. Quality of life related
#(1) Subjective satisfaction survey
#Relationship between EnvirnomentSatisfaction and Attrition
p_EnvirnomentSatisfaction <- ggplot(data = Attrition_data, aes(x = EnvironmentSatisfaction)) +
  geom_bar(aes(fill = Attrition), position = "fill") +
  theme_economist() +
  labs(title = "EnvirnomentSatisfaction VS Attrition", x = "EnvirnomentSatisfaction", y = "Ratio")
p_EnvirnomentSatisfaction

#Relationship between JobSatisfication and Attrition
p_JobSatisfication <- ggplot(data = Attrition_data, aes(x = JobSatisfaction)) +
  geom_bar(aes(fill = Attrition), position = "fill") +
  theme_economist() +
  labs(title = "JobSatisfication VS Attrition", x = "JobSatisfication", y = "Ratio")
p_JobSatisfication

#Relationship between RelationshipSatisfaction and Attrition
p_RelationshipSatisfaction <- ggplot(data = Attrition_data, aes(x = RelationshipSatisfaction)) +
  geom_bar(aes(fill = Attrition), position = "fill") +
  theme_economist() +
  labs(title = "RelationshipSatisfaction VS Attrition", x = "RelationshipSatisfaction", y = "Ratio")
p_RelationshipSatisfaction

#Relationship between WorkLifeBalance and Attrition
p_WorkLifeBalance <- ggplot(data = Attrition_data, aes(x = WorkLifeBalance)) +
  geom_bar(aes(fill = Attrition), position = "fill") +
  theme_economist() +
  labs(title = "WorkLifeBalance VS Attrition", x = "WorkLifeBalance", y = "Ratio")
p_WorkLifeBalance

# Analysis result:
# Less satisfied people are more likely to leave
# Most likely to leave work life imbalance

#(2) Objective work life conflict
#Relationship between DistanceFromHome and Attrition
p_DistanceFromHome <- ggplot(data = Attrition_data, aes(x = DistanceFromHome)) +
  geom_density(aes(fill = Attrition), alpha = 0.7) +
  theme_economist() +
  labs(title = "DistanceFromHome VS Attrition", x = "DistanceFromHome", y = "") +
  scale_x_continuous(breaks = seq(1, 29, 1))
p_DistanceFromHome

#Relationship between OverTime and Attrition
p_OverTime <- ggplot(data = Attrition_data, aes(x = OverTime)) +
  geom_bar(aes(fill = Attrition), position = "fill") +
  theme_economist() +
  labs(title = "OverTime VS Attrition", x = "DistanceFromHome", y = "Ratio")
p_OverTime

#Relationship between BusinessTravel and Attrition
p_BusinessTravel <- ggplot(data = Attrition_data, aes(x = BusinessTravel)) +
  geom_bar(aes(fill = Attrition), position = "fill") +
  theme_economist() +
  labs(title = "BusinessTravel VS Attrition", x = "BusinessTravel", y = "Ratio") +
  theme(axis.text.x = element_text(angle = 90))
p_BusinessTravel

# Analysis result:
# Employees who work overtime are more likely to leave
# More likely to leave if the working distance is more than 11km
# Frequent business trips are easier to leave

#5. Exploratory analysis conclusion
#Based on the exploratory analysis of the data, there are many factors affecting employee turnover, including:
#The imbalance between work and life - overtime, distance from home, business trip,etc
#If the work input can not get the matching return, the employees are more inclined to leave;
#Benefits such as preferred stock subscription are the form of return that employees pay more attention to
#The factors of age and the number of companies employed will also affect the turnover rate of employees

#Training model
#1. Decision tree model
#(1) Variable sorting
Attrition_data_predicted <- Attrition_data %>%
  select(- EmployeeCount, -EmployeeNumber, -Over18, -StandardHours)
levels(Attrition_data$JobRole) <- c("HC", "HR", "Lab", "Man", "MDir", "RsD", "RsSci", "SlEx", "SlRep")
levels(Attrition_data$EducationField) <- c("HR", "LS", "MRK", "MED", "NA", "TD")
#(2) Split data
set.seed(3221)
n <- nrow(Attrition_data_predicted)
rnd <- sample(n, n * 0.7)
train <- Attrition_data_predicted[rnd, ]
test <- Attrition_data_predicted[- rnd, ]
#(3) Modeling
dtree <- rpart(Attrition ~ ., data = train)
test$Attrition <- as.factor(test$Attrition)
preds <- predict(dtree, test, type = "class")
rcov <- roc(as.numeric(test$Attrition), as.numeric(preds))

#Observe the feasibility of the model
rcov$auc
prop.table(table(test$Attrition, preds, dnn = c("Actual", "Predicted")),1)

#Ploting decision tree
dtreepr <- prune(dtree, cp = 0.01666667)
predspr <- predict(dtreepr, test, type = "class")
rocvpr <- roc(as.numeric(test$Attrition), as.numeric(predspr),smooth=TRUE)
rocvpr$auc
rpart.plot(dtreepr, 
           type = 4,
           tweak = 0.9,
           fallen.leaves = F,
           cex = 0.7)
plot(rocvpr,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),grid.col=c("green","red"),max.auc.polygon=TRUE,auc.polygon.col="skyblue",print.thres=TRUE)

#2. Random Forest model
set.seed(2343)
fit_forest <- randomForest(as.factor(Attrition) ~ ., data = train)
rfpreds <- predict(fit_forest, test, type = "class")
# Calculating AUC area
rocrf <- roc(as.numeric(test$Attrition), as.numeric(rfpreds))
rocrf$auc

#3. GBM()
set.seed(3433)
ctrl <- trainControl(method = "cv",
                     number = 10,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE) 
fit_gbm <- train(Attrition ~.,
                 data = train,
                 method = "gbm",
                 verbose = FALSE,
                 metric = "ROC",
                 trControl = ctrl)
gbmpreds <- predict(fit_gbm, test)
rocgbm <- roc(as.numeric(test$Attrition), as.numeric(gbmpreds))
rocgbm$auc

#4. Optimize GBM model
ctrl$seeds <- fit_gbm$control$seeds
#(1) Optimizing GBM model by weighting
model_weights <- ifelse(train$Attrition == "No", 
                        (1/table(train$Attrition)[1]),
                        (1/table(train$Attrition)[2])) 
weightedfit <- train(Attrition ~ .,
                     data = train,
                     method = "gbm",
                     verbose = FALSE,
                     weights = model_weights,
                     metric = "ROC", 
                     trControl = ctrl)
weightedpreds <- predict(weightedfit, test)
rocweight <- roc(as.numeric(test$Attrition), as.numeric(weightedpreds))
rocweight$auc

#(2) up-sampling
ctrl$sampling <- "up" 
set.seed(3433)
upfit <- train(Attrition ~.,
               data = train,
               method = "gbm",
               verbose = FALSE,
               metric = "ROC",
               trControl = ctrl)
uppreds <- predict(upfit, test)
rocup <- roc(as.numeric(test$Attrition), as.numeric(uppreds))
rocup$auc

#(3) down-sampling
ctrl$sampling <- "down"
set.seed(3433)
downfit <- train(Attrition ~.,
                 data = train,
                 method = "gbm",
                 verbose = FALSE,
                 metric = "ROC",
                 trControl = ctrl)
downpreds <- predict(downfit, test)
rocdown <- roc(as.numeric(test$Attrition), as.numeric(downpreds))
rocdown$auc

prop.table(table(test$Attrition, weightedpreds, dnn = c("Actual", "Predicted")),1)

# Model application
#1. Variable importance list
varImp(weightedfit)

#2. Prediction
weightedprobs <- predict(weightedfit, test, type = "prob")
test$Prediction <- weightedprobs$Yes
ggplot(data = test, aes(x = JobRole, y = Prediction)) +
  geom_boxplot(aes(fill = JobRole, alpha = 0.5)) +
  theme_economist() +
  labs(title = "Possibility of employees attrition in different departments", x = "Department", y = "Attrition Probility")
