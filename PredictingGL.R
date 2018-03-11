#installing packages required for analysis library(caret)
library(dplyr)
library(stargazer)
#setting working directory and reading in the data setwd("/Users/charlielundquist/downloads")
df <- read.csv("datafake.csv")
View(df)
#splitting data randomly into train and test sets (2/3 and 1/3 respectively)
set.seed(100)
trainIndex <- createDataPartition(df$Backtograde, p = .67,
                                  list = FALSE, times = 1)
Train <- df[ trainIndex,] Test <- df[-trainIndex,]
View(Train)
#creating the first model and summarizing the results
#dependent variable: Back to Grade Level at end of year (binomial- yes or no) #independent variables: Number of scheduled sessions per week (1 or 2), Grade (1- 4),
#Gender (male or female), ELL Status (yes or no), Race (white, latinx, african american, other)
model <- glm(Backtograde ~ Gap + NumSessWk +
               Grade + Gender + EllStat + Race, data = Train, family = binomial())
summary(model)
#using the model to calculate the probability of reaching grade for every row in the dataset
Test$model_prob <- predict(model, Test, type = "response") View(Test)
#Creating new binary variable using the predictions calculated for each row above #I.E. - 1 if probability is > or = to 0.5, ifelse 0.
#Also creating new variable 'b2gbinary' to read 1 for every student with a 'yes' for #Backtograde variable, ifelse 0
Test <- Test %>% mutate(model_pred = 1*(model_prob > .49) + 0, b2gbinary = 1*(Backtograde == "yes") + 0)
View(Test)
#Using the two new variables 'model_pred' and 'b2gbinary' to test the accuracy of the model
Test <- Test %>% mutate(accurate = 1*(model_pred == b2gbinary)) sum(Test$accurate)/nrow(Test)
#dropping unassociated variables to increase model reliability (all variables with #insignificant p-values (other than grade) are dropped).
#Copying code above but using with a model that uses variables: grade, number of sessions
#per week, and first quarter gp - ge.
set.seed(101)
trainIndex <- createDataPartition(df$Backtograde, p = .67,
                                  list = FALSE, times = 1)
Train2 <- df[ trainIndex,] Test2 <- df[-trainIndex,]
model2 <- glm(Backtograde ~ Gap + NumSessWk + Grade, data = Train2, family = binomial())
summary(model2)
Test2$model_prob <- predict(model2, Test2, type = "response")
Test2 <- Test2 %>% mutate(model_pred = 1*(model_prob > .49) + 0, b2gbinary = 1*(Backtograde == "yes") + 0)
Test2 <- Test2 %>% mutate(accurate = 1*(model_pred == b2gbinary))
sum(Test2$accurate)/nrow(Test2)
#making a logistic regression table stargazer(model2,type = "html",
title = "Grade Level Standard Met? Follow Equation Below", out="reg.html",dep.var.labels = "Error: Brain Break Needed", dep.var.caption = "logit(p) = log(p/(1-p))= β0 + β1*x1 + ... + βk*xk", covariate.labels = c("Q1 GP - GE", "Number of Sessions Per Week","Grade"), report=('vc*p'))
#using the model to make new predictions
df2 <- read.csv("newdat.csv")
df2$prob <- predict(model2,df2,type="response")
View(df2)