
#Libraries
library(readr)
library(mice)
library(VIM)
library(dplyr)
library(randomForest)

#Data upload
titanic.train <- read_csv("Desktop/Data Science/kaggle/competitions/titanic/titanic/train.csv")
titanic.test <- read_csv("Desktop/Data Science/kaggle/competitions/titanic/titanic/test.csv")

#Creating Labels and preparing to join to data frames
titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE
titanic.test$Survived <- NA
titanic.full <- rbind(titanic.train, titanic.test)

#Replace NA to S in Embarked
titanic.full[is.na(titanic.full$Embarked), "Embarked"] <- 'S'

#Missing Data obervation in %
p<- function(x) {sum(is.na(x))/length(x)*100}
apply(titanic.full, 2, p)
md.pattern(titanic.full)

#Impute NA in Age
impute<-mice(titanic.full[ ,],m=3, seed = 123)
print(impute)
clean_df <- complete(impute, 1)
View(clean_df)

#Categorical casting

clean_df$Pclass <- as.factor(clean_df$Pclass)
clean_df$Sex <- as.factor(clean_df$Sex)
clean_df$Embarked <- as.factor(clean_df$Embarked)

#Data Partitioning

titanic.train <- clean_df[clean_df$IsTrainSet == TRUE,]
titanic.test <- clean_df[clean_df$IsTrainSet == FALSE,]

#Prediction using Random Forest
titanic.train$Survived <- as.factor(titanic.train$Survived)

survived.equation <- ("Survived ~ Pclass + Sex+ Age+ SibSp + Parch +Fare + Embarked")
survived.formula <- as.formula(survived.equation)

titanic.model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodsize = 0.01*nrow(titanic.train))
feature.equation  <- "Pclass + Sex+ Age+ SibSp + Parch +Fare + Embarked"

Survived <- predict(titanic.model, newdata = titanic.test)
Survived

PassengerId <- titanic.test$PassengerId
output.df<- as.data.frame(PassengerId)
output.df$Survived <- Survived

View(output.df)

write.csv(output.df, file = "kaggle_titanic.csv", row.names = FALSE)

