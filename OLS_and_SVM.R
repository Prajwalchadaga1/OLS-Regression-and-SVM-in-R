# Setting the work directory and loading the values of the pollution CSv into a variable "data"
setwd("P:\\Spring 2020\\ML\\Classes\\Class 8")
data = read.csv("pollution.csv",header = T, sep =",")
head(data)

###############  Build a OLS Regression model  ##########################

ols_model <- lm(y~.,data)
summary(ols_model)

# Calculating Root Mean Squared Error
RMSE = function(error) { sqrt(mean(error^2)) }

ols_error = RMSE(ols_model$residuals)
ols_error

#Determining the correlation of the independent features with the dependent feature(phone price)
library(corrplot)
corr = round(cor(data), 5)
corr
corrplot(corr)
boxplot(data$y)
boxplot(data$ï..x1)
boxplot(data$x9)
boxplot(data$x2)


## assumptions regarding OLS
library(olsrr)
ols_plot_resid_qq(ols_model)
ols_test_normality(ols_model)
ols_plot_resid_fit(ols_model)

#find outliers
ols_plot_cooksd_bar(ols_model)
ols_plot_cooksd_chart(ols_model)



########  SVM  ##########
library(e1071)

split_data <- sample(nrow(data), nrow(data)*0.8)
Trainset <- data[split_data, ]
Testset <- data[-split_data, ]


#Build a Support Vector Model
svm_model = svm(y~.,data=Trainset)
summary(svm_model)



predicted_test <- predict(svm_model, Testset[,-16])
residuals_test = Testset$y - predicted_test
residuals_test
RMSE(residuals_test) 


#Tuning the SVM to obtain the best SVM and hence reduce the RMSE
svm_tune = tune(svm,y~.,data=data,ranges=list(epsilon=seq(0,1,0.1),cost=seq(1,10,1)))
best_svm_model = svm_tune$best.model
best_prediction = predict(best_svm_model,Testset[,-16])
best_prediction
residuals_bestmodel = Testset$y - best_prediction
residuals_bestmodel
RMSE(residuals_bestmodel)