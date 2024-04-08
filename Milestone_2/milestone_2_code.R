##Load Libraries##
library(dplyr)
library(ggplot2)
library(tidyr)
library(glmnet)
library(tidyverse)
library(caret)
library(ISLR)

library(ggplot2)
library(gridExtra)
library(rpart)
library(png)

library(caTools)
library(car)
library(glmnet)
library(gmodels)
library(e1071)

########################################################

#Import dataset and create a subset that removes NA cases
dataset <- read.csv("cleaned_dataset.csv")

########################################################
# Section1: Linear Regression
#######################################################
## Cleaning the data

# Look at the variable's name
names(dataset)

# Remove the unnecessary variables and only select the design feature variable and turbine capacity parameter
tmp <- dataset %>% select(t_cap, t_hh, t_rd, t_ttlh)

# Remove the NA
tmp <- tmp %>%  filter(!is.na(t_cap)& !is.na(t_hh) & !is.na(t_rd)& !is.na(t_ttlh))

# Let's find out how many records were eliminated during the cleaning process.
nrow(tmp)

# Calculate the percentage of data removed
percent_removed <- round((1 - 68943 / 73352) * 100,1)

# Print the result
print(paste("Percentage of data removed =", percent_removed, "%"))

# Let's standrilize the variables
tmp.st <- as.data.frame(scale(tmp))

# Viewing the first few rows of the standardized data
head(tmp.st)

# View the summary
summary(tmp.st)


################################


## Plot each independent variable


# Reshape the data
tmp_long <- tmp.st %>%
  pivot_longer(cols = c(t_hh, t_rd, t_ttlh), names_to = "Variable", values_to = "Value")

# Now, create the facet plot
ggplot(tmp_long, aes(x = Value, y = t_cap)) +
  geom_point(aes(color=Variable)) + # Use geom_line() or another geom_*() function as needed
  facet_wrap(~ Variable, scales = "free_x") +
  labs(x = "Design Feature value", y = "Turbine Capacity (kW)", title = "Plot of turbine capacity (kW) vs. Design Features ") +
  theme_minimal()+
  scale_color_discrete(name = "Design Feature", 
                       labels = c("hub height (m)", 
                                  "rotor diameter (m)", 
                                  "total height (m)"))



#######################################

## a.compute a linear regression with respect to the dependent feature


#Conduct linear regression considering t-hh, t-rd, t-ttlh as independent variables and t-cap as dependent variable.
#Set a seed
set.seed(1)

# Randomly divide the data into training and testing sets
# Calculate indices for the training set, capturing 70% of the data
train_insts <- sample (nrow(tmp.st), 0.7*nrow(tmp.st))

# Create the training dataset using the selected indices
tmp_train <- tmp.st[train_insts,]

# Create the testing dataset using the remaining indices
tmp_test <- tmp.st[-train_insts,]



# Conduct regression analysis
model1.lm <- lm(t_cap~ t_hh , data =tmp_train )
model2.lm <- lm(t_cap~ t_rd , data =tmp_train)
model3.lm <- lm(t_cap~ t_ttlh, data =tmp_train)



# look at the summary of regression
summary(model1.lm)
summary(model2.lm)
summary(model3.lm)



# fit the regression line for t_hh
plot(tmp.st$t_cap, tmp.st$t_hh, xlab="Turbin Hub Height (m)", ylab="Turbine Capacity (kw)")
abline(model1.lm, col="red", lwd=3) 


# fit the regression line for t_rd
plot(tmp.st$t_cap, tmp.st$t_rd, xlab="Turbin Rotor diameter (m)" , ylab="Turbine Capacity (kw)")
abline(model2.lm, col="green", lwd=3) 

# fit the regression line for t_ttlh
plot(tmp.st$t_cap, tmp.st$t_ttlh, xlab="Turbin Total Height (m)" , ylab="Turbine Capacity (kw)")
abline(model3.lm, col="blue", lwd=3) 



#let's predict the t_cap by using the test data for each prediction model
# create a predict data
m1_predict <- predict (model1.lm, newdata=tmp_test)
m2_predict <- predict (model2.lm, newdata=tmp_test)
m3_predict <- predict (model3.lm, newdata=tmp_test)


# calculate the correlation between the predicted and real values 
#Check for the correlation with actual result for t_hh
cor_thh <- cor(m1_predict, tmp_test$t_cap)
cor_thh

#Check for the correlation with actual result for t_rd
cor_trd <- cor(m2_predict, tmp_test$t_cap)
cor_trd

#Check for the correlation with actual result for t_hhlt
cor_thhlt <- cor(m3_predict, tmp_test$t_cap)
cor_thhlt


# Calculate the mean square error between the two
#Check for the mean square error with actual result for t_hh
m1_RMSE <- sqrt(mean((m1_predict-tmp_test$t_cap)^2))
m1_RMSE

#Check for the mean square error with actual result for t_rd
m2_RMSE <- sqrt(mean((m2_predict-tmp_test$t_cap)^2))
m2_RMSE

#Check for the mean square error with actual result for t_hhlt
m3_RMSE <- sqrt(mean((m3_predict-tmp_test$t_cap)^2))
m3_RMSE


#Show results together with confidence and prediction bands
# plot the qqnorm and qqline for t_hh variable
qqnorm(m1_predict-tmp_test$t_cap ,main = "Normal Q-Q Plot for Turbine Hub Height")
qqline(m1_predict-tmp_test$t_cap, col="red")



# Get predictions with confidence and prediction intervals for t_hh variable
pc_thh <- predict(model1.lm, interval = "confidence", newdata = tmp_test[order(tmp_test$t_hh), ])
pp_thh <- predict(model1.lm, interval = "prediction", newdata = tmp_test[order(tmp_test$t_hh), ])

# Plot the original data
plot(tmp_test$t_hh, tmp_test$t_cap, xlab = "Turbin hub height", ylab = "Turbine Capacity",
     main = "Confidence and Prediction Bands", pch = 19)

# Add the regression line
lines(tmp_test$t_hh[order(tmp_test$t_hh)], pc_thh[, "fit"], col = "black")

# Add the confidence band
lines(tmp_test$t_hh[order(tmp_test$t_hh)], pc_thh[, "lwr"], col = "red", lty = "dashed")
lines(tmp_test$t_hh[order(tmp_test$t_hh)], pc_thh[, "upr"], col = "red", lty = "dashed")

# Add the prediction band
lines(tmp_test$t_hh[order(tmp_test$t_hh)], pp_thh[, "lwr"], col = "green", lty = "dashed")
lines(tmp_test$t_hh[order(tmp_test$t_hh)], pp_thh[, "upr"], col = "green", lty = "dashed")


#Show results together with confidence and prediction bands
# plot the qqnorm and qqline for t_rd variable
qqnorm(m2_predict-tmp_test$t_cap ,main = "Normal Q-Q Plot for Turbine Rotor Diameter ")
qqline(m2_predict-tmp_test$t_cap, col="green")


# Get predictions with confidence and prediction intervals for t_rd variable
pc_trd <- predict(model2.lm, interval = "confidence", newdata = tmp_test[order(tmp_test$t_rd), ])
pp_trd <- predict(model2.lm, interval = "prediction", newdata = tmp_test[order(tmp_test$t_rd), ])

# Plot the original data
plot(tmp_test$t_rd, tmp_test$t_cap, xlab = "Turbin Rotor Diameter", ylab = "Turbine Capacity",
     main = "Confidence and Prediction Bands", pch = 19)

# Add the regression line
lines(tmp_test$t_rd[order(tmp_test$t_rd)], pc_trd[, "fit"], col = "black")

# Add the confidence band
lines(tmp_test$t_rd[order(tmp_test$t_rd)], pc_trd[, "lwr"], col = "red", lty = "dashed")
lines(tmp_test$t_rd[order(tmp_test$t_rd)], pc_trd[, "upr"], col = "red", lty = "dashed")

# Add the prediction band
lines(tmp_test$t_rd[order(tmp_test$t_rd)], pp_trd[, "lwr"], col = "green", lty = "dashed")
lines(tmp_test$t_rd[order(tmp_test$t_rd)], pp_trd[, "upr"], col = "green", lty = "dashed")


#Show results together with confidence and prediction bands
# plot the qqnorm and qqline for t_ttlh variable
qqnorm(m3_predict-tmp_test$t_cap ,main = "Normal Q-Q Plot for Turbine Total Height ")
qqline(m3_predict-tmp_test$t_cap, col="blue")


# Order tmp_test by t_ttlh
ordered_test <- tmp_test[order(tmp_test$t_ttlh), ]

# Get predictions with confidence and prediction intervals for t_ttlh variable
pc_ttlh <- predict(model3.lm, interval = "confidence", newdata = ordered_test)
pp_ttlh <- predict(model3.lm, interval = "prediction", newdata = ordered_test)

# Plot the original data
plot(ordered_test$t_ttlh, ordered_test$t_cap, xlab = "Turbine Total Height", ylab = "Turbine Capacity",
     main = "Confidence and Prediction Bands", pch = 19)

# Add the regression line
lines(ordered_test$t_ttlh, pc_ttlh[, "fit"], col = "black")

# Add the confidence band
lines(ordered_test$t_ttlh, pc_ttlh[, "lwr"], col = "red", lty = "dashed")
lines(ordered_test$t_ttlh, pc_ttlh[, "upr"], col = "red", lty = "dashed")

# Add the prediction band
lines(ordered_test$t_ttlh, pp_ttlh[, "lwr"], col = "green", lty = "dashed")
lines(ordered_test$t_ttlh, pp_ttlh[, "upr"], col = "green", lty = "dashed")


####################################

# Multivariate regressions

## b.Show whether considering combinations of independent features improves the prediction results. 



# Conduct multivariate regression analysis
model12.lm <- lm(t_cap~ t_hh+t_rd , data =tmp_train )
model13.lm <- lm(t_cap~ t_hh+t_ttlh , data =tmp_train)
model23.lm <- lm(t_cap~ t_rd+t_ttlh, data =tmp_train)
model123.lm <- lm(t_cap~ t_hh+t_rd+t_ttlh, data=tmp_train)


# look at the summary of regression
summary(model12.lm)
summary(model13.lm)
summary(model23.lm)
summary(model123.lm)



#let's predict the t_cap by using the test data for each prediction model
# create a predict data
m12_predict <- predict (model12.lm, newdata=tmp_test)
m13_predict <- predict (model13.lm, newdata=tmp_test)
m23_predict <- predict (model23.lm, newdata=tmp_test)
m123_predict <- predict (model123.lm, newdata=tmp_test)



# calculate the correlation between the predicted and real values 
#Check for the correlation with actual result for t_hh, t_rd
cor12 <- cor(m12_predict, tmp_test$t_cap)
cor12

#Check for the correlation with actual result for t_hh, t_ttlh
cor13 <- cor(m13_predict, tmp_test$t_cap)
cor13

#Check for the correlation with actual result for t_ttlh, t-rd
cor23 <- cor(m23_predict, tmp_test$t_cap)
cor23

#Check for the correlation with actual result for t_ttlh, t-rd, t_hh
cor123 <- cor(m123_predict, tmp_test$t_cap)
cor123


# Calculate the mean square error between the two
#Check for the mean square error with actual result for t_hh, t-rd
m12_RMSE <- sqrt(mean((m12_predict-tmp_test$t_cap)^2))
m12_RMSE

#Check for the mean square error with actual result for t_hh, t_ttlh
m13_RMSE <- sqrt(mean((m13_predict-tmp_test$t_cap)^2))
m13_RMSE

#Check for the mean square error with actual result for t_ttlh, t_rd
m23_RMSE <- sqrt(mean((m23_predict-tmp_test$t_cap)^2))
m23_RMSE

#Check for the mean square error with actual result for t_ttlh, t_rd, t_hh
m123_RMSE <- sqrt(mean((m123_predict-tmp_test$t_cap)^2))
m123_RMSE

# Calculate the residual of the model
rs12 <- residuals(model12.lm)
qqnorm(rs12 ,main = "Normal Q-Q Plot for Turbine Hub Height & Rotor Diameter")
qqline(rs12, col="red")


# Calculate the residual of the model
rs13 <- residuals(model13.lm)
qqnorm(rs13 ,main = "Normal Q-Q Plot for Turbine Hub Height & Tip Height")
qqline(rs13, col="red")




# Calculate the residual of the model
rs23 <- residuals(model23.lm)
qqnorm(rs23 ,main = "Normal Q-Q Plot for Turbine Rotor Diameter & Tip Height")
qqline(rs23, col="red")



# Calculate the residual of the model
rs123 <- residuals(model123.lm)
qqnorm(rs123 ,main = "Normal Q-Q Plot for hub Height & Rotor Diameter & Tip Height")
qqline(rs123, col="red")



####################################

#  Regularization

## c.Repeat experiments in (a) and (b) adding regularization.

### Compute a linear regression with respect to the dependent feature cosidering alpha=1


# Conduct regularization analysis
# considering t-hh and t_rd
cv_fit12 <- cv.glmnet(as.matrix(tmp_train[,c(-1,-4)]), as.vector(tmp_train[,1]), alpha = 1)
plot(cv_fit12)

# calculate the coefficients of cv_fit considering t_hh and t_rd variables
coef(cv_fit12)

# Check the lambda value
cv_fit12_lambda <-cv_fit12$lambda.min
cv_fit12_lambda

# Predict the t-cap considering cv_fit12
model12.re <- predict (cv_fit12, newx=as.matrix(tmp_test[,c(-1,-4)]))

# correlation to check result
cor(model12.re, as.vector(tmp_test[,1]))

#mean square error to check result
sqrt(mean((model12.re-tmp_test$t_cap)^2))


# Conduct regularization analysis
# considering t-hh and t_ttlh
cv_fit13 <- cv.glmnet(as.matrix(tmp_train[,c(-1,-3)]), as.vector(tmp_train[,1]), alpha = 1)
plot(cv_fit13)



# calculate the coefficients of cv_fit considering t_hh and t_ttlh variables
coef(cv_fit13)

# Check the lambda value
cv_fit13_lambda <-cv_fit13$lambda.min
cv_fit13_lambda

# Predict the t-cap considering cv_fit13
model13.re <- predict (cv_fit13, newx=as.matrix(tmp_test[,c(-1,-3)]))

# correlation or mean square error to check result
cor(model13.re, as.vector(tmp_test[,1]))

#mean square error to check result
sqrt(mean((model13.re-tmp_test$t_cap)^2))

# Conduct regularization analysis
# considering t-rd and t_ttlh
cv_fit23 <- cv.glmnet(as.matrix(tmp_train[,c(-1,-2)]), as.vector(tmp_train[,1]), alpha = 1)
plot(cv_fit23)



# calculate the coefficients of cv_fit considering t_rd and t_ttlh variables
coef(cv_fit23)

# Check the lambda value
cv_fit23_lambda <-cv_fit23$lambda.min
cv_fit23_lambda

# Predict the t-cap considering cv_fit23
model23.re <- predict (cv_fit23, newx=as.matrix(tmp_test[,c(-1,-2)]))

# correlation or mean square error to check result
cor(model23.re, as.vector(tmp_test[,1]))

#mean square error to check result
sqrt(mean((model23.re-tmp_test$t_cap)^2))



# Conduct regularization analysis
# considering t_hh and t-rd and t_ttlh
cv_fit123 <- cv.glmnet(as.matrix(tmp_train[,c(-1)]), as.vector(tmp_train[,1]), alpha = 1)
plot(cv_fit123)


# calculate the coefficients of cv_fit considering t_hh and t_rd and t_ttlh variables
coef(cv_fit123)

# Check the lambda value
cv_fit123_lambda <-cv_fit123$lambda.min
cv_fit123_lambda

# Predict the t-cap considering cv_fit123
model123.re <- predict (cv_fit123, newx=as.matrix(tmp_test[,c(-1)]))

# correlation or mean square error to check result
cor(model123.re, as.vector(tmp_test[,1]))

#mean square error to check result
sqrt(mean((model123.re-tmp_test$t_cap)^2))


#####################################################

### Compute a linear regression with respect to the dependent feature cosidering alpha=2


# Conduct regularization analysis
# considering t-hh and t_rd
cv_fit12_alpha2 <- cv.glmnet(as.matrix(tmp_train[,c(-1,-4)]), as.vector(tmp_train[,1]), alpha = 2)
plot(cv_fit12_alpha2)



# calculate the coefficients of cv_fit considering t_hh and t_rd variables
coef(cv_fit12_alpha2)

# Check the lambda value
cv_fit12_lambda <-cv_fit12_alpha2$lambda.min
cv_fit12_lambda

# Predict the t-cap considering cv_fit12
model12.re <- predict (cv_fit12_alpha2, newx=as.matrix(tmp_test[,c(-1,-4)]))

# correlation or mean square error to check result
cor(model12.re, as.vector(tmp_test[,1]))

#mean square error to check result
sqrt(mean((model12.re-tmp_test$t_cap)^2))




# Conduct regularization analysis
# considering t-hh and t_ttlh
cv_fit13_alpha2 <- cv.glmnet(as.matrix(tmp_train[,c(-1,-3)]), as.vector(tmp_train[,1]), alpha = 2)
plot(cv_fit13_alpha2)



# calculate the coefficients of cv_fit considering t_hh and t_ttlh variables
coef(cv_fit13_alpha2)

# Check the lambda value
cv_fit13_lambda <-cv_fit13_alpha2$lambda.min
cv_fit13_lambda

# Predict the t-cap considering cv_fit13
model13.re <- predict (cv_fit13_alpha2, newx=as.matrix(tmp_test[,c(-1,-3)]))

# correlation or mean square error to check result
cor(model13.re, as.vector(tmp_test[,1]))

#mean square error to check result
sqrt(mean((model13.re-tmp_test$t_cap)^2))


# Conduct regularization analysis
# considering t-rd and t_ttlh
cv_fit23_alpha2 <- cv.glmnet(as.matrix(tmp_train[,c(-1,-2)]), as.vector(tmp_train[,1]), alpha = 2)
plot(cv_fit23_alpha2)



# calculate the coefficients of cv_fit considering t_rd and t_ttlh variables
coef(cv_fit23_alpha2)

# Check the lambda value
cv_fit23_lambda <-cv_fit23_alpha2$lambda.min
cv_fit23_lambda

# Predict the t-cap considering cv_fit23
model23.re <- predict (cv_fit23_alpha2, newx=as.matrix(tmp_test[,c(-1,-2)]))

# correlation or mean square error to check result
cor(model23.re, as.vector(tmp_test[,1]))

#mean square error to check result
sqrt(mean((model23.re-tmp_test$t_cap)^2))





# Conduct regularization analysis
# considering t_hh and t-rd and t_ttlh
cv_fit123_alpha2 <- cv.glmnet(as.matrix(tmp_train[,c(-1)]), as.vector(tmp_train[,1]), alpha = 2)
plot(cv_fit123_alpha2)



# calculate the coefficients of cv_fit considering t_hh and t_rd and t_ttlh variables
coef(cv_fit123_alpha2)

# Check the lambda value
cv_fit123_lambda <-cv_fit123_alpha2$lambda.min
cv_fit123_lambda

# Predict the t-cap considering cv_fit123
model123.re <- predict (cv_fit123_alpha2, newx=as.matrix(tmp_test[,c(-1)]))

# correlation or mean square error to check result
cor(model123.re, as.vector(tmp_test[,1]))

#mean square error to check result
sqrt(mean((model123.re-tmp_test$t_cap)^2))


#######################################################
# d. Repeat a-c multiple times with different randomly selected training and testing sets and report differences or similarities across runs.
# defining training control as cross-validation and  
# value of K equal to 5
train_control <- trainControl(method = "cv", 
                              number = 5) 

# Conduct regression analysis considering k=5

model1.lm <- train(t_cap~ t_hh , data =tmp.st, method="lm", trControl=train_control )
model2.lm <- train(t_cap~ t_rd , data =tmp.st, method="lm", trControl=train_control)
model3.lm <- train(t_cap~ t_ttlh, data =tmp.st, method="lm", trControl=train_control)

# see the result
print(model1.lm)
print(model2.lm)
print(model3.lm)



# training the model by assigning sales column as dependent variable and rest other column as independent variable 

model12.lm <- train(t_cap~ t_hh+t_rd , data =tmp.st, method="lm", trControl=train_control)
model13.lm <- train(t_cap~ t_hh+t_ttlh , data =tmp.st, method="lm", trControl=train_control)
model23.lm <- train(t_cap~ t_rd+t_ttlh, data =tmp.st, method="lm", trControl=train_control)
model123.lm <- train(t_cap~ t_hh+t_rd+t_ttlh, data =tmp.st, method="lm", trControl=train_control)

print(model12.lm)
print(model13.lm)
print(model23.lm)
print(model123.lm)





# training the model by assigning sales column as dependent variable and rest other column as independent variable 

model12.lasso <- train(t_cap~ t_hh+t_rd ,  data = tmp.st, method = "glmnet",
                       trControl = train_control,
                       tuneGrid = expand.grid(alpha = 1, lambda=0.1))

model13.lasso <- train(t_cap~ t_hh+t_ttlh ,  data = tmp.st, method = "glmnet",
                       trControl = train_control,
                       tuneGrid = expand.grid(alpha = 1, lambda=0.1))

model23.lasso <- train(t_cap~ t_rd+t_ttlh,  data = tmp.st, method = "glmnet",
                       trControl = train_control,
                       tuneGrid = expand.grid(alpha = 1, lambda=0.1))

model123.lasso <- train(t_cap~ t_hh+t_rd+t_ttlh,  data = tmp.st, method = "glmnet",
                        trControl = train_control,
                        tuneGrid = expand.grid(alpha = 1, lambda=0.1))

print(model12.lasso)
print(model13.lasso)
print(model23.lasso)
print(model123.lasso)


# For Ridge regression (alpha = 0)
model12.ridge <- train(t_cap ~ t_hh + t_rd, data = tmp.st, method = "glmnet",
                       trControl = train_control,
                       tuneGrid = expand.grid(alpha = 0, lambda = 0.1))  

model13.ridge <- train(t_cap ~ t_hh + t_ttlh, data = tmp.st, method = "glmnet",
                       trControl = train_control,
                       tuneGrid = expand.grid(alpha = 0, lambda = 0.1))  

model23.ridge <- train(t_cap ~ t_rd + t_ttlh, data = tmp.st, method = "glmnet",
                       trControl = train_control,
                       tuneGrid = expand.grid(alpha = 0, lambda = 0.1)) 

model123.ridge <- train(t_cap ~ t_hh + t_rd + t_ttlh, data = tmp.st, method = "glmnet",
                        trControl = train_control,
                        tuneGrid = expand.grid(alpha = 0, lambda = 0.1))  

print(model12.ridge)
print(model13.ridge)
print(model23.ridge)
print(model123.ridge)


#####################################################
## Section 2:Log Regression & Naive Bayes
#####################################################

#Import dataset and create a subset that removes NA cases
complete_data <- complete.cases(dataset)
data_subset <- dataset[complete_data,]

#Create a clean data frame with the main variables
clean_subset <- data_subset[,c("t_cap", "t_hh","t_rd","t_rsa","t_ttlh")]
#Convert the dependent variable into a categorical → above >1700 is a high performing 1, <1700 is not 0
clean_subset$t_cap_bin <- ifelse(clean_subset$t_cap > 1700, 1,0)
#Standardize dataset
standard <- scale(clean_subset)

#Randomize and create training (75%) and testing (25%) sets
set.seed(123)
split <- sample.split(clean_subset$t_cap_bin, SplitRatio <- 0.75)
training_set <- subset(clean_subset, split ==TRUE)
test_set <- subset(clean_subset, split ==FALSE)
#Standardize training dataset
stand <- scale(training_set[, -which(names(training_set) == "t_cap_bin")])

#Run Lasso (alpha =1) regression to minimize multicollinearity and extract coefficients
cv_fit <- cv.glmnet(as.matrix(stand), training_set$t_cap_bin, alpha = 1)
min_l <- cv_fit$lambda.min
lasso_coef <- coef(cv_fit, s=min_l)[-1] 

#Build log regression model using training set
classifier <- glm(t_cap_bin~.-t_cap, family = binomial,data =training_set)
#Extract original coefficients from log model
log_coef <- coef(classifier)
#Replace original coeffs with Lasso coefficients to instantiate
classifier$coefficients <- c(log_coef[1], lasso_coef)

#Check statistical significance
summary(classifier)

#Exponentiate coefficients to interpret as odd-ratios
exp(coef(classifier))

#Use the predict function to generate predicted probabilities of the dependent variable using the log model
prob_pred <- predict(classifier,type="response",newdata = test_set)

#Convert probabilities to 1 and 0 if larger than 0.5 probability 
y_pred <- ifelse(prob_pred > 0.5, 1,0)

#Use a confusion matrix to evaluate quality of predicted probabilities
CrossTable(test_set[,6], y_pred,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted','actual'))

#Naive Bayes Code
# Convert t_cap_bin to a factor with levels "No" and "Yes"
clean_subset$t_cap_bin <- factor(clean_subset$t_cap_bin, levels = c(0, 1), labels = c("No", "Yes"))

# Convert across previously defined training and testing sets from log regression
training_set$t_cap_bin <- factor(training_set$t_cap_bin, levels = c(0, 1), labels = c("No", "Yes"))
test_set$t_cap_bin <- factor(test_set$t_cap_bin, levels = c(0, 1), labels = c("No", "Yes"))

# Train the NB Classifier, remove first column of original t_cap values
nb_classifer <- naiveBayes(training_set[,-1],training_set$t_cap_bin)

#Predict on testing set
nb_predict <- predict(nb_classifier, newdata = testing_set)

# evaluate using Confusion Matrix
CrossTable(nb_predict, test_set$t_cap_bin,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))

#Create a 2nd classifier to run the NB with a laplace estimator. Use this classifier to predict on testing set. Run a confusion matrix to evaluate accuracy.
nb_classifer2 <- naiveBayes(training_set[,-1],training_set$t_cap_bin,laplace = 1)
nb_predict2 <- predict(nb_classifer2, newdata= test_set)
CrossTable(nb_predict2, test_set$t_cap_bin,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))

#####################################################
## Section 3:Decision Trees and Random Forests
#####################################################

# Reading in dataset
data <- read.csv("datasets/cleaned_dataset.csv")

# Generate random indices to shuffle the dataset
set.seed(123)  # for reproducibility
random_indices <- sample(nrow(data))

# Shuffle the dataset
shuffled_data <- data[random_indices, ]

train_proportion <- 0.8  # 80% for training, 20% for testing

# Calculate the number of samples for training and testing
num_train_samples <- round(train_proportion * nrow(shuffled_data))
num_test_samples <- nrow(shuffled_data) - num_train_samples

# Split the shuffled dataset into training and testing sets
train_data <- shuffled_data[1:num_train_samples, ]
test_data <- shuffled_data[(num_train_samples + 1):nrow(shuffled_data), ]

# Define a function to plot histograms and density plots
plot_distribution <- function(data, title) {
  ggplot(data, aes(x = t_cap)) +
    geom_histogram(fill = "blue", color = "black", bins = 20) +
    labs(title = title, x = "Turbine Capacity", y = "Frequency")
}

# Plot density plots for the original dataset, training set, and testing set
original_plot <- plot_distribution(data, "Distribution of Original Dataset")
training_plot <- plot_distribution(train_data, "Distribution of Training Set")
testing_plot <- plot_distribution(test_data, "Distribution of Testing Set")

# Combine the plots
png(filename = "generated_graphs/training_testing_distributions_dt.png", width = 800, height = 600) ## Saving png
grid.arrange(original_plot, training_plot, testing_plot,
             ncol = 3)
dev.off()
# Check the distribution of the training and testing sets
summary(train_data)
summary(test_data)

# Train Regression tree
r_tree_model <- rpart(t_cap ~ t_hh + t_rd + t_rsa + t_ttlh , data = train_data)  # Assuming t_cap is continuous, no need to specify method

# Print decision tree rules
print(r_tree_model)

# Visualize decision tree
plot(r_tree_model)
text(r_tree_model)

## add code for boosting 
## add decision tree

