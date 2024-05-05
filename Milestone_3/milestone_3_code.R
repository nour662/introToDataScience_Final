##Load Libraries##
library(dplyr)
library(ggplot2)
library(kernlab)
library(caret)
library(randomForest)

########################################################

#Import dataset and create a subset that removes NA cases
dataset <- read.csv("cleaned_dataset.csv")

########################################################
# Section 1: 
#######################################################
#a.Train and test a linear SVM model
####################################
  
# check the structure of the data
str(dataset)

# Look at the variable's name
names(dataset)

# Remove the unnecessary variables and only select the design feature variable and turbine capacity parameter
tmp <- dataset %>% select(t_cap, t_hh, t_rd, t_ttlh)

# Remove the NA
tmp <- tmp %>%  filter(!is.na(t_cap)& !is.na(t_hh) & !is.na(t_rd)& !is.na(t_ttlh)) 

# apply normalization to entire data frame

tmp <- as.data.frame(scale(tmp))

# Define train and test set
set.seed(123)
trainIndex <- createDataPartition(tmp$t_cap, p=0.75, list=FALSE, times = 1)
train_set <- tmp[trainIndex,]
test_set <- tmp[-trainIndex,]


# Develope the SVMs classifier
capacity_classifier <- ksvm(t_cap~., data=train_set,type = "eps-svr", kernel="vanilladot")
print(capacity_classifier)


# Test the model
capacity_predict <- predict(capacity_classifier, test_set)
head(capacity_predict)

# Calculate RMSE
rmse <- sqrt(mean((capacity_predict - test_set$t_cap)^2))

# Print RMSE
print(rmse)


# Print the correlation
cor(capacity_predict, test_set$t_cap)

####################################
#b.Kernels
####################################

# Develop a classifier based on radial basis function kernel
capacity_classifier_rfb <- ksvm(t_cap~., data=train_set, type="eps-svr", kernel="rbfdot")
print(capacity_classifier_rfb)

# Test the model
capacity_predict_rfb <- predict(capacity_classifier_rfb, test_set)

# Calculate RMSE
rmse_rfb <- sqrt(mean((capacity_predict_rfb - test_set$t_cap)^2))

# Print RMSE
print(rmse_rfb)

# Calculate the correlation
cor(capacity_predict_rfb, test_set$t_cap)

# Develop a classifier based on polynomial function kernel
capacity_classifier_poly <- ksvm(t_cap~., data=train_set, type="eps-svr", kernel="polydot")
print(capacity_classifier_poly)

# Test the model
capacity_predict_poly <- predict(capacity_classifier_poly, test_set)

# Calculate RMSE
rmse_poly <- sqrt(mean((capacity_predict_poly - test_set$t_cap)^2))

# Print RMSE
print(rmse_poly)

# Print Correlation
cor(capacity_predict_poly, test_set$t_cap)

########################################################
# Section 2: Neural Networks
#######################################################

#####################################################
## Section 3: 
#####################################################
## Section 4: Comparative Analysis 
#####################################################

# Categorize the dataset based on t_capacity quantile

# Remove NA values from tmp
tmp <- na.omit(tmp)

# Calculate quantiles
quantiles <- quantile(tmp$t_cap, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

# Initialize the new column with default value
tmp$quantile_label <- 4  # Initialize with the highest category

# Assign labels based on quantile ranges
tmp$quantile_label[tmp$t_cap <= quantiles[2]] <- 1  # First quartile
tmp$quantile_label[tmp$t_cap > quantiles[2] & tmp$t_cap <= quantiles[3]] <- 2  # Second quartile
tmp$quantile_label[tmp$t_cap > quantiles[3] & tmp$t_cap <= quantiles[4]] <- 3  # Third quartile
# Values above the third quartile threshold remain labeled as 4

# Convert quantile_label to a factor
tmp$quantile_label <- factor(tmp$quantile_label)
#####################################################
# 1. Conduct Repeated k-fold Cross validation
control_RK <- trainControl(method="repeatedcv", number=3, repeats = 3)

#set the seed
set.seed(123)


# Set the SVM model
modelSVM_RK <- train(quantile_label~t_hh+t_rd+t_ttlh, data=tmp, method="svmRadial", trControl=control_RK)

# set the RF model
modelRF_RK <- train(quantile_label~t_hh+t_rd+t_ttlh, data=tmp, method="rf", trControl=control_RK)

# set the NN model
modelNN_Rk <- train(quantile_label~t_hh+t_rd+t_ttlh, data=tmp, method="nnet", trControl=control_RK)

# Compare the performance
results_RK <- resamples(list(RF=modelRF_RK, SVM=modelSVM_RK, NN=modelNN_Rk))
summary(results_RK)

#box plot
bwplot(results_RK)

# dot plots of results
dotplot(results_RK)

####################################################
# 2. Conduct k-fold Cross validation
control_K <- trainControl(method="cv", number=3)

# Set the SVM model
modelSVM_K <- train(quantile_label~t_hh+t_rd+t_ttlh, data=tmp, method="svmRadial", trControl=control_K)

# set the RF model
modelRF_K <- train(quantile_label~t_hh+t_rd+t_ttlh, data=tmp, method="rf", trControl=control_K)

# set the NN model
modelNN_k <- train(quantile_label~t_hh+t_rd+t_ttlh, data=tmp, method="nnet", trControl=control_K)

# Compare the performance
results_K <- resamples(list(RF=modelRF_K , SVM=modelSVM_K, NN=modelNN_k))
summary(results_K)

#box plot
bwplot(results_K)

# dot plots of results
dotplot(results_K)

#####################################################
# 3. Bootstrapping
control_BT <- trainControl(method="boot", number=3)


# Set the SVM model
modelSVM_BT <- train(quantile_label~t_hh+t_rd+t_ttlh, data=tmp, method="svmRadial", trControl=control_BT)

# set the RF model
modelRF_BT <- train(quantile_label~t_hh+t_rd+t_ttlh, data=tmp, method="rf", trControl=control_BT)

# set the NN model
modelNN_BT <- train(quantile_label~t_hh+t_rd+t_ttlh, data=tmp, method="nnet", trControl=control_BT)

# Compare the performance
results_BT <- resamples(list(RF=modelRF_BT, SVM=modelSVM_BT, NN=modelNN_BT))
summary(results_BT)

#box plot
bwplot(results_BT)

# dot plots of results
dotplot(results_BT)
############################################################
