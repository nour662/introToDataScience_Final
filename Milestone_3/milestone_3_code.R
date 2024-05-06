##Load Libraries##
library(dplyr)
library(ggplot2)
library(kernlab)
library(caret)
library(randomForest)
library(neuralnet)
library(NbClust)
library(factoextra)
library(cluster)
library(dbscan)
library(FNN)
library(caTools)
library(corrplot)
library(glmnet)
library(gmodels)
library(e1071)

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

# Import dataset
dataset <- read.csv("datasets/cleaned_dataset.csv")

# Subset the dataset
subset <- dataset[, c("t_cap", "t_hh", "t_rd", "t_rsa", "t_ttlh")]
subset <- na.omit(subset)

# Normalize input variables to [0-1] range
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
subset_normalized <- subset %>% mutate(across(where(is.numeric), normalize))

subset_normalized <- subset_normalized %>% 
  sample_frac(size = 0.2, replace = FALSE) 

# Define proportion for training data
train_proportion <- 0.7

# Generate random indices to shuffle the dataset
set.seed(123)  # for reproducibility
random_indices <- sample(nrow(subset_normalized))

# Shuffle the dataset
shuffled_data <- subset_normalized[random_indices, ]

# Split the shuffled dataset into training and testing sets
train_data <- shuffled_data[1:round(train_proportion * nrow(shuffled_data)), ]
test_data <- shuffled_data[(round(train_proportion * nrow(shuffled_data)) + 1):nrow(shuffled_data), ]

# Define the number of hidden layers to iterate over
hidden_layers <- c(1, 5, 10)

# Open a text file to save the results
sink("generated_results/neuralnet_results.txt")

# Iterate over different numbers of hidden layers
for (hl in hidden_layers) {
  cat("Number of Hidden Layers:", hl, "\n")
  
  # Define neural network model
  nn_model <- neuralnet(
    formula = t_cap ~ t_hh + t_rd + t_rsa + t_ttlh,
    data = train_data,
    hidden = hl, 
    stepmax = 1e6
  )
  
  # Check if weights were calculated
  if (!is.null(nn_model$weights)) {
    # Visualize Network Topology and save plot as file
    png(filename = paste0("generated_graphs/neuralnet_plot_hidden_", hl, ".png"), width = 800, height = 600)
    plot(nn_model)
    dev.off()
    
    # Make predictions on testing data
    predictions <- predict(nn_model, as.matrix(test_data[, c("t_hh", "t_rd", "t_rsa", "t_ttlh")]))
    
    # Compute correlation between predicted and actual capacity
    correlation <- cor(predictions, test_data$t_cap)
    
    # Calculate Mean Squared Error (MSE)
    mse <- mean((predictions - test_data$t_cap)^2)
    
    # Calculate Root Mean Squared Error (RMSE)
    rmse <- sqrt(mse)
    
    # Calculate Mean Absolute Error (MAE)
    mae <- mean(abs(predictions - test_data$t_cap))
    
    # Print the metrics
    cat("Mean Squared Error (MSE):", mse, "\n")
    cat("Root Mean Squared Error (RMSE):", rmse, "\n")
    cat("Mean Absolute Error (MAE):", mae, "\n")
    cat("Correlation:", correlation, "\n\n")
  } else {
    cat("Weights were not calculated for", hl, "hidden layers.\n\n")
  }
}
# Close the text file
sink()





#####################################################
## Section 3: Clustering
#####################################################
##Setup data (read, clean, subset key variables)
cleaned_dataset <- dataset[complete.cases(dataset[, c("t_cap", "t_hh", "t_rd", "t_rsa", "t_ttlh")]), ]
pdata <- cleaned_dataset[,c("t_cap", "t_hh", "t_rd", "t_rsa", "t_ttlh")]

#K-means Clustering
#Set random seed
set.seed(123)
#Create a subset dataframe of 1000 rows
subset <- pdata[sample(nrow(pdata), 1000), ]
#Standardize subset
standard <- scale(subset)
#Determine optimal number of clusters using Elbow method
fviz_nbclust(standard, kmeans, method = "wss")
#Run k means based on optimal k value and plot
k2 <- kmeans(standard, centers = 3, nstart = 100)
fviz_cluster(k2, data=standard)
#Analyze clusters by creating a table of each clusters summary stats
cluster1_data <- standard[k2$cluster == 1, ]
cluster2_data <- standard[k2$cluster == 2, ]
cluster3_data <- standard[k2$cluster == 3, ]
cluster_summary <- lapply(list(cluster1_data, cluster2_data, cluster3_data), function(cluster_data) {summary(cluster_data)})

#Hierarchical Clustering
#Calculate the euclidean distance between each row (dissimilarity matrix)
d <- dist(standard, method = "euclidean")
#Cluster using distances (complete linkage) and plot
hc1 <- hclust(d, method = "complete")
plot(hc1, cex = 0.6, hang = -1)
#Analyze clusters
h.clusters <- cutree(hc1, k = 5)
data_with_clusters <- cbind(standard, Cluster = h.clusters)
cluster_summary <- aggregate(cbind(t_cap, t_hh, t_rd, t_rsa, t_ttlh) ~ Cluster, data = data_with_clusters, FUN = summary)
#Count no. of turbines in each cluster
cluster_counts <- table(data_with_clusters[, "Cluster"])

##Density-Based Clustering
#Determine optimal epsilon value by generating a k distance plot
dbscan::kNNdistplot(standard,k=8)
#Evaluate plot to determine optimal epsilon value; run dbscan function to cluster original dataset based on optimal epsilon value 
db <- dbscan(standard, eps = 0.50, minPts = 8)
#Plot clusters
fviz_cluster(db, data = standard)

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

#####################################################
## Section 5: Feature Selection
#####################################################

# 1. Linear Regression - Filtering

## Original model for t_hh from Milestone 2
#Conduct linear regression considering t-hh as independent variable and t-cap as dependent variable.
#Set a seed
set.seed(1)

# Randomly divide the data into training and testing sets
# Calculate indices for the training set, capturing 75% of the data
split <- sample.split(pdata, SplitRatio <- 0.75)
training_set <- subset(pdata, split ==TRUE)
test_set <- subset(pdata, split ==FALSE)

# Create the training dataset using the selected indices
tmp_train <- pdata[training_set,]

# Create the testing dataset using the remaining indices
tmp_test <- pdata[-training_set,]

# Conduct regression analysis
model1.lm <- lm(t_cap~ t_hh + t_rd + t_rsa + t_ttlh , data =tmp_train )
summary(model1.lm)

#let's predict the t_cap by using the test data for the model, set as data frame
m1_predict <- predict (model1.lm, newdata=tmp_test)
m1_predict <- as.data.frame(m1_predict)

#Configure the predict output to match the testing set for comparison
colnames(m1_predict) <- "t_cap"
m1_predict_subset <- m1_predict[1:nrow(tmp_test), ]

# calculate the correlation between the predicted and real values 
#Check for the correlation with actual result for t_hh
cor_thh <- cor(m1_predict_subset, tmp_test$t_cap)
cor_thh

## Applying filtering for t_hh linear regression model
#Plot correlation matrix
corrV <- cor(training_set)
corrplot(corrV, method="number", is.corr=FALSE)

#Remove t_hh as weakest variable
model2 <- update(model1.lm, ~.-thh) 
summary(model2)

# 2. Logistic Regression - Lasso Regularization

#Convert the dependent variable into a categorical → above >1700 is a high performing 1, <1700 is not 0
pdata$t_cap_bin <- ifelse(pdata$t_cap > 1700, 1,0)
#Standardize dataset
standard <- scale(pdata)
#Create training and testing sets
split1 <- sample.split(pdata$t_cap_bin, SplitRatio <- 0.75)
training_set1 <- subset(pdata, split1 ==TRUE)
test_set1 <- subset(pdata, split1 ==FALSE)
#Build regression model and print summary stats
log_reg <- glm(t_cap_bin ~ ., family = binomial, data = training_set)
summary(log_reg)

#Build regression model with lasso regularization and print summary stats
x <- as.matrix(training_set[, !names(training_set) %in% "t_cap_bin"])
lasso_model <- glmnet(x, training_set$t_cap_bin, family = "binomial", alpha = 1)
summary(lasso_model)

# 3. Naive Bayes - Wrapper Method
#Randomize and create training (75%) and testing (25%) sets
set.seed(123)
split <- sample.split(pdata$t_cap_bin, SplitRatio <- 0.75)
training_set <- subset(pdata, split ==TRUE)
test_set <- subset(pdata, split ==FALSE)
#Standardize training dataset
stand <- scale(training_set[, -which(names(training_set) == "t_cap_bin")])
#Run wrapper on original Naive Bayes model from Milestone 2 using the train() function
nb_model <- train(t_cap_bin ~ ., 
                  +                   data = training_set, 
                  +                   method = "nb", 
                  +                   trControl = trainControl(method = "cv", number = 10))
#Predict on new NB Model using testing set
predictions <- predict(nb_model, newdata = test_set)
#Print confusion matrix to evaluate accuracy
confusionMatrix(predictions, test_set$t_cap_bin)
#########################################################

