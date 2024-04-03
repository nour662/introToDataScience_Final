##Load Libraries##
library(ggplot2)
library(gridExtra)
library(rpart)
library(png)

library(caTools)
library(car)
library(glmnet)
library(gmodels)
library(e1071)

### Question #2: Log Regression and Naive Bayes ###

#Import dataset and create a subset that removes NA cases
dataset <- read.csv("cleaned_datset.csv")
complete_data <- complete.cases(dataset)
data_subset <- dataset[complete_data,]

#Create a clean data frame with the main variables and standardize
clean_subset <- data_subset[,c("t_cap", "t_hh","t_rd","t_rsa","t_ttlh")]
standard <- scale(clean_subset)

#Regularization Code
#Create training and testing sets
testidx <- which(1:nrow(standard)%%4==0)
data_train <- standard[-testidx,]
data_test <- standard[testidx,]

#Run ridge regression, plot and check coefficients
cv.fit <- cv.glmnet(as.matrix(data_train[,c(-1)]),as.vector(data_train[,1]),alpha=0)
plot(cv.fit)
coef(cv.fit)

##Check for minimum lambda value
cv.fit$lambda.min

#Compare correlation between regularized model and actual testing set
prediction <- predict(cv.fit,newx = as.matrix(data_test[,c(-1)]))
cor(prediction,as.vector(data_test[,1]))

#Log Regression Code
#Convert the dependent variable into a categorical → above >1700 is a high performing 1, <1700 is not 0
clean_subset$t_cap_bin <- ifelse(clean_subset$t_cap > 1700, 1,0)

#Randomize and create training (75%) and testing (25%) sets
set.seed(123)
split <- sample.split(clean_subset$t_cap_bin, SplitRatio <- 0.75)
training_set <- subset(clean_subset, split ==TRUE)
test_set <- subset(clean_subset, split ==FALSE)

# Build log regression model excluding the original column with numerical values for the dependent variable
classifier <- glm(t_cap_bin~.-t_cap, family = binomial,data =training_set)

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

### Question 3: Decision Trees and Random Forests ############

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

