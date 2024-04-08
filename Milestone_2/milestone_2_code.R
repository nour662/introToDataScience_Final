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

#Import dataset and create a subset that removes NA cases
dataset <- read.csv("cleaned_datset.csv")
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

