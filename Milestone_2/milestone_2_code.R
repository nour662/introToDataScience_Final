library(ggplot2)
library(gridExtra)
library(rpart)
library(png)
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

