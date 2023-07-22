# Load required packages
library(tidyverse)
library(rpart)
library(rpart.plot)

# Define the dataset
dataset <- tibble(
  Day = 1:14,
  Outlook = c("Sunny", "Sunny", "Overcast", "Rain", "Rain", "Rain", "Overcast", "Sunny", "Sunny", "Rain", "Sunny", "Overcast", "Overcast", "Rain"),
  Temp = c("Hot", "Hot", "Hot", "Mild", "Cool", "Cool", "Cool", "Mild", "Cool", "Mild", "Mild", "Mild", "Hot", "Mild"),
  Humidity = c("High", "High", "High", "High", "Normal", "Normal", "Normal", "High", "Normal", "Normal", "Normal", "High", "Normal", "High"),
  Wind = c("Weak", "Strong", "Weak", "Weak", "Weak", "Strong", "Strong", "Weak", "Weak", "Weak", "Strong", "Strong", "Weak", "Strong"),
  Play = c("No", "No", "Yes", "Yes", "Yes", "No", "Yes", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "No")
)

# Define a function to calculate the entropy of a dataset
entropy <- function(data, target) {
  # Count the number of occurrences of each value of the target variable
  counts <- data %>%
    count({{target}}) %>%
    pull(n)
  
  # Calculate the proportion of each value
  proportions <- counts / sum(counts)
  
  # Calculate the entropy
  -sum(proportions * log2(proportions))
}

# Define a function to calculate the information gain of a split on a feature
information_gain <- function(data, feature, target) {
  # Calculate the entropy of the original dataset
  entropy_original <- entropy(data, {{target}})
  
  # Calculate the entropy of each subset of the data split by the feature
  subsets <- data %>%
    group_by({{feature}}) %>%
    summarise(count = n(), entropy = entropy(., {{target}})) %>%
    ungroup()
  
  # Calculate the weighted average entropy of the subsets
  entropy_subsets <- subsets %>%
    summarise(weighted_entropy = sum(count / nrow(data) * entropy))
  
  # Calculate the information gain
  entropy_original - entropy_subsets$weighted_entropy
}

# Define a function to recursively build the decision tree
build_tree <- function(data, target, features) {
  # Base case: if there are no more features to split on, return the most common value of the target variable
  if (length(features) == 0) {
    return(names(sort(table(data[[target]]), decreasing = TRUE))[1])
  }
  
  # Calculate the information gain of each feature
  information_gains <- features %>%
    map_dbl(~ information_gain(data, {{.}}, {{target}}))
  
  # Select the feature with the highest information gain
  best_feature <- features[[which.max(information_gains)]]
  
  # Create a new node for the decision tree
  node <- list()
  node$name <- as.character(best_feature)
  node$type <- "internal"
  node$children <- list()
  
  # For each possible value of the best feature, build a subtree recursively
   for (value in unique(data[[best_feature]])) {
  # Subset the data by the current value of the best feature
  subset_data <- data %>%
  filter({{best_feature}} == value)