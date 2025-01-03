# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Set random seed for reproducibility
set.seed(123)

# Define the size of the dataset
num_rows <- 5000  # Moderate-sized dataset
num_categories <- 5  # Number of unique categories in categorical variables

# Generate synthetic data
generate_synthetic_data <- function(num_rows, num_categories) {
  
  # Generate a categorical variable (e.g., customer segment)
  category <- sample(paste0("Segment_", 1:num_categories), 
                     num_rows, replace = TRUE)
  
  # Generate a numerical variable (e.g., annual income in thousands)
  annual_income <- round(rnorm(num_rows, mean = 60, sd = 15), 2)
  
  # Generate another numerical variable (e.g., age)
  age <- sample(18:70, num_rows, replace = TRUE)
  
  # Generate a binary variable (e.g., purchased product: yes/no)
  purchased <- sample(c("Yes", "No"), num_rows, replace = TRUE, prob = c(0.4, 0.6))
  
  # Generate a date variable (e.g., transaction date)
  transaction_date <- sample(seq(as.Date('2023-01-01'), as.Date('2023-12-31'), by="day"), 
                             num_rows, replace = TRUE)
  
  # Combine into a data frame
  data <- data.frame(
    Category = category,
    Annual_Income = annual_income,
    Age = age,
    Purchased = purchased,
    Transaction_Date = transaction_date
  )
  
  return(data)
}

# Generate the dataset
synthetic_data <- generate_synthetic_data(num_rows, num_categories)

# Summary of the generated dataset
summary(synthetic_data)

# Save the dataset to a CSV file
write.csv(synthetic_data, "synthetic_data.csv", row.names = FALSE)
message("Synthetic dataset saved to 'synthetic_data.csv'.")

# Visualize the data: Histogram of Annual Income
ggplot(synthetic_data, aes(x = Annual_Income)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Annual Income", x = "Annual Income (in thousands)", y = "Frequency")

# Visualize the data: Bar chart of Purchased status by Category
ggplot(synthetic_data, aes(x = Category, fill = Purchased)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Purchased Status by Category", x = "Category", y = "Count", fill = "Purchased")
