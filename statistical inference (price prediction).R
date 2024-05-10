# Load necessary library
library(readr)

# Set the file path where your dataset is stored
file_path <- "C:/Programming projects/Python/Car Price Prediction Project - Statistics/Sport car price.csv"

# Load the dataset into R
car_data <- read_csv(file_path)

# Convert "Horsepower" column from character to numeric
car_data$Horsepower <- as.numeric(car_data$Horsepower)

# Check the structure of the dataset after conversion
str(car_data)

# Optionally, you can also check the first few rows of the dataset
head(car_data)

#				TASK 02

# Get summary statistics for each variable
summary(car_data)

# Function to calculate mode
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# Apply the function to each column
modes <- sapply(car_data, get_mode)

# Combine summary statistics with mode
summary_with_mode <- cbind(summary(car_data), Mode = modes)

# Print the summary statistics with mode
print(summary_with_mode)

#				TASK 03

install.packages("farver")

# Load necessary library
library(ggplot2)

# Create a boxplot for each variable with different colors
boxplot_data <- stack(car_data)

# Construct the boxplot with ggplot2
ggplot(boxplot_data, aes(x = ind, y = values, fill = ind)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Horsepower" = "red", 
                                "Torque (lb-ft)" = "blue", 
                                "0-60 MPH Time (seconds)" = "green", 
                                "Price (in USD)" = "purple")) +
  labs(title = "Box and Whisker Plot for Car Dataset",
       x = "Variables", y = "Values") +
  theme_minimal()

#				TASK 04

# Create scatter plot
scatter_plot <- ggplot(car_data, aes(x = Horsepower, y = `Price (in USD)`)) +
  geom_point(aes(color = "Horsepower")) +
  geom_point(aes(x = `Torque (lb-ft)`, color = "Torque")) +
  geom_point(aes(x = `0-60 MPH Time (seconds)`, color = "0-60")) +
  labs(title = "Scatter Plot: Independent Variables vs Price",
       x = "Independent Variables", y = "Price (in USD)",
       color = "Variable") +
  scale_color_manual(values = c("Horsepower" = "red", "Torque" = "blue", "0-60" = "green"))

# Print scatter plot
print(scatter_plot)

#				TASK 05

# Run linear regression model
model <- lm(`Price (in USD)` ~ Horsepower + `Torque (lb-ft)` + `0-60 MPH Time (seconds)`, data = car_data)

# Summarize the model
summary(model)

#				TASK 06


# Create scatter plot
scatter_plot <- ggplot(car_data, aes(x = Horsepower, y = `Price (in USD)`)) +
  geom_point(aes(color = "Horsepower")) +  # Scatter plot of Horsepower
  geom_smooth(aes(x = Horsepower), method = "lm", se = FALSE, color = "red") +  # Linear regression line for Horsepower
  geom_point(aes(x = `Torque (lb-ft)`, color = "Torque")) +  # Scatter plot of Torque
  geom_smooth(aes(x = `Torque (lb-ft)`), method = "lm", se = FALSE, color = "blue") +  # Linear regression line for Torque
  geom_point(aes(x = `0-60 MPH Time (seconds)`, color = "0-60")) +  # Scatter plot of 0-60 MPH time
  geom_smooth(aes(x = `0-60 MPH Time (seconds)`), method = "lm", se = FALSE, color = "green") +  # Linear regression line for 0-60 MPH time
  labs(title = "Scatter Plot with Linear Regression Lines",
       x = "Independent Variables", y = "Price (in USD)",
       color = "Variable") +
  scale_color_manual(values = c("Horsepower" = "red", "Torque" = "blue", "0-60" = "green"))

# Print scatter plot
print(scatter_plot)
