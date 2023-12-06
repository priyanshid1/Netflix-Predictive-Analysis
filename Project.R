#Project - Netflix User Database 
#Created by Priyanshi Deliwala on October 30th

#reading the csv file 
library(readr)
#setting the csv file path
setwd("/Users/priyadeliwala/Desktop/ISTM/Semester 3/Foundation of AI/Project")

#creating a data frame with fed_stimulus as a name
netflix_data <- read.csv("NetflixUserbase.csv")
View(netflix_data)
summary(netflix_data)

#Possible Regression 

# Handle missing values (if any)
netflix_data <- na.omit(netflix_data)
View(netflix_data)

# Encode categorical variables
netflix_data$Subscription_Type <- as.factor(netflix_data$Subscription.Type)
netflix_data$Country <- as.factor(netflix_data$Country)
netflix_data$Gender <- as.factor(netflix_data$Gender)
netflix_data$Device <- as.factor(netflix_data$Device)


# Split the data into training and testing sets
set.seed(123) # for reproducibility
library(caTools)
split <- sample.split(netflix_data$Monthly.Revenue, SplitRatio = 0.7)
train_data <- netflix_data[split, ]
test_data <- netflix_data[!split, ]

#SVM MODEL

install.packages("e1071")
library(e1071)

# Build an SVM model
svm_model <- svm(Monthly.Revenue ~ Subscription.Type + Age + Gender + Device, data = train_data, kernel = "radial")

# Print the model
print(svm_model)

# Make predictions on the test data
svm_predictions <- predict(svm_model, test_data)

# Calculate metrics
svm_mae_value <- mae(svm_predictions, test_data$Monthly.Revenue)
svm_mse_value <- mse(svm_predictions, test_data$Monthly.Revenue)

# Print the metrics
cat("SVM - Mean Absolute Error (MAE):", svm_mae_value, "\n")
cat("SVM - Mean Squared Error (MSE):", svm_mse_value, "\n")

#Classification
# Install and load the randomForest package
install.packages("randomForest")
library(randomForest)

# Load necessary libraries
library(caret)
library(randomForest)

# Convert categorical variables to factors
netflix_data$Subscription_Type <- as.factor(netflix_data$Subscription.Type)
netflix_data$Country <- as.factor(netflix_data$Country)
netflix_data$Gender <- as.factor(netflix_data$Gender)
netflix_data$Device <- as.factor(netflix_data$Device)

# Split the data into training and testing sets
set.seed(123)
split <- createDataPartition(netflix_data$Subscription_Type, p = 0.7, list = FALSE)
train_data <- netflix_data[split, ]
test_data <- netflix_data[-split, ]

# Train a random forest model
model <- randomForest(Subscription_Type ~ Country + Device + Gender + Last.Payment.Date + Monthly.Revenue + Plan.Duration, data = train_data)

# Make predictions on the test set
predictions <- predict(model, test_data)

# Evaluate the model
rd_accuracy<-confusionMatrix(predictions, test_data$Subscription_Type)
cat("Random Forest - Accuracy:", rf_accuracy, "\n")

install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

# Count the frequency of each subscription type
subscription_counts <- table(netflix_data$Subscription.Type)

# Convert the counts to a data frame
subscription_df <- as.data.frame(subscription_counts)

# Set names for the data frame columns
colnames(subscription_df) <- c("Subscription.Type", "Count")

# Calculate the percentages
subscription_df$Percentage <- (subscription_df$Count / sum(subscription_df$Count)) * 100

# Create the pie chart with percentage labels
library(ggplot2)

pie_chart <- ggplot(subscription_df, aes(x = "", y = Count, fill = Subscription.Type)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Subscription Type Distribution") +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5)) +
  theme_void()

# Display the pie chart
print(pie_chart)

#DEVICE USAGE

# Count the frequency of each device usage
device_counts <- table(netflix_data$Device)

# Convert the counts to a data frame
device_df <- as.data.frame(device_counts)

# Set names for the data frame columns
colnames(device_df) <- c("Device", "Count")

# Create a bar chart for device usage
device_chart <- ggplot(device_df, aes(x = Device, y = Count, fill = Device)) +
  geom_bar(stat = "identity") +
  labs(title = "Device Usage Distribution") +
  xlab("Device") +
  ylab("Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the bar chart
print(device_chart)


# Box plot to visualize the distribution of age by device, with color-coding by gender
# Create a box plot
ggplot(netflix_data, aes(x = Device, y = Age, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Box Plot of Age by Device with Gender Color",
       x = "Device",
       y = "Age") +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal()

# Box plot to visualize the distribution of age by device, with color-coding by subscription type 
# Create a box plot
ggplot(netflix_data, aes(x = Device, y = Age, fill = Subscription_Type)) +
  geom_boxplot() +
  labs(title = "Box Plot of Age by Device with Subscription Type Color",
       x = "Device",
       y = "Age") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()
