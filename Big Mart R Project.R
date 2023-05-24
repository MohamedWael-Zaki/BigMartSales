# Load the necessary libraries and the dataset
library(ggplot2)   # for data visualization
library(dplyr)     # for data manipulation
library(tidyr)     # for data wrangling
library(readr)     # for reading data
library(skimr)     # for data summary
library(caret)     # for modeling

# Load the dataset
data <- read.csv("C:/Users/ideapad/Desktop/Project Big Data/Train.csv")
View(data)

# Check the structure of the dataset
str(data)

# Summarize the dataset
#summary(data)
skim(data)

# Check for missing values
#sapply(data, function(x) sum(is.na(x)))
# Check for duplicate rows
#data <- unique(data)


# Remove unnecessary variables
data_cleaned <- select(data, -c(Outlet_Establishment_Year, Outlet_Identifier))

#IMPORTANT -->!
#mutate(data_cleaned$Item_Fat_Content <- ifelse(data_cleaned$Item_Fat_Content %in% c("Low Fat", "LF"), "Low Fat", "Regular"))


# Hypothesis Testing
# Subset the data by Item_Type
item_type1 <- subset(data_cleaned, Item_Type == "Dairy")
item_type2 <- subset(data_cleaned, Item_Type == "Meat")

# Perform a t-test
t.test(item_type1$Item_Outlet_Sales, item_type2$Item_Outlet_Sales, alternative = 'greater')

# Confidence Interval: It is the interval that we are 95% confident will contain the true unknown value of population mean.

#Observation: the output indicates that there is no significant difference in the mean Item_Outlet_Sales between the two groups, item_type1 and item_type2, and that any observed differences could be due to chance.


#Converting categorical variables to factors is important because it enables R to recognize the variable as categorical, which can be useful for performing statistical analysis and generating visualizations. For example, when creating plots, R can use the levels of the factor to create meaningful axis labels and legends.

# Convert categorical variables to factors
data_cleaned$Item_Type <- factor(data$Item_Type)
View(data$Item_Type)

data_cleaned$Item_Fat_Content <- factor(data$Item_Fat_Content)
#View(data$Item_Fat_Content)

data_cleaned$Outlet_Type <- factor(data$Outlet_Type)
#View(data$Outlet_Type)

data_cleaned$Outlet_Location_Type <- factor(data$Outlet_Location_Type)
#View(data$Outlet_Location_Type)


## Including Plots

# EDA -> Data Visualization 
############################
# Histogram: To visualize the distribution of a numerical variable.
# Box plot: To visualize the distribution and identify outliers of a numerical variable.
# Bar chart: To visualize the frequency of a categorical variable.
# Scatter plot: To visualize the relationship between two numerical variables.

# Check for outliers
ggplot(data, aes(x="", y=Item_Outlet_Sales)) +
  geom_boxplot(fill="white", color="black") +
  labs(title="Box Plot of Item Outlet Sales")

# Observation: The box plot shows that there are several outliers in the Item_Outlet_Sales variable, with some values exceeding 10,000 in sales. This suggests that some products or stores are performing exceptionally well and may be worth further investigation for potential marketing strategies.

# Removal of outliers 
# Calculate the lower and upper thresholds for outlier removal (e.g., using the IQR method)
Q1 <- quantile(data$Item_Outlet_Sales, 0.25)
Q3 <- quantile(data$Item_Outlet_Sales, 0.75)
IQR <- Q3 - Q1
lower_threshold <- Q1 - 1.5 * IQR
upper_threshold <- Q3 + 1.5 * IQR

# Remove outliers based on the thresholds
data_cleaned <- data[data$Item_Outlet_Sales >= lower_threshold & data$Item_Outlet_Sales <= upper_threshold, ]

# Visualize the distribution of Item_MRP(Maximum Retail Price)
ggplot(data_cleaned, aes(Item_MRP)) +
  geom_histogram(color="black", fill="lightblue") +
  labs(title="Histogram of Item MRP", x="Item MRP", y="Count")

# Observation: The histograms show that Item_MRP is approximately normally distributed, with a peak around 120-140.


# Visualize the relationship between Item MRP vs Item Outlet Sales
ggplot(data_cleaned, aes(Item_MRP, Item_Outlet_Sales)) +
  geom_point(color="darkblue") +
  labs(title="Scatter Plot of Item MRP vs Item Outlet Sales", x="Item MRP", y="Item Outlet Sales")

# Observation: The scatter plot shows a positive correlation between Item_MRP and Item_Outlet_Sales, indicating that higher-priced products tend to sell more. However, there is also a lot of variability in sales at different price points, indicating that other factors are also influencing sales.

# Bar chart of Item_Type
data_cleaned$Item_Fat_Content <- recode(data_cleaned$Item_Fat_Content,
                                        'LF' = 'Low Fat',
                                        'low fat' = 'Low Fat',
                                        'reg' = 'Regular')

table(data_cleaned$Item_Fat_Content)

ggplot(data_cleaned, aes(x=Item_Type, fill=Item_Fat_Content)) +
  geom_bar(position="dodge") +
  labs(title="Bar Chart of Item Type", x="Item Type", y="Count")

# Observation: The bar chart shows that Fruits and Vegetables, Snack Foods, and Household items are the most commonly sold products in Big Mart stores. Additionally, there is an even distribution of low-fat and regular fat items across most product categories. However, the majority of dairy products are low-fat, which could indicate a trend towards healthier food choices among customers.


# Box plots for Item_Outlet_Sales by Outlet_Location_Type
ggplot(data_cleaned, aes(x = Outlet_Location_Type, y = Item_Outlet_Sales)) +
  geom_boxplot() +
  xlab("Outlet Location Type") +
  ylab("Item Outlet Sales") +
  ggtitle("Distribution of Item Outlet Sales by Outlet Location Type")

# Observation: The box plots show that the median Item_Outlet_Sales is higher for Tier 3 cities compared to Tier 1 and Tier 2 cities. The interquartile range (IQR) is also larger for Tier 3 cities, indicating more variability in sales.


# Pie Chart for each Outlet_Type
# Create a data frame with the counts of each outlet type
outlet_counts <- data_cleaned %>%
  group_by(Outlet_Type) %>%
  summarize(count = n())
# Create a pie chart of the outlet type counts
ggplot(outlet_counts, aes(x = "", y = count, fill = Outlet_Type)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart of Outlet Types")

# Observation: The pie chart shows that the majority of outlets in the Big Mart data set are Supermarkets Type1, followed by Grocery Stores and Supermarkets Type2. This suggests that the majority of sales in the data set come from these types of stores and could be useful information for marketing and inventory management decisions.


#Faceted bar chart of Outlet Location Type and Outlet Type by Item Outlet Sales
ggplot(data_cleaned, aes(x=Outlet_Location_Type, y=Item_Outlet_Sales, fill=Outlet_Type)) + 
  geom_bar(stat="summary", fun=mean) + labs(title="Faceted bar chart of Outlet Location Type and Outlet Type by Item Outlet Sales", x="Outlet Location Type", y="Mean Item Outlet Sales") + facet_grid(~Outlet_Type)

#Observation: The faceted bar chart shows the mean Item Outlet Sales for each combination of Outlet Location Type and Outlet Type. Overall, Supermarket Type 3 has the highest mean Item Outlet Sales across all Outlet Location Types, while Grocery Store has the lowest mean Item Outlet Sales across all Outlet Location Types. Within each Outlet Type, the mean Item Outlet Sales varies across Outlet Location Types, with Tier 3 cities generally having higher mean Item Outlet Sales compared to Tier 1 and 2 cities.

#Density plot of Item MRP by Item Type
ggplot(data_cleaned, aes(x=Item_MRP, fill=Item_Type)) + geom_density(alpha=0.6) + 
  labs(title="Density plot of Item MRP by Item Type", x="Item MRP", y="Density")

#Observation: The density plot shows the distribution of Item MRP for each Item Type. Some Item Types, such as Fruits and Vegetables and Snack Foods, have a wider range of Item MRPs compared to other Item Types, such as Health and Hygiene and Household. This suggests that Item MRP may be a more important factor in determining Item Outlet Sales for certain Item Types compared to others.

# Load the dataset
train <- read.csv("C:/Users/ideapad/Desktop/Project Big Data/Train.csv")

# Remove the ID column
train$Item_Identifier <- NULL

# Fill missing values
train$Item_Weight[is.na(train$Item_Weight)] <- median(train$Item_Weight, na.rm = TRUE)
train$Outlet_Size[is.na(train$Outlet_Size)] <- "Unknown"

# Create a new column for the age of the stores
train$Outlet_Age <- 2013 - as.numeric(substr(train$Outlet_Establishment_Year, 1, 4))

# Convert categorical variables to factors
train$Item_Fat_Content <- factor(train$Item_Fat_Content, levels=c("Low Fat","Regular","LF","reg","low fat"))
train$Item_Type <- factor(train$Item_Type)
train$Outlet_Identifier <- factor(train$Outlet_Identifier)
train$Outlet_Location_Type <- factor(train$Outlet_Location_Type)
train$Outlet_Type <- factor(train$Outlet_Type)

# Split the dataset into training and testing sets
set.seed(123)
train_index <- createDataPartition(train$Item_Outlet_Sales, p=0.7, list=FALSE)
train_data <- train[train_index, ]
test_data <- train[-train_index, ]

# Fit a linear regression model to the training set
lm_model <- lm(Item_Outlet_Sales ~ ., data=train_data)

# Generate predictions on the testing set
pred <- predict(lm_model, newdata=test_data)

# Evaluate the performance of the model
rmse <- RMSE(pred, test_data$Item_Outlet_Sales)
r_squared <- R2(pred, test_data$Item_Outlet_Sales)

# Print the performance metrics
cat("Original Model Performance:\n")
cat("RMSE: ", rmse, "\n")
cat("R-squared: ", r_squared, "\n")

# Implement suggestions for model improvement
# Feature Engineering
train_data$Item_MRP_Squared <- train_data$Item_MRP^2
test_data$Item_MRP_Squared <- test_data$Item_MRP^2

# Removal Of Outliers
# Calculate z-scores for Item_Outlet_Sales
z_scores <- scale(train_data$Item_Outlet_Sales)
# Define the threshold for outlier detection (e.g., z-score of 3)
threshold <- 3
# Identify outlier indices
outlier_indices <- which(abs(z_scores) > threshold)
# Remove outliers from train_data
train_data_clean <- train_data[-outlier_indices, ]

# Variable Selection
# Exclude Outlet_Size from the model
lm_model_updated <- lm(Item_Outlet_Sales ~ . - Outlet_Size, data=train_data_clean)  

# Generate predictions on the testing set using the updated model
pred_updated <- predict(lm_model_updated, newdata=test_data)

# Evaluate the performance of the updated model
rmse_updated <- RMSE(pred_updated, test_data$Item_Outlet_Sales)
r_squared_updated <- R2(pred_updated, test_data$Item_Outlet_Sales)

# Print the performance metrics of the updated model
cat("Updated Model Performance:\n")
cat("RMSE: ", rmse_updated, "\n")
cat("R-squared: ", r_squared_updated, "\n")


# Visualize the relationship between Item MRP and Item Outlet Sales using ggplot
ggplot(train, aes(x=Item_MRP, y=Item_Outlet_Sales)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Linear Regression of Item MRP and Item Outlet Sales", x="Item MRP", y="Item Outlet Sales")

#Observation: from the scatter plot of Item MRP and Item Outlet Sales is that there appears to be a positive linear relationship between the two variables. As the Item MRP (maximum retail price) increases, the Item Outlet Sales also tend to increase.

# Select the variables for clustering
cluster_data <- data_cleaned[, c("Item_MRP", "Item_Outlet_Sales")]

# Perform k-means clustering
k <- 3  # Specify the desired number of clusters
kmeans_model <- kmeans(cluster_data, centers = k, nstart = 10)

# Get the cluster assignments
cluster_assignments <- kmeans_model$cluster

# Add the cluster assignments to the original dataset
data_cleaned$Cluster <- factor(cluster_assignments)

# Calculate the within-cluster sum of squares (WCSS)
wcss <- sum(kmeans_model$withinss)

# Print the WCSS 
cat("WCSS:", wcss, "\n")

# Visualize the clusters
library(ggplot2)
ggplot(data_cleaned, aes(x = Item_MRP, y = Item_Outlet_Sales, color = Cluster)) +
  geom_point() +
  labs(title = "K-means Clustering of Item MRP and Item Outlet Sales") +
  theme_minimal()

#Observation: The K-means clustering analysis of Item MRP and Item Outlet Sales resulted in three distinct clusters. The clusters demonstrate clear separation, indicating different patterns or segments within the dataset. This clustering provides valuable insights for identifying customer segments or market segments based on these variables.

#Cluster 1: High-Value Customers
#Cluster 2: Moderate-Value Customers  
#Cluster 3: Low-Value Customers

#These names reflect the idea that Cluster 1 represents customers with higher purchasing power or higher sales values, Cluster 2 represents customers with moderate purchasing power or moderate sales values, and Cluster 3 represents customers with lower purchasing power or lower sales values.
