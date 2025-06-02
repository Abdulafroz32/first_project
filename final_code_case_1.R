# Load necessary libraries
library(ggplot2)
library(dplyr)
library(mice)
library(VIM) 
library(geosphere)
library(lubridate)
library(caret)
library(randomForest)
library(car)
library(pROC)        # For AUC-ROC analysis
library(e1071)       # For Naïve Bayes
library(ROSE)
#Load the dataset 
car_rental.df <- read.csv("C:/Users/abdul/OneDrive/Desktop/Analytic_Practicum/case-1/SAR_Rental.csv")
#check the structure ofthe dataset
str(car_rental.df)
#first 6-rows of data set
head(car_rental.df)
#summary of dataset
summary(car_rental.df)
# Bar plot for Car_Cancellation
ggplot(car_rental.df, aes(x = factor(Car_Cancellation))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Ride Cancellations", x = "Cancellation (0 = No, 1 = Yes)", y = "Count") +
  theme_minimal()
# Stacked bar plot for booking method vs cancellation
ggplot(car_rental.df, aes(x = factor(online_booking), fill = factor(Car_Cancellation))) +
  geom_bar(position = "fill") +
  labs(title = "Online Booking vs Cancellation", x = "Online Booking (0 = No, 1 = Yes)", y = "Proportion") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"), name = "Cancellation")
# Stacked bar plot for booking method vs cancellation
ggplot(car_rental.df, aes(x = factor(mobile_site_booking), fill = factor(Car_Cancellation))) +
  geom_bar(position = "fill") +
  labs(title = "mobile_site_booking vs Cancellation", x = "Mobile site Booking (0 = No, 1 = Yes)", y = "Proportion") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"), name = "Cancellation")
# Bar plot for Travel Type vs Cancellation
ggplot(car_rental.df, aes(x = factor(travel_type_id), fill = factor(Car_Cancellation))) +
  geom_bar(position = "fill") +
  labs(title = "Travel Type vs Cancellation", x = "Travel Type", y = "Proportion of Cancellations") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"), name = "Cancellation")
# Count the number of bookings per user
booking_counts <- car_rental.df %>%
  count(user_id, name = "bookings") %>%
  arrange(desc(bookings))
# Select the top 10 users with most bookings
top_users <- head(booking_counts, 10)
# Create a bar plot for visualization
ggplot(top_users, aes(x = factor(user_id), y = bookings)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 10 Users with Most Bookings",
       x = "User ID",
       y = "Number of Bookings") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Count cancellations per user and arrange in descending order
cancellations <- car_rental.df %>%
  filter(Car_Cancellation == 1) %>%
  count(user_id, name = "cancellations") %>%
  arrange(desc(cancellations))
#select top 10 users with ride cancellations.
top_cancel <- head(cancellations, 10)
ggplot(top_cancel, aes(x = factor(user_id), y = cancellations)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Top 10 Users with Most Cancellations",
       x = "User ID",
       y = "Number of Cancellations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Covert empty strings "" into NA since to_date has empty strings
car_rental.df$to_date[car_rental.df$to_date == ""] <- NA
#total no  of missing valuesa and which column has how many missing values
sum(is.na(car_rental.df))
colSums(is.na(car_rental.df))
#using md.pattern to check missing values relations
md.pattern(car_rental.df, rotate.names=TRUE)
# Visualize missing data
aggr(car_rental.df, col=c("navy", "yellow"), numbers=TRUE, sortVars=TRUE, labels=names(car_rental.df), cex.axis=.7, gap=3)
# Filter rows where from_area_id is missing and select travel_type_id
missing_travel_type_ids <- car_rental.df %>%
  filter(is.na(from_area_id)) %>%
  select(travel_type_id)
# View the unique travel_type_id values for missing from_area_id
unique(missing_travel_type_ids)
# Filter rows where to_area_id is missing and select travel_type_id
missing_travel_type_ids <- car_rental.df %>%
  filter(is.na(to_area_id)) %>%
  select(travel_type_id)
# View the unique travel_type_id values for missing from_area_id
unique(missing_travel_type_ids)
sum(missing_travel_type_ids == 3)
sum(missing_travel_type_ids == 1)
#dropping rows with from_area_id has NAs
car_rental.df <- car_rental.df %>%
  filter(!is.na(from_area_id))
# Remove rows where travel_type_id is 1
car_rental.df <- car_rental.df %>%
  filter(!(travel_type_id == 1 & is.na(to_area_id)))
#fill mising values in to_area_id with "0".
car_rental.df <- car_rental.df %>%
  mutate(to_area_id = ifelse(is.na(to_area_id), 0, to_area_id))
summary(car_rental.df$to_lat)  # Check min, max, mean, median
summary(car_rental.df$to_long)
#disturbution of to lat and to long using histogram.
hist(car_rental.df$to_lat, breaks = 30, main = "Distribution of to_lat", col = "blue")
hist(car_rental.df$to_long, breaks = 30, main = "Distribution of to_long", col = "blue")
# Compute global median for rows where to_area_id = 0
global_median_lat <- median(car_rental.df$to_lat, na.rm = TRUE)
global_median_long <- median(car_rental.df$to_long, na.rm = TRUE)
# fill missing values with median
car_rental.df <- car_rental.df %>%
  mutate(
         # For to_area_id = 0, use global median
         to_lat = ifelse(is.na(to_lat), global_median_lat, to_lat),
         to_long = ifelse(is.na(to_long), global_median_long, to_long))
# Calculate distances using latitude and longitude
start_points <- cbind(car_rental.df$from_long, car_rental.df$from_lat)
end_points <- cbind(car_rental.df$to_long, car_rental.df$to_lat)
# Calculate distances (in meters), convert to kilometers
car_rental.df$Distance <- distHaversine(start_points, end_points) / 1000
min(car_rental.df$Distance)
max(car_rental.df$Distance)
# Get all package_id values where travel_type_id == 2
package_ids_for_travel_type_2 <- car_rental.df %>%
  filter(travel_type_id == 2) %>%
  select(package_id)
# View unique package_id values
unique(package_ids_for_travel_type_2)
# Drop specified columns
car_rental.df <- car_rental.df %>%
  select(-row.,-package_id, -from_city_id, -to_city_id, -from_long,-from_lat,
         -to_lat,-to_long)
# Verify the columns are removed
colnames(car_rental.df)
colSums(is.na(car_rental.df))
car_rental.df$from_date <- as.POSIXct(car_rental.df$from_date,format = "%m/%d/%Y %H:%M")
car_rental.df$to_date <- as.POSIXct(car_rental.df$to_date,format = "%m/%d/%Y %H:%M")
str(car_rental.df)
#dropping all the rows with incorrect timestamp in to_date
car_rental.df <- car_rental.df[car_rental.df$to_date >= car_rental.df$from_date | is.na(car_rental.df$to_date), ]
# Step 1: Compute median trip duration by travel_type_id and Distance
median_durations <- car_rental.df %>%
  filter(!is.na(to_date)) %>%
  mutate(duration = as.numeric(difftime(to_date, from_date, units = "mins"))) %>%
  group_by(travel_type_id, Distance) %>%
  summarize(median_duration = median(duration, na.rm = TRUE), .groups = 'drop')
# Step 2: Compute median trip duration by travel_type_id only (ignoring Distance)
median_durations_travel_type <- car_rental.df %>%
  filter(!is.na(to_date)) %>%
  mutate(duration = as.numeric(difftime(to_date, from_date, units = "mins"))) %>%
  group_by(travel_type_id) %>%
  summarize(median_duration_tt = median(duration, na.rm = TRUE), .groups = 'drop')
# Step 3: Compute global median trip duration
global_median_duration <- car_rental.df %>%
  filter(!is.na(to_date)) %>%
  mutate(duration = as.numeric(difftime(to_date, from_date, units = "mins"))) %>%
  summarize(global_median = median(duration, na.rm = TRUE)) %>%
  pull(global_median)
# Step 4: Merge all median durations
car_rental.df <- car_rental.df %>%
  left_join(median_durations, by = c("travel_type_id", "Distance")) %>%
  left_join(median_durations_travel_type, by = "travel_type_id") %>%
  mutate(
    # 1st priority: Use specific (travel_type_id, Distance) median
    median_duration = ifelse(is.na(median_duration), median_duration_tt, median_duration),
    
    # 2nd priority: Use travel_type_id median if specific median is missing
    median_duration = ifelse(is.na(median_duration), global_median_duration, median_duration),
    
    # Impute missing to_date values
    to_date = ifelse(is.na(to_date), from_date + dminutes(median_duration), to_date)
  ) %>%
  select(-median_duration, -median_duration_tt)  # Remove temporary columns
# Final check
car_rental.df$to_date <- as.POSIXct(car_rental.df$to_date,format = "%m/%d/%Y %H:%M")
sum(is.na(car_rental.df$to_date))  # Should be 0
colSums(is.na(car_rental.df))
#convert into categorical values
car_rental.df$user_id <- as.factor(car_rental.df$user_id)
car_rental.df$vehicle_model_id <- as.factor(car_rental.df$vehicle_model_id)
car_rental.df$travel_type_id <- as.factor(car_rental.df$travel_type_id)
car_rental.df$from_area_id <- as.factor(car_rental.df$from_area_id)
car_rental.df$to_area_id <- as.factor(car_rental.df$to_area_id)
car_rental.df$online_booking <- as.factor(car_rental.df$online_booking)
car_rental.df$mobile_site_booking <- as.factor(car_rental.df$mobile_site_booking)
car_rental.df$Car_Cancellation <- as.factor(car_rental.df$Car_Cancellation)
car_rental.df$to_date <- as.POSIXct(car_rental.df$to_date, format = "%m/%d/%Y %H:%M")
car_rental.df$booking_created <- as.POSIXct(car_rental.df$booking_created, format = "%m/%d/%Y %H:%M")
str(car_rental.df)
# Boxplot of Distance vs. Car Cancellation
ggplot(car_rental.df, aes(x = Car_Cancellation, y = Distance, fill = Car_Cancellation)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Distribution of Distance by Cancellation Status") +
  xlab("Car Cancellation") + ylab("Distance (km)")
#Feature engineering day , month and year extraction
car_rental.df$from_year <- as.numeric(format(car_rental.df$from_date, "%Y"))
car_rental.df$from_month <- as.numeric(format(car_rental.df$from_date, "%m"))
car_rental.df$from_day <- as.numeric(format(car_rental.df$from_date, "%d"))
car_rental.df$from_date_hour <- as.numeric(format(car_rental.df$from_date, "%H"))
car_rental.df$from_date_minute <- as.numeric(format(car_rental.df$from_date, "%M"))
#Convert date-time in booking_created
car_rental.df$booking_year <- as.numeric(format(car_rental.df$booking_created, "%Y"))
car_rental.df$booking_month <- as.numeric(format(car_rental.df$booking_created, "%m"))
car_rental.df$booking_day <- as.numeric(format(car_rental.df$booking_created, "%d"))
car_rental.df$booking_created_hour <- as.numeric(format(car_rental.df$booking_created, "%H"))
car_rental.df$booking_created_minute <- as.numeric(format(car_rental.df$booking_created, "%M"))
car_rental.df$to_year <- as.numeric(format(car_rental.df$to_date, "%Y"))
car_rental.df$to_month <- as.numeric(format(car_rental.df$to_date, "%m"))
car_rental.df$to_day <- as.numeric(format(car_rental.df$to_date, "%d"))
car_rental.df$to_date_hour <- as.numeric(format(car_rental.df$to_date, "%H"))
car_rental.df$to_date_minute <- as.numeric(format(car_rental.df$to_date, "%M"))
str(car_rental.df)
car_rental.df <- car_rental.df[, !(colnames(car_rental.df) %in% c("from_year", "to_year","booking_year"))]
car_rental.df$from_month <- factor(car_rental.df$from_month, 
                                       levels = 1:12, 
                                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
car_rental.df$booking_month <- factor(car_rental.df$booking_month, 
                                       levels = 1:12, 
                                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
car_rental.df$to_month <- factor(car_rental.df$to_month, 
                                      levels = 1:12, 
                                      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
car_rental.df$trip_start_time <- cut(
  car_rental.df$from_date_hour,
  breaks = c(-1, 5, 11, 17, 23),  # Define breakpoints
  labels = c("Night", "Morning", "Afternoon", "Evening")
)
car_rental.df$trip_end_time <- cut(
  car_rental.df$to_date_hour,
  breaks = c(-1, 5, 11, 17, 23),  # Define breakpoints
  labels = c("Night", "Morning", "Afternoon", "Evening")
)
car_rental.df$trip_book_time <- cut(
  car_rental.df$booking_created_hour,
  breaks = c(-1, 5, 11, 17, 23),  # Define breakpoints
  labels = c("Night", "Morning", "Afternoon", "Evening")
)
car_rental.df$is_weekend_start <- as.factor(ifelse(car_rental.df$from_day %in% c(6, 7), 1, 0))
car_rental.df$is_weekend_booking <- as.factor(ifelse(car_rental.df$booking_day %in% c(6, 7), 1, 0))
car_rental.df$is_weekend_end <- as.factor(ifelse(car_rental.df$to_day %in% c(6, 7), 1, 0))
str(car_rental.df)
car_rental.df <- car_rental.df[, !(colnames(car_rental.df) %in% c("from_date_hour","to_date_hour","booking_created_hour",
                                                                  "from_day","booking_day","to_day"))]
car_rental.df <- car_rental.df[, !(colnames(car_rental.df) %in% c("from_date_minute",
                  "to_date_minute","booking_created_minute","to_date"
                  ,"from_date","booking_created"))]
str(car_rental.df)
#summary statistic of distance varaible
summary(car_rental.df$Distance)
#Disatnce disturbution using histogram
ggplot(car_rental.df, aes(x = Distance)) + geom_histogram(binwidth = 10) + theme_minimal() + labs(title = "Distance Distribution")
#Cor-relation b/w distance and car cancellation.
cor(car_rental.df$Distance, as.numeric(car_rental.df$Car_Cancellation))
ggplot(car_rental.df, aes(x = Car_Cancellation, y = Distance)) +
  geom_boxplot() + theme_minimal() + labs(title = "Distance by Car Cancellation")
#Vehicle used most frequently
summary(car_rental.df$vehicle_model_id)
#Time of the ride start
summary(car_rental.df$trip_start_time)
#trip start time distribution using histogram
ggplot(car_rental.df, aes(x = trip_start_time)) + geom_bar() + theme_minimal() + labs(title = "Trip Start Time Distribution")
#vehicle distribution
ggplot(car_rental.df, aes(x = vehicle_model_id, fill = Car_Cancellation)) +
  geom_bar(position = "fill") + theme_minimal() + labs(title = "Car Cancellation by Vehicle Model")
#dropping unnecasssry columns
car_rental.df <- car_rental.df[, !(colnames(car_rental.df) %in% c("user_id","from_area_id","to_area_id"))]
car_rental.df <- car_rental.df[, !(colnames(car_rental.df) %in% c("vehicle_model_id"))]
str(car_rental.df)
set.seed(123)
# Set up cross-validation control
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
# Run RFE
rfeResults <- rfe(x = car_rental.df[, -which(names(car_rental.df) == "Car_Cancellation")],  # predictors
                  y = car_rental.df$Car_Cancellation,  # target
                  sizes = c(1:10),  # Try 1 to 10 features
                  rfeControl = ctrl)
# Output the results
print(rfeResults)
# Get the optimal number of features and the corresponding predictors
rfeResults$optVariables
# Plot the results to visualize how model performance changes with the number of features
plot(rfeResults, type = c("g", "o"))
#drop to_month since it causes alias
car_rental.df <- car_rental.df[, !(colnames(car_rental.df) %in% c("to_month"))]
#logistic regression to check significant predictors
logit_model <- glm(Car_Cancellation ~ ., data = car_rental.df,family = "binomial")
summary(logit_model)
vif_result <- vif(logit_model)
alias(logit_model)
# Assign high and low season labels
car_rental.df$season <- ifelse(car_rental.df$from_month %in% c("May", "June", "July", "August", "September"), "High", "Low")
# Convert to a factor variable
car_rental.df$season <- factor(car_rental.df$season, levels = c("Low", "High"))
# Remove the redundant predictor `is_weekend_end`
car_rental.df <- car_rental.df[, !(colnames(car_rental.df) %in% c("is_weekend_end"))]
# Drop original month columns to avoid multicollinearity
car_rental.df <- car_rental.df %>% select(-from_month, -booking_month)
set.seed(123)
#data partitioning
# Step 1: Training set (60%)
trainIndex <- sample(1:nrow(car_rental.df), size = 0.60 * nrow(car_rental.df))
train_data <- car_rental.df[trainIndex, ]
remaining_data <- car_rental.df[-trainIndex, ]

# Step 2: Split remaining 40% into validation (25%) and test (15%)
validIndex <- sample(1:nrow(remaining_data), size = 0.625 * nrow(remaining_data))
validation_data <- remaining_data[validIndex, ]
test_data <- remaining_data[-validIndex, ]

#build logistic regression model
log_model <- glm(Car_Cancellation ~ . , data = train_data, family = binomial)
summary(log_model)
# Validation set predictions and evaluation
valid_probs_log <- predict(log_model, newdata = validation_data, type = "response")
head(valid_probs_log)
valid_pred_log <- ifelse(valid_probs_log > 0.3, 1, 0)
head(valid_pred_log)
valid_pred_log <- factor(valid_pred_log, levels = c(0, 1))
# Confusion Matrix for Logistic Regression
conf_matrix_log <- confusionMatrix(valid_pred_log, validation_data$Car_Cancellation, positive = "1")
print(conf_matrix_log)
set.seed(123)
# Train Random Forest model
rf_model <- randomForest(Car_Cancellation ~ ., data = train_data, ntree = 100, mtry = 3, importance = TRUE)
# Print model summary
print(rf_model)
# Feature Importance
importance(rf_model)
varImpPlot(rf_model)
# Predict on validation set
valid_pred_rf <- predict(rf_model, newdata = validation_data, type = "prob")[,2]
# Convert probabilities to binary class (Threshold = 0.5)
valid_pred_class_rf <- ifelse(valid_pred_rf > 0.3, 1, 0)
# Ensure that both predicted and actual factors have the same levels
valid_pred_class_rf <- factor(valid_pred_class_rf, levels = c(0, 1))  # Ensure order is 0, 1
validation_data$Car_Cancellation <- factor(validation_data$Car_Cancellation, levels = c(0, 1))  # Ensure order is 0, 1
# Generate confusion matrix
conf_matrix_rf <- confusionMatrix(valid_pred_class_rf, validation_data$Car_Cancellation, positive = "1")
print(conf_matrix_rf)
set.seed(123)
# Train Naïve Bayes model
nb_model <- naiveBayes(Car_Cancellation ~ ., data = train_data, laplace = 1)
# Print model summary
print(nb_model)
# Predict on validation set
valid_pred_nb <- predict(nb_model, newdata = validation_data, type = "raw")[,2]
# Convert probabilities to binary class (Threshold = 0.5)
valid_pred_class_nb <- ifelse(valid_pred_nb > 0.3, 1, 0)
# Generate confusion matrix
conf_matrix_nb <- confusionMatrix(factor(valid_pred_class_nb), factor(validation_data$Car_Cancellation), positive = "1")
print(conf_matrix_nb)
table(car_rental.df$Car_Cancellation)
# Visualize imbalance
ggplot(train_data, aes(x = factor(Car_Cancellation))) + 
  geom_bar(fill = "blue") +
  theme_minimal() +
  labs(title = "Class Distribution", x = "Car Cancellation", y = "Count")
#install.packages("ROSE")  # Install ROSE package
set.seed(123)
# Apply ROSE to generate synthetic samples
train_data_balanced <- ROSE(Car_Cancellation ~ ., data = train_data, seed = 123)$data
# Check new class distribution
table(train_data_balanced$Car_Cancellation)
set.seed(123)
#build logistic regression model
log_model_1 <- glm(Car_Cancellation ~ . , data = train_data_balanced, family = binomial)
summary(log_model_1)
# Validation set predictions and evaluation
valid_probs_log <- predict(log_model_1, newdata = validation_data, type = "response")
valid_pred_log <- ifelse(valid_probs_log > 0.5, 1, 0)
valid_pred_log <- factor(valid_pred_log, levels = c(0, 1))
# Confusion Matrix for Logistic Regression
conf_matrix_log <- confusionMatrix(valid_pred_log, validation_data$Car_Cancellation, positive = "1")
print(conf_matrix_log)
# Compute ROC curve
roc_curve <- roc(validation_data$Car_Cancellation, valid_probs_log)
# Plot ROC curve
plot(roc_curve, col = "blue", main = "ROC Curve for Logistic Regression")
# Get AUC value
auc_value <- auc(roc_curve)
auc_value
# Train Random Forest model
rf_model_1 <- randomForest(Car_Cancellation ~ ., data = train_data_balanced, ntree = 100, mtry = 3, importance = TRUE)
# Print model summary
print(rf_model_1)
# Feature Importance
importance(rf_model_1)
varImpPlot(rf_model_1)
# Predict on validation set
valid_pred_rf <- predict(rf_model_1, newdata = validation_data, type = "prob")[,2]
# Convert probabilities to binary class (Threshold = 0.5)
valid_pred_class_rf <- ifelse(valid_pred_rf > 0.5, 1, 0)
# Generate confusion matrix
conf_matrix_rf <- confusionMatrix(factor(valid_pred_class_rf), factor(validation_data$Car_Cancellation), positive = "1")
print(conf_matrix_rf)
# ---- Random Forest ROC Curve ----
roc_rf <- roc(validation_data$Car_Cancellation, valid_pred_rf)  # Create ROC curve
auc_rf <- auc(roc_rf)  # Calculate AUC
plot(roc_rf, col = "blue", main = "ROC Curve - Random Forest", lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc_rf, 4)), col = "blue", lwd = 2)
# Train Naïve Bayes model
nb_model_1 <- naiveBayes(Car_Cancellation ~ ., data = train_data_balanced, laplace = 1)
# Print model summary
print(nb_model_1)
# Predict on validation set
valid_pred_nb <- predict(nb_model_1, newdata = validation_data, type = "raw")[,2]
# Convert probabilities to binary class (Threshold = 0.5)
valid_pred_class_nb <- ifelse(valid_pred_nb > 0.5, 1, 0)
# Generate confusion matrix
conf_matrix_nb <- confusionMatrix(factor(valid_pred_class_nb), factor(validation_data$Car_Cancellation), positive = "1")
print(conf_matrix_nb)
# ---- Naïve Bayes ROC Curve ----
roc_nb <- roc(validation_data$Car_Cancellation, valid_pred_nb)  # Create ROC curve
auc_nb <- auc(roc_nb)  # Calculate AUC
plot(roc_nb, col = "red", main = "ROC Curve - Naïve Bayes", lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc_nb, 4)), col = "red", lwd = 2)
#use random forest model to Predict on test set
test_pred_rf <- predict(rf_model_1, newdata = test_data, type = "prob")[,2]
# Convert probabilities to binary class (Threshold = 0.5)
test_pred_class_rf <- ifelse(test_pred_rf > 0.5, 1, 0)
# Generate confusion matrix
conf_matrix_rf <- confusionMatrix(factor(test_pred_class_rf), factor(test_data$Car_Cancellation), positive = "1")
print(conf_matrix_rf)
#generate ROC curve on test data
roc_rf <- roc(test_data$Car_Cancellation, test_pred_rf)  # Create ROC curve
auc_rf <- auc(roc_rf)  # Calculate AUC
plot(roc_rf, col = "blue", main = "ROC Curve - Random Forest", lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc_rf, 4)), col = "blue", lwd = 2)
