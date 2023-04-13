library(caret)

# Load data
car1 <- read.csv("car-data.csv")
car1 <- car1 %>% 
  mutate(Mileage = as.numeric(str_replace(Mileage," kmpl| km/kg","")),
         Engine = as.numeric(str_replace(Engine," CC","")),
         Power = as.numeric(str_replace(Power," bhp","")),
         Fuel_Type = as.factor(Fuel_Type),
         Owner_Type = as.factor(Owner_Type),
         Transmission = as.factor(Transmission)) %>% 
  select(-c(X, Name, New_Price, Location))
car1$Year <- as.numeric(car1$Year)
car1$Kilometers_Driven <- as.numeric(car1$Kilometers_Driven)
car1$Fuel_Type <- as.numeric(car1$Fuel_Type)
car1$Owner_Type <- as.numeric(car1$Owner_Type)
car1$Transmission <- as.numeric(car1$Transmission)
car1 <- drop_na(car1)

# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(car1$Price, p = 0.7, list = FALSE)
training <- car1[trainIndex,]
testing <- car1[-trainIndex,]

# Define the feature selection wrapper method
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 5)

# Train a random forest model with feature selection
set.seed(123)
rf_fit <- rfe(training[, -1], training$Price, sizes = c(1:7), rfeControl = ctrl)

# Print the feature ranking
print(rf_fit$optVariables)

# Use the selected features to train a final random forest model
final_rf <- randomForest(Price ~ ., data = training[, c("Price", rf_fit$optVariables)], importance = TRUE, ntree = 500)

# Evaluate the final model on the testing set
predictions <- predict(final_rf, testing[, c("Price", rf_fit$optVariables)])
rmse <- RMSE(predictions, testing$Price)
rsq <- R2(predictions, testing$Price)

# Print evaluation metrics
cat("RMSE: ", rmse, "\n")
cat("R-squared: ", rsq, "\n")
