library(tidyverse)
library(GGally)
library(class)
library(caret)
library(mlr)
library(rpart)
library(randomForest)

# Load the data and preprocess it
car <- read.csv("car-data.csv")
car <- car %>% 
  mutate(Mileage = as.numeric(str_replace(Mileage," kmpl| km/kg","")), 
         Engine = as.numeric(str_replace(Engine," CC","")), 
         Power = as.numeric(str_replace(Power," bhp","")), 
         Fuel_Type = as.factor(Fuel_Type), 
         Owner_Type = as.factor(Owner_Type), 
         Transmission = as.factor(Transmission)) %>% 
  select(-c(X, Name, New_Price, Location))
car$Year <- as.numeric(car$Year)
car$Kilometers_Driven <- as.numeric(car$Kilometers_Driven)
car$Fuel_Type <- as.numeric(car$Fuel_Type)
car$Owner_Type <- as.numeric(car$Owner_Type)
car$Transmission <- as.numeric(car$Transmission)
car <- drop_na(car)

# Split the data into training and testing sets
set.seed(123)
carn <- car[,1:9]
carnor <- as.data.frame(lapply(carn, function(x) (x - min(x)) / (max(x) - min(x))))
train <- carnor[1:4697,]
test <- carnor[4698:5872,]
train_label <- car[1:4697,10]
actual <- car[4698:5872,10]

# Fit the multiple linear regression model
lm_model <- lm(train_label ~ ., data = train)
lm_pred <- predict(lm_model, test)

# Fit the decision tree model
dt_model <- rpart(train_label ~ ., data = train)
dt_pred <- predict(dt_model, test)

# Fit the random forest model
rf_model <- randomForest(train_label ~ ., data = train)
rf_pred <- predict(rf_model, test)

# Calculate the performance metrics for each model
lm_rmse <- sqrt(mean((actual - lm_pred)^2))
lm_mae <- mean(abs(actual - lm_pred))
lm_mse <- mean((actual - lm_pred)^2)
lm_r2 <- summary(lm_model)$r.squared

dt_rmse <- sqrt(mean((actual - dt_pred)^2))
dt_mae <- mean(abs(actual - dt_pred))
dt_mse <- mean((actual - dt_pred)^2)
dt_r2 <- summary(dt_model)$r.squared

rf_rmse <- sqrt(mean((actual - rf_pred)^2))
rf_mae <- mean(abs(actual - rf_pred))
rf_mse <- mean((actual - rf_pred)^2)
rf_r2 <- summary(rf_model)$r.squared

# Print the performance metrics for each model
cat("\n---Multiple Linear Regression---\n")
cat("RMSE = ", lm_rmse, "\n")
cat("MAE = ", lm_mae, "\n")
cat("MSE = ", lm_mse, "\n")
cat("R-squared = ", lm_r2, "\n")

cat("\n---Decision Tree Regression---\n")
cat("RMSE = ", dt_rmse, "\n")
cat("MAE = ", dt_mae, "\n")
cat("MSE = ", dt_mse, "\n")
cat("R-squared = ", dt_r2, "\n")

cat("\n---Random Forest Regression---\n")
cat("RMSE = ", rf_rmse, "\n")
cat("MAE = ", rf_mae, "\n")
cat("MSE = ", rf_mse, "\n")
cat("R-squared = ", rf_r2, "\n")