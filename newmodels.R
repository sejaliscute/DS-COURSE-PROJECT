library(xgboost)
library(tidyverse)
library(caret)            
library(GGally)
library(class)
car<- read.csv("car-data.csv")
#str(car)
#car<- car %>% mutate(Mileage = as.numeric(str_replace(Mileage," kmpl| km/kg","")),Engine = as.numeric(str_replace(Engine," CC","")),Power = as.numeric(str_replace(Power," bhp","")),Fuel_Type = as.factor(Fuel_Type),Owner_Type = as.factor(Owner_Type),Transmission = as.factor(Transmission))%>% select(-c(X,Name,New_Price,Location))
car$Mileage <- as.numeric(str_replace(car$Mileage, " kmpl| km/kg", ""))
car$Engine <- as.numeric(str_replace(car$Engine, " CC", ""))
car$Power <- str_replace(car$Power, " bhp", "")
car$Power <- as.integer(car$Power)
car$Fuel_Type <- as.factor(car$Fuel_Type)
car$Owner_Type <- as.factor(car$Owner_Type)
car$Transmission <- as.factor(car$Transmission)
car <- car[, !names(car) %in% c("X", "Name", "New_Price", "Location")]


car$Year<-as.numeric(car$Year)
car$Kilometers_Driven<-as.numeric(car$Kilometers_Driven)
car$Fuel_Type<-as.numeric(car$Fuel_Type)
car$Owner_Type<-as.numeric(car$Owner_Type)
car$Transmission<-as.numeric(car$Transmission)
#str(car)
car<- drop_na(car)

q<-ggcorr(car, label = T)
print(q)


set.seed(123)
car<-car[sample(nrow(car)),-c(3,5,9)]

n2<- function(b){
  (b-min(b))/(max(b)-min(b))
}
carn<-car[,1:6]
carnor<-as.data.frame(lapply(carn,n2))


train<-carnor[1:4697,]
test<-carnor[4698:5872,]

train_x = data.matrix(train[, -1])
train_y = train[,1]

test_x = data.matrix(test[, -1])
test_y = test[, 1]

xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

watchlist = list(train=xgb_train, test=xgb_test)

model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 500)

summary(model)

pred_y = predict(model, xgb_test)

mae1 <- MAE(pred_y, test_y)
mse1<-RMSE(pred_y, test_y)^2
rmse1<-RMSE(pred_y, test_y)
r2<-R2(pred_y, test_y, form = "traditional")

cat("\n Xgboost")
cat("\n MAE:", mae1, "\n", "MSE:", mse1, "\n","RMSE:", rmse1, "\n", "R-squared:", r2)

