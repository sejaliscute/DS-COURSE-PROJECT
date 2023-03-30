library(tidyverse)
library(GGally)
library(class)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)

options(warn = -1)

car<- read.csv("car-data.csv")
#str(car)
car<- car %>% mutate(Mileage = as.numeric(str_replace(Mileage," kmpl| km/kg","")),Engine = as.numeric(str_replace(Engine," CC","")),Power = as.numeric(str_replace(Power," bhp","")),Fuel_Type = as.factor(Fuel_Type),Owner_Type = as.factor(Owner_Type),Transmission = as.factor(Transmission))%>% select(-c(X,Name,New_Price,Location))
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
car<-car[sample(nrow(car)),]

n2<- function(b){
  (b-min(b))/(max(b)-min(b))
}
carn<-car[,1:9]
carnor<-as.data.frame(lapply(carn,n2))


train<-carnor[1:4697,]
test<-carnor[4698:5872,]
train_label<- car[1:4697,10]
actual<-car[4698:5872,10]
test_data<-car[4698:5872,]


mlr<-lm(Price~.,car)
s1<-summary(mlr)
print(s1)
pred1<-predict(mlr,test_data)

mae <- MAE(pred1, actual)
mse<-RMSE(pred1, actual)^2
rmse<-RMSE(pred1, actual)
R2<-R2(pred1, actual, form = "traditional")
cat("\nMultiple Linear Regression")
cat(" \nMAE:", mae, "\n", "MSE:", mse, "\n", "RMSE:", rmse, "\n", "R-squared:", R2)

dt<- rpart(train_label~., data = train, method = 'anova')
rpart.plot(dt)
pred2<- predict(dt,test)

mae1 <- MAE(pred2, actual)
mse1<-RMSE(pred2, actual)^2
rmse1<-RMSE(pred2, actual)
r2<-R2(pred2, actual, form = "traditional")

cat("\nDecision Tree")
cat("\n MAE:", mae1, "\n", "MSE:", mse1, "\n","RMSE:", rmse1, "\n", "R-squared:", r2)


rf <- randomForest(train_label~., data = train)
pred<-predict(rf,test)

print(rf)

mae2<- MAE(pred, actual)
mse2<-RMSE(pred, actual)^2
rmse2<-RMSE(pred, actual)
R22<-R2(pred, actual, form = "traditional")
cat("\nRandom Forest")
cat("\n MAE:", mae2, "\n", "MSE:", mse2, "\n", "RMSE:", rmse2, "\n", "R-squared:", R22)

final_data<- cbind(test_data,pred)
view(final_data)


