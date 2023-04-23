library(tidyverse)
library(GGally)
library(class)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)



car1<- read.csv("car-data.csv")
#str(car)
car1<- car1 %>% mutate(Mileage = as.numeric(str_replace(Mileage," kmpl| km/kg","")),Engine = as.numeric(str_replace(Engine," CC","")),Power = as.numeric(str_replace(Power," bhp","")),Fuel_Type = as.factor(Fuel_Type),Owner_Type = as.factor(Owner_Type),Transmission = as.factor(Transmission))%>% select(-c(X,Name,New_Price,Location))
car1$Year<-as.numeric(car1$Year)
car1$Kilometers_Driven<-as.numeric(car1$Kilometers_Driven)
car1$Fuel_Type<-as.numeric(car1$Fuel_Type)
car1$Owner_Type<-as.numeric(car1$Owner_Type)
car1$Transmission<-as.numeric(car1$Transmission)
#str(car)
car1<- drop_na(car1)

r<-ggcorr(car1, label = T)
print(r)


ggpairs(car1, title="correlogram with ggpairs()")

set.seed(123)
car1<-car1[sample(nrow(car1)),c(4,7,8,10)]

n2<- function(b){
  (b-min(b))/(max(b)-min(b))
}
carn1<-car1[,1:3]
carnor1<-as.data.frame(lapply(carn1,n2))

train<-carnor1[1:4697,]
test<-carnor1[4698:5872,]
train_label<- car1[1:4697,4]
tl<-cbind(carnor1[1:4697,],car1[1:4697,4])
actual<-car1[4698:5872,4]
test_data<-car1[4698:5872,]

mlr<-lm(Price~.,car1)
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

svr<-svm(train_label~.,data=train)
pred3<-predict(svr,test)
summary(svr)
mae3<- MAE(pred3, actual)
mse3<-RMSE(pred3, actual)^2
rmse3<-RMSE(pred3, actual)
R23<-R2(pred3, actual, form = "traditional")
cat("\nSupport Vector Regression")
cat("\n MAE:", mae3, "\n", "MSE:", mse3, "\n", "RMSE:", rmse3, "\n", "R-squared:", R23)
