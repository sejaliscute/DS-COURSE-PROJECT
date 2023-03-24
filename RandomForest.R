library(tidyverse)
library(GGally)
library(class)
library(caret)
car<- read.csv("car-data.csv")
str(car)
car<- car %>% mutate(Mileage = as.numeric(str_replace(Mileage," kmpl| km/kg","")),Engine = as.numeric(str_replace(Engine," CC","")),Power = as.numeric(str_replace(Power," bhp","")),Fuel_Type = as.factor(Fuel_Type),Owner_Type = as.factor(Owner_Type),Transmission = as.factor(Transmission))%>% select(-c(X,Name,New_Price,Location))
car$Year<-as.numeric(car$Year)
car$Kilometers_Driven<-as.numeric(car$Kilometers_Driven)
car$Fuel_Type<-as.numeric(car$Fuel_Type)
car$Owner_Type<-as.numeric(car$Owner_Type)
car$Transmission<-as.numeric(car$Transmission)
str(car)
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

library(randomForest)
rf <- randomForest(train_label~., data = train)
pred<-predict(rf,test)

print(rf)

d = actual-pred
mse = mean((d)^2)
mae = mean(abs(d))
rmse = sqrt(mse)
cat("\nmse= ",mse)
cat("\nmae= ",mae)
cat("\nrmse= ",rmse)
