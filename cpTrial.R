library(tidyverse)
library(GGally)
library(class)
defaultW <- getOption("warn") 

options(warn = -1) 


car<- read.csv("car-data.csv")
glimpse(car)
car<- car %>% mutate(Mileage = as.numeric(str_replace(Mileage," kmpl| km/kg","")),Engine = as.numeric(str_replace(Engine," CC","")),                     Power = as.numeric(str_replace(Power," bhp","")),Fuel_Type = as.factor(Fuel_Type),Owner_Type = as.factor(Owner_Type),Transmission = as.factor(Transmission))%>% select(-c(X,Name,New_Price,Location))
car$Year<-as.numeric(car$Year)
car$Kilometers_Driven<-as.numeric(car$Kilometers_Driven)
str(car)
car<- drop_na(car)

set.seed(123)
car <- car[sample(nrow(car)), ]

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
car1<- car %>% select(-c(Fuel_Type,Transmission,Owner_Type))
carnor <- as.data.frame(lapply(car1, normalize))


car_train <- carnor[1:4697,]
car_test <- carnor[4698:5872,]


q<-ggcorr(car_train, label = T)
print(q)



l2<-lm(Price~ Power+Engine,car_train)
print(summary(l2))


# view(car_test)
Price<- car_test$Price
car_test<- car_test %>% select(- Price)

pred<-predict(l2,car_test)
data<-cbind(car_test,pred,Price)
print(data)
err<-data$pred -Price

m<-mean(abs(err))
cat("\nmean Error is: ",m)









